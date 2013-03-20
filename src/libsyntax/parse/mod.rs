// Copyright 2012 The Rust Project Developers. See the COPYRIGHT
// file at the top-level directory of this distribution and at
// http://rust-lang.org/COPYRIGHT.
//
// Licensed under the Apache License, Version 2.0 <LICENSE-APACHE or
// http://www.apache.org/licenses/LICENSE-2.0> or the MIT license
// <LICENSE-MIT or http://opensource.org/licenses/MIT>, at your
// option. This file may not be copied, modified, or distributed
// except according to those terms.

//! The main parser interface


use ast::node_id;
use ast;
use codemap::{span, CodeMap, FileMap, CharPos, BytePos};
use codemap;
use diagnostic::{span_handler, mk_span_handler, mk_handler, Emitter};
use parse::attr::parser_attr;
use parse::lexer::{reader, StringReader};
use parse::parser::Parser;
use parse::token::{ident_interner, mk_ident_interner};

use core::io;
use core::option::{None, Option, Some};
use core::path::Path;
use core::result::{Err, Ok, Result};

pub mod lexer;
pub mod parser;
pub mod token;
pub mod comments;
pub mod attr;


/// Common routines shared by parser mods
pub mod common;

/// Functions dealing with operator precedence
pub mod prec;

/// Routines the parser uses to classify AST nodes
pub mod classify;

/// Reporting obsolete syntax
pub mod obsolete;

// info about a parsing session.
// This structure and the reader both have
// an interner associated with them. If they're
// not the same, bad things can happen.
pub struct ParseSess {
    cm: @codemap::CodeMap, // better be the same as the one in the reader!
    next_id: node_id,
    span_diagnostic: @span_handler, // better be the same as the one in the reader!
    interner: @ident_interner,
}

pub fn new_parse_sess(demitter: Option<Emitter>) -> @mut ParseSess {
    let cm = @CodeMap::new();
    @mut ParseSess {
        cm: cm,
        next_id: 1,
        span_diagnostic: mk_span_handler(mk_handler(demitter), cm),
        interner: mk_ident_interner(),
    }
}

pub fn new_parse_sess_special_handler(sh: @span_handler,
                                      cm: @codemap::CodeMap)
                                   -> @mut ParseSess {
    @mut ParseSess {
        cm: cm,
        next_id: 1,
        span_diagnostic: sh,
        interner: mk_ident_interner(),
    }
}

// a bunch of utility functions of the form parse_<thing>_from_<source>
// where <thing> includes crate, expr, item, stmt, tts, and one that
// uses a HOF to parse anything, and <source> includes file and
// source_str.

// this appears to be the main entry point for rust parsing by
// rustc and crate:
pub fn parse_crate_from_file(
    input: &Path,
    cfg: ast::crate_cfg,
    sess: @mut ParseSess
) -> @ast::crate {
    let p = new_parser_from_file(sess, /*bad*/ copy cfg, input);
    p.parse_crate_mod(/*bad*/ copy cfg)
    // why is there no p.abort_if_errors here?
}

pub fn parse_crate_from_file_using_tts(
    input: &Path,
    cfg: ast::crate_cfg,
    sess: @mut ParseSess
) -> @ast::crate {
    let p = new_parser_from_file(sess, /*bad*/ copy cfg, input);
    let tts = p.parse_all_token_trees();
    new_parser_from_tts(sess,cfg,tts).parse_crate_mod(/*bad*/ copy cfg)
    // why is there no p.abort_if_errors here?
}



pub fn parse_crate_from_source_str(
    name: ~str,
    source: @~str,
    cfg: ast::crate_cfg,
    sess: @mut ParseSess
) -> @ast::crate {
    let p = new_parser_from_source_str(
        sess,
        /*bad*/ copy cfg,
        /*bad*/ copy name,
        codemap::FssNone,
        source
    );
    maybe_aborted(p.parse_crate_mod(/*bad*/ copy cfg),p)
}

pub fn parse_expr_from_source_str(
    name: ~str,
    source: @~str,
    +cfg: ast::crate_cfg,
    sess: @mut ParseSess
) -> @ast::expr {
    let p = new_parser_from_source_str(
        sess,
        cfg,
        /*bad*/ copy name,
        codemap::FssNone,
        source
    );
    maybe_aborted(p.parse_expr(), p)
}

pub fn parse_item_from_source_str(
    name: ~str,
    source: @~str,
    +cfg: ast::crate_cfg,
    +attrs: ~[ast::attribute],
    sess: @mut ParseSess
) -> Option<@ast::item> {
    let p = new_parser_from_source_str(
        sess,
        cfg,
        /*bad*/ copy name,
        codemap::FssNone,
        source
    );
    maybe_aborted(p.parse_item(attrs),p)
}

pub fn parse_meta_from_source_str(
    name: ~str,
    source: @~str,
    +cfg: ast::crate_cfg,
    sess: @mut ParseSess
) -> @ast::meta_item {
    let p = new_parser_from_source_str(
        sess,
        cfg,
        /*bad*/ copy name,
        codemap::FssNone,
        source
    );
    maybe_aborted(p.parse_meta_item(),p)
}

pub fn parse_stmt_from_source_str(
    name: ~str,
    source: @~str,
    +cfg: ast::crate_cfg,
    +attrs: ~[ast::attribute],
    sess: @mut ParseSess
) -> @ast::stmt {
    let p = new_parser_from_source_str(
        sess,
        cfg,
        /*bad*/ copy name,
        codemap::FssNone,
        source
    );
    maybe_aborted(p.parse_stmt(attrs),p)
}

pub fn parse_tts_from_source_str(
    name: ~str,
    source: @~str,
    +cfg: ast::crate_cfg,
    sess: @mut ParseSess
) -> ~[ast::token_tree] {
    let p = new_parser_from_source_str(
        sess,
        cfg,
        /*bad*/ copy name,
        codemap::FssNone,
        source
    );
    *p.quote_depth += 1u;
    maybe_aborted(p.parse_all_token_trees(),p)
}

// given a function and parsing information (source str,
// filename, crate cfg, and sess), create a parser,
// apply the function, and check that the parser
// consumed all of the input before returning the function's
// result.
pub fn parse_from_source_str<T>(
    f: &fn (Parser) -> T,
    name: ~str, ss: codemap::FileSubstr,
    source: @~str,
    +cfg: ast::crate_cfg,
    sess: @mut ParseSess
) -> T {
    let p = new_parser_from_source_str(
        sess,
        cfg,
        /*bad*/ copy name,
        /*bad*/ copy ss,
        source
    );
    let r = f(p);
    if !p.reader.is_eof() {
        p.reader.fatal(~"expected end-of-string");
    }
    maybe_aborted(r,p)
}

// return the next unused node id.
pub fn next_node_id(sess: @mut ParseSess) -> node_id {
    let rv = sess.next_id;
    sess.next_id += 1;
    // ID 0 is reserved for the crate and doesn't actually exist in the AST
    fail_unless!(rv != 0);
    return rv;
}

pub fn new_parser_from_source_str(sess: @mut ParseSess,
                                  +cfg: ast::crate_cfg,
                                  +name: ~str,
                                  +ss: codemap::FileSubstr,
                                  source: @~str)
                               -> Parser {
    let filemap = sess.cm.new_filemap_w_substr(name, ss, source);
    let srdr = lexer::new_string_reader(
        copy sess.span_diagnostic,
        filemap,
        sess.interner
    );
    Parser(sess, cfg, srdr as @reader)
}

/// Read the entire source file, return a parser
/// that draws from that string
pub fn new_parser_result_from_file(
    sess: @mut ParseSess,
    +cfg: ast::crate_cfg,
    path: &Path
) -> Result<Parser, ~str> {
    match io::read_whole_file_str(path) {
        Ok(src) => {
            let filemap = sess.cm.new_filemap(path.to_str(), @src);
            let srdr = lexer::new_string_reader(copy sess.span_diagnostic,
                                                filemap,
                                                sess.interner);
            Ok(Parser(sess, cfg, srdr as @reader))

        }
        Err(e) => Err(e)
    }
}

/// Create a new parser, handling errors as appropriate
/// if the file doesn't exist
pub fn new_parser_from_file(
    sess: @mut ParseSess,
    +cfg: ast::crate_cfg,
    path: &Path
) -> Parser {
    match new_parser_result_from_file(sess, cfg, path) {
        Ok(parser) => parser,
        Err(e) => {
            sess.span_diagnostic.handler().fatal(e)
        }
    }
}

/// Create a new parser based on a span from an existing parser. Handles
/// error messages correctly when the file does not exist.
pub fn new_sub_parser_from_file(
    sess: @mut ParseSess,
    +cfg: ast::crate_cfg,
    path: &Path,
    sp: span
) -> Parser {
    match new_parser_result_from_file(sess, cfg, path) {
        Ok(parser) => parser,
        Err(e) => {
            sess.span_diagnostic.span_fatal(sp, e)
        }
    }
}

pub fn new_parser_from_tts(
    sess: @mut ParseSess,
    +cfg: ast::crate_cfg,
    +tts: ~[ast::token_tree]
) -> Parser {
    let trdr = lexer::new_tt_reader(
        copy sess.span_diagnostic,
        sess.interner,
        None,
        tts
    );
    Parser(sess, cfg, trdr as @reader)
}

// abort if necessary
pub fn maybe_aborted<T>(+result : T, p: Parser) -> T {
    p.abort_if_errors();
    result
}



#[cfg(test)]
mod test {
    use super::*;
    use std::serialize::Encodable;
    use std;
    use core::io;
    use core::option::Option;
    use core::option::Some;
    use core::option::None;
    use core::int;
    use core::num::NumCast;
    use codemap::{dummy_sp, CodeMap, span, BytePos};
    use ast;
    use parse::parser::Parser;
    use parse::token::{ident_interner, mk_ident_interner, mk_fresh_ident_interner};
    use diagnostic::{span_handler, mk_span_handler, mk_handler, Emitter};

    // add known names to interner for testing
    fn mk_testing_interner() -> @ident_interner {
        let i = mk_fresh_ident_interner();
        // baby hack; in order to put the identifiers
        // 'a' and 'b' at known locations, we're going
        // to fill up the interner to length 100. If
        // the # of preloaded items on the interner
        // ever gets larger than 100, we'll have to
        // adjust this number (say, to 200) and
        // change the numbers in the identifier
        // test cases below.

        fail_unless!(i.len() < 100);
        for int::range(0,100-((i.len()).to_int())) |_dc| {
            i.gensym(@~"dontcare");
        }
        i.intern(@~"a");
        i.intern(@~"b");
        i.intern(@~"c");
        i.intern(@~"d");
        i.intern(@~"return");
        fail_unless!(i.get(ast::ident{repr:101}) == @~"b");
        i
    }

    // make a parse_sess that's closed over a
    // testing interner (where a -> 100, b -> 101)
    fn mk_testing_parse_sess() -> @mut ParseSess {
        let interner = mk_testing_interner();
        let cm = @CodeMap::new();
        @mut ParseSess {
            cm: cm,
            next_id: 1,
            span_diagnostic: mk_span_handler(mk_handler(None), cm),
            interner: interner,
        }
    }

    // map a string to tts, using a made-up filename
    fn string_to_tts (source_str : @~str) -> ~[ast::token_tree] {
        parse_tts_from_source_str(
            ~"bogofile",
            source_str,
            ~[],
            mk_testing_parse_sess())
    }

    // map tts to a parser
    fn tts_to_parser(tts : ~[ast::token_tree]) -> @Parser {
        let s = mk_testing_parse_sess();
        @new_parser_from_tts(s,~[],tts)
    }

    #[test] fn to_json_str<E : Encodable<std::json::Encoder>>(val: @E) -> ~str {
        do io::with_str_writer |writer| {
            val.encode(~std::json::Encoder(writer));
        }
    }

    fn string_to_crate (source_str : @~str) -> @ast::crate {
        tts_to_parser(string_to_tts(source_str)).parse_crate_mod(~[])
    }

    fn string_to_expr (source_str : @~str) -> @ast::expr {
        tts_to_parser(string_to_tts(source_str)).parse_expr()
    }

    fn string_to_item (source_str : @~str) -> Option<@ast::item> {
        tts_to_parser(string_to_tts(source_str)).parse_item(~[])
    }

    // produce a codemap::span
    fn sp (a: uint, b: uint) -> span {
        span{lo:BytePos(a),hi:BytePos(b),expn_info:None}
    }

    // convert a vector of uints to a vector of ast::idents
    fn ints_to_idents(ids: ~[uint]) -> ~[ast::ident] {
        ids.map(|u| ast::ident{repr:*u})
    }

    // test helper to simplify producing PATH tokens
    fn testpath(lo: uint, hi: uint, ids: ~[uint], is_global: bool)
        -> ast::token_tree {
        ast::tt_tok(sp(lo,hi),token::PATH(ints_to_idents(ids),is_global))
    }
    
    #[test] fn tt_paths_1 () {
        let return_id = 104;
        assert_eq! (string_to_tts(@~"a"),
                     ~[testpath(0,1,~[100],false)]);
        assert_eq! (string_to_tts(@~" a b"),
                     ~[testpath (1,2,~[100],false),
                       testpath (3,4,~[101],false)]);
        assert_eq! (string_to_tts(@~"a::b"),
                     ~[testpath (0,4,~[100,101],false)]);
        assert_eq! (string_to_tts(@~"::a::b"),
                     ~[testpath(0,6,~[100,101],true)]);
        assert_eq! (string_to_tts(@~"a:: b"),
                     ~[testpath(0,5,~[100,101],false)]);
        assert_eq! (string_to_tts(@~"a :: b"),
                     ~[testpath(0,6,~[100,101],false)]);
        assert_eq! (string_to_tts(@~"::a:: c"),
                     ~[testpath(0,7,~[100,102],true)]);
        assert_eq! (string_to_tts(@~"return:: c"),
                     ~[testpath(0,10,~[return_id,102],false)]);
        assert_eq! (string_to_tts(@~"return ::c"),
                     ~[testpath(0,6,~[return_id],false),
                       testpath(7,10,~[102],true)]);
    }

    #[test] fn aaa_path_exprs () {
        assert_eq!(string_to_expr(@~"a"),
                   @ast::expr{id:1,
                              callee_id:2,
                              node:ast::expr_path(@ast::Path {span:sp(0,1),
                                                              global:false,
                                                              idents:@ints_to_idents(~[100]),
                                                              rp:None,
                                                              types:~[],
                                                               ctxt:@ast::MT}),
                              span:sp(0,1)})
    }
    
    #[test] fn aab_path_exprs () {
        assert_eq!(string_to_expr(@~"::a::b"),
                   @ast::expr{id:1,
                               callee_id:2,
                               node:ast::expr_path(@ast::Path {span:sp(0,6),
                                                               global:true,
                                                               idents:@ints_to_idents(~[100,101]),
                                                               rp:None,
                                                               types:~[],
                                                               ctxt:@ast::MT}),
                              span:sp(0,6)})
    }
    
    #[should_fail]
    #[test] fn bad_path_expr_1() {
        string_to_expr(@~"::abc::def::return");
    }

    #[test] fn tt_paths_2 () {
        // can this occur? not sure:
        /*assert_eq!(string_to_tts(@~"::{"),
                    ~[ast::tt_tok(sp(0,2),token::MOD_SEP),
                      ast::tt_tok(sp(2,3),token::LBRACE)]);
        assert_eq!(string_to_tts(@~"::<"),
                    ~[ast::tt_tok(sp(0,2),token::MOD_SEP),
                      ast::tt_tok(sp(2,3),token::LT)]);*/
        assert_eq!(string_to_tts(@~"b::c::<"),
                    ~[testpath(0,4,~[101,102],false),
                      ast::tt_tok(sp(4,6),token::MOD_SEP),
                      ast::tt_tok(sp(6,7),token::LT)]);
    }


    // check the contents of the tt manually:
    #[test] fn alltts () {
        let source_str = @~"fn foo (x : int) { x; }";
        let tts = parse_tts_from_source_str(
            ~"bogofile",
            source_str,
            ~[],
            new_parse_sess(None));
        assert_eq!(to_json_str(@tts),
                    ~"[[\"tt_tok\",[null,[\"PATH\",[[\"fn\"],false]]]],\
                      [\"tt_tok\",[null,[\"PATH\",[[\"foo\"],false]]]],\
                      [\"tt_delim\",[[[\"tt_tok\",[null,[\"LPAREN\",[]]]],\
                      [\"tt_tok\",[null,[\"PATH\",[[\"x\"],false]]]],\
                      [\"tt_tok\",[null,[\"COLON\",[]]]],\
                      [\"tt_tok\",[null,[\"PATH\",[[\"int\"],false]]]],\
                      [\"tt_tok\",[null,[\"RPAREN\",[]]]]]]],\
                      [\"tt_delim\",[[[\"tt_tok\",[null,[\"LBRACE\",[]]]],\
                      [\"tt_tok\",[null,[\"PATH\",[[\"x\"],false]]]],\
                      [\"tt_tok\",[null,[\"SEMI\",[]]]],\
                      [\"tt_tok\",[null,[\"RBRACE\",[]]]]]]]]"
                   );
        let ast1 = new_parser_from_tts(new_parse_sess(None),~[],tts)
            .parse_item(~[]);
        let ast2 = parse_item_from_source_str(
            ~"bogofile",
            @~"fn foo (x : int) { x; }",
            ~[],~[],
            new_parse_sess(None));
        assert_eq!(ast1,ast2);
    }
}

//
// Local Variables:
// mode: rust
// fill-column: 78;
// indent-tabs-mode: nil
// c-basic-offset: 4
// buffer-file-coding-system: utf-8-unix
// End:
//
