// Copyright 2012 The Rust Project Developers. See the COPYRIGHT
// file at the top-level directory of this distribution and at
// http://rust-lang.org/COPYRIGHT.
//
// Licensed under the Apache License, Version 2.0 <LICENSE-APACHE or
// http://www.apache.org/licenses/LICENSE-2.0> or the MIT license
// <LICENSE-MIT or http://opensource.org/licenses/MIT>, at your
// option. This file may not be copied, modified, or distributed
// except according to those terms.

//extern mod syntax;
use core::prelude::*;

//use syntax::parse::{lexer, token};
//use syntax::diagnostic;
//use syntax::codemap;
use parse::lexer::*;
use parse::token::*;
use diagnostic::*;
use codemap::*;

// the SpliceReader has a stack of
// TokenTrees to be used before getting
// to the underlying token stream.
pub struct SpliceReader {
    subreader : @StringReader,
    // I'm guessing that dvec is the right
    // thing to use for a stack?
    //preTrees: ~DVec<~TokenTree>
}

pub impl SpliceReader : reader {
    fn is_eof(&self) -> bool {
        self.subreader.is_eof()
    }
    fn next_token(&self) -> TokenAndSpan {
        // deviously replace identifiers with "boogaloo"
        match self.subreader.next_token() {
            TokenAndSpan{tok: token::LIT_STR(id), sp: sp} =>
            TokenAndSpan{tok: token::LIT_STR(self.interner().intern(@~"boogaloo")),
                         sp: sp},
            any => any
        }
    }
    fn fatal(&self, ++m: ~str) -> ! {
        self.subreader.fatal(m);
    }
    fn span_diag(&self) -> span_handler {
        self.subreader.span_diag()
    }
    pure fn interner(&self) -> @token::ident_interner {
        self.subreader.interner()
    }
    fn peek(&self) -> TokenAndSpan {
        match self.subreader.peek() {
            TokenAndSpan{tok: token::LIT_STR(id), sp: sp} =>
            TokenAndSpan{tok: token::LIT_STR(self.interner().intern(@~"boogaloo")),
                         sp: sp},
            any => any
        } 
    }
    // this just *can't* be a good idea...
    fn dup(&self) -> reader {
        self.subreader.dup()
    }
}

/*pub impl SpliceReader {
    fn push_tree(&self, tt: ~TokenTree)    
}*/

pub fn new_splicereader (span_diagnostic: span_handler,
                         filemap: @codemap::FileMap,
                         itr: @token::ident_interner) -> @SpliceReader{
    @SpliceReader{subreader: new_string_reader(span_diagnostic, filemap, itr)}
}

#[cfg(test)]
pub mod test {

    use super::*;
    use util::interner;
    use diagnostic;
    use codemap::*;
    use parse::lexer;
    use parse::token;
    use util::testing::{check_equal, check_equal_ptr};
    #[test] fn t1 () {
        let teststr =
            @~"z\"abc\"tr ";
        let cm = CodeMap::new();
        let fm = cm.new_filemap(~"zebra.rs",teststr);
        let ident_interner = token::mk_ident_interner(); // interner::mk();
        let id = ident_interner.intern(@~"z");
        let span_handler =
            mk_span_handler(mk_handler(None),@cm);
        let string_reader = new_splicereader(span_handler,fm,ident_interner);
        let tok1 = string_reader.next_token();
        let tok2 = TokenAndSpan{
            tok:token::IDENT(id, false),
            sp: span {lo:BytePos(0),hi:BytePos(1),expn_info: None}};
        check_equal (tok1,tok2);
        let tok3 = string_reader.next_token();
        let tok4 = TokenAndSpan{
            tok:token::LIT_STR(ident_interner.intern (@~"boogaloo")),
            sp:span {lo:BytePos(1),hi:BytePos(6),expn_info: None}};
        check_equal (tok3,tok4);
    }
}
