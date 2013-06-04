use parse::lexer;
use parse::lexer::StringReader;
use parse::token;
use ast::token_tree;

fn parse_sep_and_zerok(rdr: @mut StringReader) -> (Option<token::Token>, bool) {
    if *rdr.token == token::BINOP(token::STAR)
        || *rdr.token == token::BINOP(token::PLUS) {
        let zerok = *rdr.token == token::BINOP(token::STAR);
        rdr.bump();
        (None, zerok)
    } else {
        let sep = copy *rdr.token;
        rdr.bump();
        if *rdr.token == token::BINOP(token::STAR)
            || *rdr.token == token::BINOP(token::PLUS) {
            let zerok = *rdr.token == token::BINOP(token::STAR);
            rdr.bump();
            (Some(sep), zerok)
        } else {
            rdr.fatal("expected `*` or `+`");
        }
    }
}

// parse a single token tree from the input.
fn parse_token_tree(rdr: @mut StringReader) -> token_tree {
    // should be unnecessary now...
    //maybe_whole!(deref self, nt_tt);

    // this is the fall-through for the 'match' below.
    // invariants: the current token is not a left-delimiter,
    // not an EOF, and not the desired right-delimiter (if
    // it were, parse_seq_to_before_end would have prevented
    // reaching this point.
    fn parse_non_delim_tt_tok(p: &Parser) -> token_tree {
        // should be unnecessary now...
        //maybe_whole!(deref p, nt_tt);
        match *p.token {
            token::RPAREN | token::RBRACE | token::RBRACKET =>
            {
                p.fatal(
                    fmt!(
                        "incorrect close delimiter: `%s`",
                        p.this_token_to_str()
                    )
                );
            }
            /* we ought to allow different depths of unquotation */
            token::DOLLAR if *p.quote_depth > 0u => {
                p.bump();
                let sp = *p.span;

                if *p.token == token::LPAREN {
                    let seq = p.parse_seq(
                        &token::LPAREN,
                        &token::RPAREN,
                        seq_sep_none(),
                        |p| p.parse_token_tree()
                    );
                    let (s, z) = p.parse_sep_and_zerok();
                    let seq = match seq {
                        spanned { node, _ } => node,
                    };
                    tt_seq(
                        mk_sp(sp.lo, p.span.hi),
                        seq,
                        s,
                        z
                    )
                } else {
                    tt_nonterminal(sp, p.parse_ident())
                }
            }
            _ => {
                parse_any_tt_tok(p)
            }
        }
    }

    // turn the next token into a tt_tok:
    fn parse_any_tt_tok(p: &Parser) -> token_tree{
        let res = tt_tok(*p.span, copy *p.token);
        p.bump();
        res
    }

    match *rdr.token {
        token::EOF => {
            rdr.fatal("file ended with unbalanced delimiters");
        }
        token::LPAREN | token::LBRACE | token::LBRACKET => {
            let close_delim = token::flip_delimiter(&*rdr.token);
            tt_delim(
                vec::append(
                    // the open delimiter:
                    ~[parse_any_tt_tok(self)],
                    vec::append(
                        rdr.parse_seq_to_before_end(
                            &close_delim,
                            seq_sep_none(),
                            |p| p.parse_token_tree()
                        ),
                        // the close delimiter:
                        [parse_any_tt_tok(self)]
                    )
                )
            )
        }
        _ => parse_non_delim_tt_tok(self)
    }
}

// parse a stream of tokens into a list of token_trees,
// up to EOF.
fn parse_all_token_trees(rdr: @mut StringReader) -> ~[token_tree] {
    let mut tts = ~[];
    while *rdr.token != token::EOF {
        tts.push(parse_token_tree(rdr));
    }
    tts
}

fn parse_matchers(rdr: @mut StringReader) -> ~[matcher] {
    // unification of matchers and token_trees would vastly improve
    // the interpolation of matchers
    maybe_whole!(self, nt_matchers);
    let name_idx = @mut 0u;
    match *rdr.token {
        token::LBRACE | token::LPAREN | token::LBRACKET => {
            parse_matcher_subseq(
                rdr,
                name_idx,
                copy *rdr.token,
                // tjc: not sure why we need a copy
                token::flip_delimiter(rdr.token)
            )
        }
        _ => rdr.fatal("expected open delimiter")
    }
}


// This goofy function is necessary to correctly match parens in matchers.
// Otherwise, `$( ( )` would be a valid matcher, and `$( () )` would be
// invalid. It's similar to common::parse_seq.
fn parse_matcher_subseq(
    rdr: @mut StringReader,
    name_idx: @mut uint,
    bra: token::Token,
    ket: token::Token
) -> ~[matcher] {
    let mut ret_val = ~[];
    let mut lparens = 0u;

    // oops... got to fix this:
    expect(rdr,&bra);

    while *rdr.token != ket || lparens > 0u {
        if *rdr.token == token::LPAREN { lparens += 1u; }
        if *rdr.token == token::RPAREN { lparens -= 1u; }
        ret_val.push(parse_matcher(rdr,name_idx));
    }

    rdr.bump();

    return ret_val;
}

fn parse_matcher(rdr: @mut StringReader, name_idx: @mut uint) -> matcher {
    let lo = rdr.span.lo;

    let m = if *rdr.token == token::DOLLAR {
        rdr.bump();
        if *rdr.token == token::LPAREN {
            let name_idx_lo = *name_idx;
            let ms = parse_matcher_subseq(
                rdr,
                name_idx,
                token::LPAREN,
                token::RPAREN
            );
            if ms.len() == 0u {
                rdr.fatal("repetition body must be nonempty");
            }
            let (sep, zerok) = parse_sep_and_zerok(rdr);
            match_seq(ms, sep, zerok, name_idx_lo, *name_idx)
        } else {
            // problem here:
            let bound_to = parse_ident(rdr);
            // problem here:
            expect(rdr,&token::COLON);
            // and here:
            let nt_name = parse_ident(rdr);
            let m = match_nonterminal(bound_to, nt_name, *name_idx);
            *name_idx += 1u;
            m
        }
    } else {
        let m = match_tok(copy *rdr.token);
        rdr.bump();
        m
    };

    return spanned(lo, rdr.span.hi, m);
}

