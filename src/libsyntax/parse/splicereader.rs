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
use parse::{lexer, token};
use diagnostic;
use codemap;

pub struct SpliceReader {
    subreader : @lexer::StringReader,
}

pub impl SpliceReader : lexer::reader {
    fn is_eof(&self) -> bool {
        self.subreader.is_eof()
    }
    fn next_token(&self) -> lexer::TokenAndSpan {
        // deviously replace identifiers with "boogaloo"
        match self.subreader.next_token() {
            lexer::TokenAndSpan{tok: token::LIT_STR(id), sp: sp} =>
            lexer::TokenAndSpan{tok: token::LIT_STR(self.interner().intern(@~"boogaloo")),
                         sp: sp},
            any => any
        }
    }
    fn fatal(&self, ++m: ~str) -> ! {
        self.subreader.fatal(m);
    }
    fn span_diag(&self) -> diagnostic::span_handler {
        self.subreader.span_diag()
    }
    pure fn interner(&self) -> @token::ident_interner {
        self.subreader.interner()
    }
    fn peek(&self) -> lexer::TokenAndSpan {
        match self.subreader.peek() {
            lexer::TokenAndSpan{tok: token::LIT_STR(id), sp: sp} =>
            lexer::TokenAndSpan{tok: token::LIT_STR(self.interner().intern(@~"boogaloo")),
                         sp: sp},
            any => any
        } 
    }
    // this just *can't* be a good idea...
    fn dup(&self) -> lexer::reader {
        self.subreader.dup()
    }
}

pub fn new_splicereader (span_diagnostic: diagnostic::span_handler,
                         filemap: @codemap::FileMap,
                         itr: @token::ident_interner) -> @SpliceReader{
    @SpliceReader{subreader: lexer::new_string_reader(span_diagnostic, filemap, itr)}
}


