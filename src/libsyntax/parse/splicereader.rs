// Copyright 2012 The Rust Project Developers. See the COPYRIGHT
// file at the top-level directory of this distribution and at
// http://rust-lang.org/COPYRIGHT.
//
// Licensed under the Apache License, Version 2.0 <LICENSE-APACHE or
// http://www.apache.org/licenses/LICENSE-2.0> or the MIT license
// <LICENSE-MIT or http://opensource.org/licenses/MIT>, at your
// option. This file may not be copied, modified, or distributed
// except according to those terms.

// developing "outside" for now.
extern mod syntax;
//use core::prelude::*;

use syntax::parse::{lexer, token};
use syntax::diagnostic;

pub struct SpliceReader {
    subreader : lexer::string_reader,
}

pub impl SpliceReader : lexer::reader {
    fn is_eof(&self) -> bool {
        self.subreader.is_eof()
    }
    fn next_token(&self) -> lexer::TokenAndSpan {
        // deviously replace identifiers with "boogaloo"
        match self.subreader.next_token() {
            TokenAndSpan{tok: IDENT(id,_), sp: sp} =>
            TokenAndSpan{tok: IDENT(self.interner.intern("boogaloo"),
                                    sp: sp}
        }
    }
    fn fatal(&self, err: ~str) -> ! {
        self.subreader.fatal(err)
    }
    fn span_diag(&self) -> diagnostic::span_handler {
        self.subreader.span_diag()
    }
    pure fn interner(&self) -> @token::ident_interner {
        self.subreader.interner()
    }
    fn peek(&self) -> lexer::TokenAndSpan {
        self.subreader.peek()
    }
    // this just *can't* be a good idea...
    fn dup(&self) -> lexer::reader {
        self.subreader.peek()
    }
}


