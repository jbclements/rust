// error-pattern:expected `(` but found `{`

// should see six errors, ending with missing args to main


mod z {

    fn main () {
        #[attribute_not_allowed] // in stmt posn
        let x = 3;
        #[another_illegal_attribute] // in stmt posn
        let y = x;
        #[yet_another_bad_attribute] // at block tail
    }

    #[fourth_bad_attribute] // at module tail
}

extern mod c {
    #[fifth_bad_attribute] // in extern mods
}

// finally, a syntax error (forgot args)
fn main {
    3;
}
