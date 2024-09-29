use std::fmt::format;

#[derive(Clone, Copy, Debug)]
pub struct Location {
    col: i32,
    line: i32,
    index: usize,
}

impl Location {
    fn increment(&self, newline: bool) -> Location {
        if newline {
            Location {
                index: self.index + 1,
                col: 0,
                line: self.line + 1,
            }
        } else {
            Location {
                index: self.index + 1,
                col: self.col + 1,
                line: self.line,
            }
        }
    }

    pub fn debug<S: Into<String>>(&self, raw: &[char], msg: S) -> String {
        let mut line = 0;
        let mut line_str = String::new();

        for c in raw {
            if *c == '\n' {
                line += 1;

                if !line_str.is_empty() {
                    break;
                }

                continue;
            }

            if self.line == line {
                line_str.push_str(&c.to_string());
            }
        }

        let space = " ".repeat(self.col as usize);
        format!("{}\n\n{}\n{}^ Near here", msg.into(), line_str, space)
    }
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum TokenKind {
    Indentifier,
    Syntax,
    Keyword,
    Number,
    Operator,
}

#[derive(Debug, Clone)]
pub struct Token {
    pub value: String,
    pub kind: TokenKind,
    pub loc: Location,
}

fn eat_whitespace(raw: &[char], initial_loc: Location) -> Location {
    let mut c = raw[initial_loc.index];
    let mut next_loc = initial_loc;
    while [' ', '\n', '\r', '\t'].contains(&c) {
        next_loc = next_loc.increment(c == '\n');
        if next_loc.index == raw.len() {
            break;
        }

        c = raw[next_loc.index];
    }

    next_loc
}

fn lex_number(raw: &[char], initial_loc: Location) -> Option<(Token, Location)> {
    let mut ident = String::new();
    let mut next_loc = initial_loc;
    let mut c = raw[initial_loc.index];
    while c.is_digit(10) {
        ident.push_str(&c.to_string());
        next_loc = next_loc.increment(false);
        c = raw[next_loc.index];
    }

    if !ident.is_empty() {
        Some((
            Token {
                value: ident,
                loc: initial_loc,
                kind: TokenKind::Number,
            },
            next_loc,
        ))
    } else {
        None
    }
}

pub fn lex(s: &[char]) -> Result<Vec<Token>, String> {
    let mut loc = Location {
        col: 0,
        index: 0,
        line: 0,
    };
    let size = s.len();
    let mut tokens: Vec<Token> = Vec::new();

    let lexers = [
        lex_keyword,
        lex_indentifier,
        lex_number,
        lex_syntax,
        lex_operator,
    ];

    'outer: while loc.index < size {
        loc = eat_whitespace(s, loc);
        if loc.index == size {
            break;
        }

        for lexer in lexers {
            let res = lexer(s, loc);
            if let Some((t, next_loc)) = res {
                loc = next_loc;
                tokens.push(t);
                continue 'outer;
            }
        }

        return Err(loc.debug(s, "Unrecognized character while lexing:"));
    }

    unimplemented!()
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_location_increment() {
        let loc = Location {
            col: 5,
            line: 3,
            index: 10,
        };

        // Test incrementing without a newline
        let new_loc = loc.increment(false);
        assert_eq!(new_loc.col, 6);
        assert_eq!(new_loc.line, 3);
        assert_eq!(new_loc.index, 11);

        // Test incrementing with a newline
        let new_loc = loc.increment(true);
        assert_eq!(new_loc.col, 0);
        assert_eq!(new_loc.line, 4);
        assert_eq!(new_loc.index, 11);
    }
}
