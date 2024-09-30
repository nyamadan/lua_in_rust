#[derive(Debug, Clone, Copy, PartialEq, Eq)]
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

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum TokenKind {
    Identifier,
    Syntax,
    Keyword,
    Number,
    Operator,
}

#[derive(Debug, Clone, PartialEq, Eq)]
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

fn lex_identifier(raw: &[char], initial_loc: Location) -> Option<(Token, Location)> {
    let mut ident = String::new();
    let mut next_loc = initial_loc;
    let mut c = raw[initial_loc.index];
    while c.is_alphanumeric() || c == '_' {
        ident.push_str(&c.to_string());
        next_loc = next_loc.increment(false);
        c = raw[next_loc.index];
    }

    if ident.len() > 0 && !ident.chars().next().unwrap().is_digit(10) {
        Some((
            Token {
                value: ident,
                loc: initial_loc,
                kind: TokenKind::Identifier,
            },
            next_loc,
        ))
    } else {
        None
    }
}

fn lex_keyword(raw: &[char], initial_loc: Location) -> Option<(Token, Location)> {
    let syntax = ["function", "end", "if", "then", "local", "return"];

    let mut next_loc = initial_loc;
    let mut value = String::new();

    'outer: for possible_syntax in syntax {
        let mut c = raw[initial_loc.index];
        next_loc = initial_loc;
        while c.is_alphanumeric() || c == '_' {
            value.push_str(&c.to_string());
            next_loc = next_loc.increment(false);
            c = raw[next_loc.index];

            let n = next_loc.index - initial_loc.index;
            if value != possible_syntax[..n] {
                value = String::new();
                continue 'outer;
            }
        }

        if value.len() < possible_syntax.len() {
            value = String::new();
        }

        break;
    }

    if value.is_empty() {
        return None;
    }

    if next_loc.index < raw.len() - 1 {
        let next_c = raw[next_loc.index];
        if next_c.is_alphanumeric() || next_c == '_' {
            return None;
        }
    }

    Some((
        Token {
            value: value,
            loc: initial_loc,
            kind: TokenKind::Keyword,
        },
        next_loc,
    ))
}

fn lex_syntax(raw: &[char], initial_loc: Location) -> Option<(Token, Location)> {
    let syntax = [";", "=", "(", ")", ","];

    for possible_syntax in syntax {
        let c = raw[initial_loc.index];
        let next_loc = initial_loc.increment(false);

        // TODO: this won't with multiple-character operations like >= or ==
        if possible_syntax == c.to_string() {
            return Some((
                Token {
                    value: possible_syntax.to_string(),
                    loc: initial_loc,
                    kind: TokenKind::Syntax,
                },
                next_loc,
            ));
        }
    }

    None
}

fn lex_operator(raw: &[char], initial_loc: Location) -> Option<(Token, Location)> {
    let operators = ["+", "-", "<"];

    for possible_syntax in operators {
        let c = raw[initial_loc.index];
        let next_loc = initial_loc.increment(false);

        // TODO: this won't with multiple-character operations like >= or ==
        if possible_syntax == c.to_string() {
            return Some((
                Token {
                    value: possible_syntax.to_string(),
                    loc: initial_loc,
                    kind: TokenKind::Operator,
                },
                next_loc,
            ));
        }
    }

    None
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
        lex_identifier,
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

    Ok(tokens)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_eat_whitespace() {
        let input: Vec<char> = "   \n\t  x".chars().collect();
        let initial_loc = Location {
            col: 0,
            line: 0,
            index: 0,
        };
        let new_loc = eat_whitespace(&input, initial_loc);

        assert_eq!(new_loc.col, 3);
        assert_eq!(new_loc.line, 1);
        assert_eq!(new_loc.index, 7);
    }

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

    #[test]
    fn test_lex_function() {
        let input = "local x = 1 + 2;";
        let raw: Vec<_> = input.chars().collect();
        let tokens = lex(&raw).expect("Failed to parse");
        let expected = vec![
            Token {
                value: String::from("local"),
                kind: TokenKind::Keyword,
                loc: Location {
                    col: 0,
                    line: 0,
                    index: 0,
                },
            },
            Token {
                value: String::from("x"),
                kind: TokenKind::Identifier,
                loc: Location {
                    col: 6,
                    line: 0,
                    index: 6,
                },
            },
            Token {
                value: String::from("="),
                kind: TokenKind::Syntax,
                loc: Location {
                    col: 8,
                    line: 0,
                    index: 8,
                },
            },
            Token {
                value: String::from("1"),
                kind: TokenKind::Number,
                loc: Location {
                    col: 10,
                    line: 0,
                    index: 10,
                },
            },
            Token {
                value: String::from("+"),
                kind: TokenKind::Operator,
                loc: Location {
                    col: 12,
                    line: 0,
                    index: 12,
                },
            },
            Token {
                value: String::from("2"),
                kind: TokenKind::Number,
                loc: Location {
                    col: 14,
                    line: 0,
                    index: 14,
                },
            },
            Token {
                value: String::from(";"),
                kind: TokenKind::Syntax,
                loc: Location {
                    col: 15,
                    line: 0,
                    index: 15,
                },
            },
        ];

        assert_eq!(tokens, expected);
    }
}
