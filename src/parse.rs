use crate::lex::{Token, TokenKind};

#[derive(Debug)]
pub enum Statement {
    Expression(Expression),
    If(If),
    FunctionDeclaration(FunctionDeclaration),
    Return(Return),
    Local(Local),
}

pub type Ast = Vec<Statement>;

#[derive(Debug)]
pub enum Literal {
    Identifier(Token),
    Number(Token),
}

#[derive(Debug)]
pub struct FunctionCall {
    pub name: Token,
    pub arguments: Vec<Expression>,
}

#[derive(Debug)]
pub struct BinaryOperation {
    pub operator: Token,
    pub left: Box<Expression>,
    pub right: Box<Expression>,
}

#[derive(Debug)]
pub enum Expression {
    FunctionCall(FunctionCall),
    BinaryOperation(BinaryOperation),
    Literal(Literal),
}

#[derive(Debug)]
pub struct FunctionDeclaration {
    pub name: Token,
    pub parameters: Vec<Token>,
    pub body: Vec<Statement>,
}

#[derive(Debug)]
pub struct If {
    pub test: Expression,
    pub body: Vec<Statement>,
}

#[derive(Debug)]
pub struct Local {
    pub name: Token,
    pub expression: Expression,
}

#[derive(Debug)]
pub struct Return {
    pub expression: Expression,
}

fn expect_keyword(tokens: &[Token], index: usize, value: &str) -> bool {
    if index >= tokens.len() {
        return false;
    }

    let t = tokens[index].clone();
    t.kind == TokenKind::Keyword && t.value == value
}

fn expect_syntax(tokens: &[Token], index: usize, value: &str) -> bool {
    if index >= tokens.len() {
        return false;
    }

    let t = tokens[index].clone();
    t.kind == TokenKind::Syntax && t.value == value
}

fn expect_identifier(tokens: &[Token], index: usize, value: &str) -> bool {
    if index >= tokens.len() {
        return false;
    }

    let t = tokens[index].clone();
    t.kind == TokenKind::Identifier && t.value == value
}

pub fn parse(_raw: &[char], _tokens: Vec<Token>) -> Result<Ast, String> {
    unimplemented!()
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::*;

    #[test]
    fn test_expect_keyword() {
        let s: Vec<_> = "if true then end;".chars().collect();
        let tokens = lex::lex(&s).unwrap();
        assert!(expect_keyword(&tokens, 0, "if"));
        assert!(expect_keyword(&tokens, 2, "then"));
    }

    #[test]
    fn test_expect_syntax() {
        let s: Vec<_> = "local a = 1;".chars().collect();
        let tokens = lex::lex(&s).unwrap();
        assert!(expect_syntax(&tokens, 2, "="));
        assert!(expect_syntax(&tokens, 4, ";"));
    }

    #[test]
    fn test_expect_identifier() {
        let s: Vec<_> = "local a = 1;".chars().collect();
        let tokens = lex::lex(&s).unwrap();
        assert!(expect_identifier(&tokens, 1, "a"));
    }

    #[test]
    fn test_parse() {
        let s: Vec<_> = "function myFunc end;".chars().collect();
        let tokens = lex::lex(&s).unwrap();

        let result = parse(&[], tokens);
        assert!(result.is_ok());
    }
}
