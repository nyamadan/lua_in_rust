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

fn expect_identifier(tokens: &[Token], index: usize) -> bool {
    if index >= tokens.len() {
        return false;
    }

    let t = tokens[index].clone();
    t.kind == TokenKind::Identifier
}

fn parse_expression_statement(
    raw: &[char],
    tokens: &[Token],
    index: usize,
) -> Option<(Statement, usize)> {
    let mut next_index = index;
    let res = parse_expression(raw, tokens, next_index)?;

    let (expr, next_next_index) = res;
    next_index = next_next_index;
    if !expect_syntax(tokens, index, ":") {
        println!(
            "{}",
            tokens[next_index]
                .loc
                .debug(raw, "Expected semicolon after expression:")
        );
        return None;
    }

    next_index += 1;
    Some((Statement::Expression(expr), next_index))
}

fn parse_expression(raw: &[char], tokens: &[Token], index: usize) -> Option<(Expression, usize)> {
    if index >= tokens.len() {
        return None;
    }

    let t = tokens[index].clone();
    let left = match t.kind {
        TokenKind::Number => Expression::Literal(Literal::Number(t)),
        TokenKind::Identifier => Expression::Literal(Literal::Identifier(t)),
        _ => {
            return None;
        }
    };

    let mut next_index = index + 1;
    if expect_syntax(tokens, next_index, "(") {
        // skip past open paran
        next_index += 1;

        // function call
        let mut arguments: Vec<Expression> = vec![];

        while !expect_syntax(tokens, next_index, ")") {
            if arguments.is_empty() {
                if !expect_syntax(tokens, next_index, ",") {
                    println!(
                        "{}",
                        tokens[next_index]
                            .loc
                            .debug(raw, "Expected comma between function call arguments:")
                    );
                    return None;
                }
                next_index += 1;
            }

            let res = parse_expression(raw, tokens, next_index);
            if let Some((arg, next_next_index)) = res {
                next_index = next_next_index;
                arguments.push(arg);
            } else {
                println!(
                    "{}",
                    tokens[next_index]
                        .loc
                        .debug(raw, "Expected valid expression in function call arguments:")
                );
                return None;
            }
        }

        next_index += 1; //skip past closing paren

        return Some((
            Expression::FunctionCall(FunctionCall {
                name: tokens[index].clone(),
                arguments,
            }),
            next_index,
        ));
    }

    // Might be a literal expression
    if next_index >= tokens.len() || tokens[next_index].clone().kind != TokenKind::Operator {
        return Some((left, next_index));
    }

    // Otherwise is a binary operation
    let op = tokens[next_index].clone();
    next_index += 1; // Skip past op

    if next_index >= tokens.len() {
        println!(
            "{}",
            tokens[next_index]
                .loc
                .debug(raw, "Expected valid tight hand side binary perand:")
        );
        return None;
    }

    let rtoken = tokens[next_index].clone();
    let right = match rtoken.kind {
        TokenKind::Number => Expression::Literal(Literal::Number(rtoken)),
        TokenKind::Identifier => Expression::Literal(Literal::Identifier(rtoken)),
        _ => {
            println!(
                "{}",
                rtoken
                    .loc
                    .debug(raw, "Expected valid right hand side binary operand:")
            );
            return None;
        }
    };

    next_index += 1;

    Some((
        Expression::BinaryOperation(BinaryOperation {
            left: Box::new(left),
            right: Box::new(right),
            operator: op,
        }),
        next_index,
    ))
}

fn parse_function(raw: &[char], tokens: &[Token], index: usize) -> Option<(Statement, usize)> {
    if !expect_keyword(tokens, index, "function") {
        return None;
    }

    let mut next_index = index + 1;

    if !expect_identifier(tokens, next_index) {
        println!(
            "{}",
            tokens[next_index]
                .loc
                .debug(raw, "Expected valid identifier for function name:")
        );
        return None;
    }

    let name = tokens[next_index].clone();
    next_index += 1;

    let mut parameters: Vec<Token> = vec![];
    while !expect_syntax(tokens, next_index, ")") {
        if !parameters.is_empty() {
            if !expect_syntax(tokens, next_index, ",") {
                println!("{}", tokens[next_index].loc.debug(raw, "Expected comma or close parenthesis after parameter in function declaration:"));
                return None;
            }

            next_index += 1;
        }

        parameters.push(tokens[next_index].clone());
        next_index += 1; // skip past param
    }

    next_index += 1; // skip past close paren

    let mut statements: Vec<Statement> = vec![];
    while !expect_keyword(tokens, next_index, "end") {
        let res = parse_statement(raw, tokens, next_index);
        if let Some((stmt, next_next_index)) = res {
            next_index = next_next_index;
            statements.push(stmt);
        } else {
            println!(
                "{}",
                tokens[next_index]
                    .loc
                    .debug(raw, "Expected valid statement in function declaration:")
            );
            return None;
        }
    }

    next_index += 1;

    Some((
        Statement::FunctionDeclaration(FunctionDeclaration {
            name,
            parameters,
            body: statements,
        }),
        next_index,
    ))
}

fn parse_statement(raw: &[char], tokens: &[Token], index: usize) -> Option<(Statement, usize)> {
    let parsers = [
        parse_if,
        parse_expression_statement,
        parse_return,
        parse_local,
    ];

    for parser in parsers {
        let res = parser(raw, tokens, index);
        if res.is_some() {
            return res;
        }
    }

    None
}

fn parse_return(raw: &[char], tokens: &[Token], index: usize) -> Option<(Statement, usize)> {
    if !expect_keyword(tokens, index, "return") {
        return None;
    }

    let mut next_index = index + 1;
    let res = parse_expression(raw, tokens, next_index);
    if res.is_none() {
        println!(
            "{}",
            tokens[next_index]
                .loc
                .debug(raw, "Expected valid expression in return statement:")
        );
        return None;
    }

    let (expr, next_next_index) = res.unwrap();
    next_index = next_next_index;
    if !expect_syntax(tokens, next_index, ";") {
        println!(
            "{}",
            tokens[next_index]
                .loc
                .debug(raw, "Expected semicolon in return statement:")
        );
        return None;
    }

    next_index += 1; // Skip past semicolon
    Some((Statement::Return(Return { expression: expr }), next_index))
}

pub fn parse(raw: &[char], tokens: Vec<Token>) -> Result<Ast, String> {
    let mut ast = vec![];
    let mut index = 0;
    let ntokens = tokens.len();
    while index < ntokens {
        let res = parse_statement(raw, &tokens, index);
        if let Some((stmt, next_index)) = res {
            index = next_index;
            ast.push(stmt);
            continue;
        }

        return Err(tokens[index].loc.debug(raw, "Invalid token while parsing:"));
    }

    Ok(ast)
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
