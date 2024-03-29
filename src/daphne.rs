use std::collections::HashMap;
use std::fs::File;
use std::io::{Write, Read, stdout};
use rustyline::hint::Hinter;
use rustyline::validate::Validator;
use rustyline::highlight::Highlighter;
use rustyline::history::DefaultHistory;
use rustyline::{Editor, DefaultEditor, CompletionType, Config, Helper, Context};
use rustyline::completion::Completer;
use crossterm::{execute, terminal, cursor};
use crossterm::event::{read, Event, KeyEventKind, KeyCode};
use crossterm::terminal::{enable_raw_mode, disable_raw_mode};
use crossterm::style::{Color, Print, ResetColor, SetForegroundColor};

#[derive(Debug, Clone, PartialEq)]
enum Operation {
    Add,
    Sub,
    Mul,
    Div,
    Pow,
    UnaryPlus,
    UnaryMinus,
    LeftParen,
    RightParen,
}

#[derive(Debug, Clone, PartialEq)]
enum Symbol {
    Comma,
    Assign,
    Colon,
}

#[derive(Debug, Clone, PartialEq)]
enum Keyword {
    Def,
    Sum,
    Prod,
    With,
}

#[derive(Debug, Clone, PartialEq)]
enum TokenType {
    Number(f64),
    Operation(Operation),
    Symbol(Symbol),
    Keyword(Keyword),
    Ident(String),
}

#[derive(Debug, Clone)]
struct Token {
    ttype: TokenType,
    pos: usize,
}

fn is_numeric(ch: char) -> bool {
    ('0'..='9').contains(&ch) || ch == '.'
}

fn is_valid_ident_char(ch: char) -> bool {
    ('a'..='z').contains(&ch) ||
    ('A'..='Z').contains(&ch) ||
    ('0'..='9').contains(&ch) ||
    ch == '_'
}

fn is_unary(pos: usize, tokens: &Vec<Token>, word: &str) -> bool {
    let ch = match word.chars().nth(pos+1) {
        Some(ch) => ch,
        None => return false,
    };
    if tokens.len() == 0 { return true; }
    if !(is_numeric(ch) || is_valid_ident_char(ch)) {
        return false;
    }
    match tokens[tokens.len()-1].ttype {
        TokenType::Operation(Operation::RightParen) => false,
        TokenType::Operation(_)           |
        TokenType::Symbol(Symbol::Comma)  |
        TokenType::Symbol(Symbol::Colon)  |
        TokenType::Symbol(Symbol::Assign) => true,
        _ => false,
    }
}

fn trim_start(line: &str) -> (&str, usize) {
    let mut begin = 0;
    while begin < line.len() && line.as_bytes()[begin].is_ascii_whitespace() {
        begin += 1;
    }
    (&line[begin..], begin)
}

fn parse_keyword(word: &str) -> Option<Keyword> {
    match word {
        "def" => Some(Keyword::Def),
        "sum" => Some(Keyword::Sum),
        "prod" => Some(Keyword::Prod),
        "with" => Some(Keyword::With),
        _ => None,
    }
}

fn is_valid_ident(word: &str) -> bool {
    for c in word.chars() {
        if is_valid_ident_char(c) { continue; }
        return false;
    }
    true
}

fn parse_word(word: &str) -> Option<TokenType> {
    if let Some(keyword) = parse_keyword(word) {
        return Some(TokenType::Keyword(keyword));
    }
    if let Ok(num) = word.parse() {
        return Some(TokenType::Number(num));
    }
    if is_valid_ident(word) {
        return Some(TokenType::Ident(word.to_string()));
    }
    None
}

fn parse(input: &str) -> Result<Vec<Token>, (usize, String)> {
    let reserved_symbols = &[
        // operators
        '+', '-', '*', '/', '^',
        // symbols
        ',', '=', '(', ')', ':'
    ];
    let mut tokens = vec![];
    let (mut input, mut begin) = trim_start(input);
    while let Some(i) = input.find(|c: char| c.is_ascii_whitespace()) {
        let mut word = &input[0..i];
        input = &input[word.len()..];
        while let Some(i) = word.find(reserved_symbols) {
            if i != 0 {
                let word = &word[0..i];
                match parse_word(word) {
                    Some(ttype) => tokens.push(Token {
                        ttype: ttype,
                        pos: begin,
                    }),
                    None => return Err((begin, word.to_string())),
                }
            }
            tokens.push(Token {
                ttype: match word.chars().nth(i).expect("Char must be in input") {
                    '+' => {
                        if is_unary(i, &tokens, &word) {
                            TokenType::Operation(Operation::UnaryPlus)
                        } else {
                            TokenType::Operation(Operation::Add)
                        }
                    },
                    '-' => {
                        if is_unary(i, &tokens, &word) {
                            TokenType::Operation(Operation::UnaryMinus)
                        } else {
                            TokenType::Operation(Operation::Sub)
                        }
                    },
                    '*' => TokenType::Operation(Operation::Mul),
                    '/' => TokenType::Operation(Operation::Div),
                    '^' => TokenType::Operation(Operation::Pow),
                    '(' => TokenType::Operation(Operation::LeftParen),
                    ')' => TokenType::Operation(Operation::RightParen),
                    ',' => TokenType::Symbol(Symbol::Comma),
                    '=' => TokenType::Symbol(Symbol::Assign),
                    ':' => TokenType::Symbol(Symbol::Colon),
                    _ => unreachable!(),
                },
                pos: begin+i,
            });
            word = &word[i+1..];
            begin += i+1;
        }
        if word.len() > 0 {
            match parse_word(&word) {
                Some(ttype) => tokens.push(Token {
                    ttype: ttype,
                    pos: begin,
                }),
                None => return Err((begin, word.to_string())),
            }
            begin += word.len();
        }
        let (trimmed, offset) = trim_start(input);
        input = trimmed;
        begin += offset;
    }
    Ok(tokens)
}

#[derive(Debug)]
enum SyntaxErr {
    MissingArg(Token),
    MissingFuncArg(Token),
    MissingOp(Token, Token),
    MissingLeftParen(Token),
    MissingRightParen(Token),
    MissingFuncIdent(Token),
    MissingArgIdent(Token),
    MissingComma(Token),
    MissingAssign(Token),
    MissingFuncBody(Token),
    MissingLowerBound(Token),
    MissingUpperBound(Token),
    MissingStep(Token),
    MissingColon(Token),
    MissingWith(Token),
    MissingItIdent(Token),
    MissingSumBody(Token),
    EmptyParens(Token),
    RepeatingArgIdent(Token),
    ItEqualsToArg(Token),
    AssignInsideOfExpr(Token),
    DefInsideOfExpr(Token),
    UnexpectedComma(Token),
    UnexpectedWith(Token),
    UnexpectedColon(Token),
}

fn compile_function(tokens: &[Token]) -> Result<Func, SyntaxErr> {
    assert!(tokens.len() > 0, "There must be at least one token");
    assert!(tokens[0].ttype == TokenType::Keyword(Keyword::Def), "There must be 'def' keyword at begin of function definition");
    let mut pos = 1;
    let ident = match &tokens.get(pos) {
        Some(Token {ttype: TokenType::Ident(ident), ..}) => ident,
        _ =>  return Err(SyntaxErr::MissingFuncIdent(tokens[0].clone())),
    };
    pos += 1;
    match tokens.get(pos) {
        Some(Token {ttype: TokenType::Operation(Operation::LeftParen), ..}) => {},
        _ =>  return Err(SyntaxErr::MissingLeftParen(tokens[pos-1].clone())),
    }
    pos += 1;
    let mut args = vec![];
    loop {
        let token = match tokens.get(pos) {
            None => return Err(SyntaxErr::MissingRightParen(tokens[pos-1].clone())),
            Some(token) => token,
        };
        if token.ttype == TokenType::Operation(Operation::RightParen)
            && tokens[pos-1].ttype != TokenType::Symbol(Symbol::Comma) {
            break;
        }
        match &token.ttype {
            TokenType::Ident(arg) => {
                if args.contains(arg) {
                    return Err(SyntaxErr::RepeatingArgIdent(token.clone()));
                }
                args.push(arg.clone());
            },
            _ => return Err(SyntaxErr::MissingArgIdent(tokens[pos-1].clone())),
        }
        pos += 1;
        if pos >= tokens.len() {
            return Err(SyntaxErr::MissingRightParen(tokens[pos-1].clone()));
        }
        match tokens[pos].ttype {
            TokenType::Symbol(Symbol::Comma) => {pos += 1},
            TokenType::Operation(Operation::RightParen) => break,
            _ => return Err(SyntaxErr::MissingComma(tokens[pos-1].clone())),
        }
    }
    pos += 1;
    match tokens.get(pos) {
        Some(Token {ttype: TokenType::Symbol(Symbol::Assign), ..}) => {},
        _ => return Err(SyntaxErr::MissingAssign(tokens[pos-1].clone())),
    }
    pos += 1;
    if pos >= tokens.len() {
        return Err(SyntaxErr::MissingFuncBody(tokens[pos-1].clone()));
    }
    let mut func_id = 0;
    let expr = match compile_expr(&tokens[pos..], &args, &mut func_id) {
        Ok(instructions) => instructions,
        Err(err) => return Err(err),
    };
    Ok(Func {
        ident: ident.clone(),
        args: (*args).to_vec(),
        expr: expr,
    })
}

fn binary_op_check(pos: usize, tokens: &[Token]) -> Result<(), Token> {
    if pos == 0 || pos == tokens.len()-1 {
        return Err(tokens[pos].clone());
    }
    match tokens[pos-1].ttype {
        TokenType::Operation(Operation::RightParen) |
        TokenType::Ident(_)                         |
        TokenType::Number(_) => {},
        _ => return Err(tokens[pos].clone()),
    }
    match tokens[pos+1].ttype {
        TokenType::Operation(Operation::UnaryPlus)  |
        TokenType::Operation(Operation::UnaryMinus) |
        TokenType::Operation(Operation::LeftParen)  |
        TokenType::Ident(_)                         |
        TokenType::Number(_) => {},
        _ => return Err(tokens[pos].clone()),
    }
    Ok(())
}

#[derive(Debug, Clone)]
enum AccType {
    Sum,
    Prod,
}

#[derive(Debug, Clone)]
enum Instruction {
    PushOp(Operation),
    PushNumber(f64),
    PushArg(String),
    FunctionCall(String, usize, usize),
    ParamBegin(usize),
    Sum(String, usize),
    Prod(String, usize),
    Jump(AccType, usize),
}

fn compile_expr(tokens: &[Token], args: &[String], func_id: &mut usize) -> Result<Vec<Instruction>, SyntaxErr> {
    let mut parens = vec![];
    let mut instructions = vec![];
    let mut i = 0;
    'outer: while i < tokens.len() {
        let ttype = &tokens[i].ttype;
        match ttype {
            TokenType::Operation(
                op @ Operation::Add |
                op @ Operation::Sub |
                op @ Operation::Mul |
                op @ Operation::Div |
                op @ Operation::Pow
            ) => {
                if let Err(token) = binary_op_check(i, tokens) {
                    return Err(SyntaxErr::MissingArg(token));
                }
                instructions.push(Instruction::PushOp(op.clone()));
            },
            TokenType::Operation(
                op @ Operation::UnaryPlus |
                op @ Operation::UnaryMinus
            ) => {
                assert!(i < tokens.len()-1, "Error in parse()");
                match tokens[i+1].ttype {
                    TokenType::Number(_) | TokenType::Ident(_) |
                    TokenType::Operation(Operation::LeftParen) => {},
                    _ => unreachable!("Probably error in parse()"),
                }
                instructions.push(Instruction::PushOp(op.clone()));
            },
            TokenType::Operation(Operation::LeftParen) => {
                if i != 0 {
                    match tokens[i-1].ttype {
                        TokenType::Operation(Operation::RightParen) => {
                            return Err(SyntaxErr::MissingOp(tokens[i-1].clone(), tokens[i].clone()));
                        },
                        TokenType::Operation(_) => {},
                        _ => return Err(SyntaxErr::MissingOp(tokens[i-1].clone(), tokens[i].clone())),
                    }
                }
                if i == tokens.len()-1 {
                    return Err(SyntaxErr::MissingRightParen(tokens[i].clone()));
                }
                match tokens[i+1].ttype {
                    TokenType::Operation(Operation::UnaryPlus)  |
                    TokenType::Operation(Operation::UnaryMinus) |
                    TokenType::Operation(Operation::LeftParen)  |
                    TokenType::Keyword(Keyword::Sum)            |
                    TokenType::Keyword(Keyword::Prod)           |
                    TokenType::Ident(_)                         |
                    TokenType::Number(_) => {},
                    _ => return Err(SyntaxErr::MissingArg(tokens[i+1].clone())),
                }
                parens.push(&tokens[i]);
                instructions.push(Instruction::PushOp(Operation::LeftParen));
            },
            TokenType::Operation(Operation::RightParen) => {
                if i == 0 {
                    return Err(SyntaxErr::MissingLeftParen(tokens[i].clone()));
                }
                match tokens[i-1].ttype {
                    TokenType::Operation(Operation::RightParen) |
                    TokenType::Number(_) => {},
                    TokenType::Ident(_) => {},
                    TokenType::Operation(Operation::LeftParen) => return Err(SyntaxErr::EmptyParens(tokens[i].clone())),
                    _ => return Err(SyntaxErr::MissingArg(tokens[i-1].clone())),
                }
                if i < tokens.len()-1 {
                    match tokens[i+1].ttype {
                        TokenType::Operation(Operation::LeftParen) => {
                            return Err(SyntaxErr::MissingOp(tokens[i].clone(), tokens[i+1].clone()));
                        },
                        TokenType::Operation(_) => {},
                        _ => return Err(SyntaxErr::MissingOp(tokens[i].clone(), tokens[i+1].clone())),
                    }
                }
                if parens.len() > 0 {
                    parens.pop();
                } else {
                    return Err(SyntaxErr::MissingLeftParen(tokens[i].clone()));
                }
                instructions.push(Instruction::PushOp(Operation::RightParen));
            },
            TokenType::Number(num) => {
                if i != 0 {
                    match tokens[i-1].ttype {
                        TokenType::Operation(Operation::RightParen) |
                        TokenType::Number(_) | TokenType::Ident(_) => return Err(SyntaxErr::MissingOp(tokens[i-1].clone(), tokens[i].clone())),
                        _ => {},
                    }
                }
                if i < tokens.len()-1 {
                    match tokens[i+1].ttype {
                        TokenType::Operation(Operation::LeftParen) |
                        TokenType::Number(_) | TokenType::Ident(_) => return Err(SyntaxErr::MissingOp(tokens[i].clone(), tokens[i+1].clone())),
                        _ => {},
                    }
                }
                instructions.push(Instruction::PushNumber(*num));
            },
            TokenType::Ident(ident) => {
                for arg in args {
                    if ident == arg {
                        instructions.push(Instruction::PushArg(arg.clone()));
                        i += 1;
                        continue 'outer;
                    }
                }
                let mut pos = i + 1;
                if pos >= tokens.len() {
                    return Err(SyntaxErr::MissingLeftParen(tokens[i].clone()));
                }
                match tokens[pos].ttype {
                    TokenType::Operation(Operation::LeftParen) => {},
                    _ => return Err(SyntaxErr::MissingLeftParen(tokens[i].clone())),
                }
                pos += 1;
                let mut last = pos;
                let mut params_count = 0;
                let mut parens = 1;
                *func_id += 1;
                loop {
                    let token = match tokens.get(pos) {
                        Some(token) => token,
                        None => return Err(SyntaxErr::MissingRightParen(tokens[i+1].clone())),
                    };
                    match token.ttype {
                        TokenType::Operation(Operation::LeftParen) => parens += 1,
                        TokenType::Operation(Operation::RightParen) => parens -= 1,
                        _ => {},
                    }
                    if parens == 0 {
                        instructions.push(Instruction::ParamBegin(*func_id));
                        if pos - last > 0 {
                            match compile_expr(&tokens[last..pos], &args, func_id) {
                                Err(err) => return Err(err),
                                Ok(mut param_instr) => {
                                    instructions.push(Instruction::PushOp(Operation::LeftParen));
                                    instructions.append(&mut param_instr);
                                    instructions.push(Instruction::PushOp(Operation::RightParen));
                                    params_count += 1;
                                }
                            }
                        }
                        break;
                    }
                    if token.ttype == TokenType::Symbol(Symbol::Comma) {
                        if pos - last <= 0 {
                            return Err(SyntaxErr::MissingFuncArg(tokens[last].clone()));
                        }
                        if parens > 1 { pos += 1; continue; }
                        match compile_expr(&tokens[last..pos], &args, func_id) {
                            Err(err) => return Err(err),
                            Ok(mut param_instr) => {
                                instructions.push(Instruction::ParamBegin(*func_id));
                                instructions.push(Instruction::PushOp(Operation::LeftParen));
                                instructions.append(&mut param_instr);
                                instructions.push(Instruction::PushOp(Operation::RightParen));
                                params_count += 1;
                            }
                        }
                        if pos >= tokens.len()-1 {
                            return Err(SyntaxErr::MissingFuncArg(tokens[pos].clone()));
                        }
                        match tokens[pos+1].ttype {
                            TokenType::Operation(Operation::RightParen) => {
                                return Err(SyntaxErr::MissingFuncArg(tokens[pos].clone()));
                            },
                            _ => {},
                        }
                        last = pos + 1;
                    }
                    pos += 1;
                }
                instructions.push(Instruction::FunctionCall(ident.clone(), params_count, *func_id));
                *func_id -= 1;
                i = pos;
            },
            op @ TokenType::Keyword(Keyword::Prod) |
            op @ TokenType::Keyword(Keyword::Sum)  => {
                if instructions.len() == 0 || tokens[i-1].ttype != TokenType::Operation(Operation::LeftParen) {
                    return Err(SyntaxErr::MissingLeftParen(tokens[i].clone()));
                }
                parens.pop().unwrap();
                instructions.pop().unwrap();
                let mut pos = i + 1;
                if pos >= tokens.len() {
                    return Err(SyntaxErr::MissingLeftParen(tokens[i].clone()));
                }
                pos += 1;
                let mut last = pos;
                let mut params_count = 0;
                let mut parens = 1;
                *func_id += 1;
                loop {
                    if params_count >= 3 {
                        return Err(SyntaxErr::UnexpectedComma(tokens[i].clone()));
                    }
                    let token = match tokens.get(pos) {
                        Some(token) => token,
                        None => return Err(SyntaxErr::MissingRightParen(tokens[i+1].clone())),
                    };
                    match token.ttype {
                        TokenType::Operation(Operation::LeftParen) => parens += 1,
                        TokenType::Operation(Operation::RightParen) => parens -= 1,
                        _ => {},
                    }
                    if parens == 0 {
                        instructions.push(Instruction::ParamBegin(*func_id));
                        if pos - last > 0 {
                            match compile_expr(&tokens[last..pos], &args, func_id) {
                                Err(err) => return Err(err),
                                Ok(mut param_instr) => {
                                    instructions.push(Instruction::PushOp(Operation::LeftParen));
                                    instructions.append(&mut param_instr);
                                    instructions.push(Instruction::PushOp(Operation::RightParen));
                                    params_count += 1;
                                }
                            }
                        }
                        break;
                    }
                    if token.ttype == TokenType::Symbol(Symbol::Comma) {
                        if pos - last <= 0 {
                            return Err(SyntaxErr::MissingFuncArg(tokens[last].clone()));
                        }
                        if parens > 1 { pos += 1; continue; }
                        match compile_expr(&tokens[last..pos], &args, func_id) {
                            Err(err) => return Err(err),
                            Ok(mut param_instr) => {
                                instructions.push(Instruction::ParamBegin(*func_id));
                                instructions.push(Instruction::PushOp(Operation::LeftParen));
                                instructions.append(&mut param_instr);
                                instructions.push(Instruction::PushOp(Operation::RightParen));
                                params_count += 1;
                            }
                        }
                        if pos >= tokens.len()-1 {
                            return Err(SyntaxErr::MissingFuncArg(tokens[pos].clone()));
                        }
                        match tokens[pos+1].ttype {
                            TokenType::Operation(Operation::RightParen) => {
                                return Err(SyntaxErr::MissingFuncArg(tokens[pos].clone()));
                            },
                            _ => {},
                        }
                        last = pos + 1;
                    }
                    pos += 1;
                }
                i = pos + 1;
                match params_count {
                    0 => return Err(SyntaxErr::MissingLowerBound(tokens[i-1].clone())),
                    1 => return Err(SyntaxErr::MissingUpperBound(tokens[i-1].clone())),
                    2 => return Err(SyntaxErr::MissingStep(tokens[i-1].clone())),
                    _ => {},
                }
                match tokens.get(i) {
                    Some(token) => {
                        match token.ttype {
                            TokenType::Keyword(Keyword::With) => {},
                            _ => return Err(SyntaxErr::MissingWith(tokens[i-1].clone())),
                        }
                    },
                    None => return Err(SyntaxErr::MissingWith(tokens[i-1].clone())),
                }
                i += 1;
                let it = match tokens.get(i) {
                    Some(token) => {
                        match &token.ttype {
                            TokenType::Ident(ident) => ident,
                            _ => return Err(SyntaxErr::MissingItIdent(tokens[i-1].clone())),
                        }
                    },
                    None => return Err(SyntaxErr::MissingItIdent(tokens[i-1].clone())),
                };
                for arg in args {
                    if arg == it {
                        return Err(SyntaxErr::ItEqualsToArg(tokens[i].clone()));
                    }
                }
                i += 1;
                match tokens.get(i) {
                    Some(token) => {
                        match token.ttype {
                            TokenType::Symbol(Symbol::Colon) => {},
                            _ => return Err(SyntaxErr::MissingColon(tokens[i-1].clone())),
                        }
                    },
                    None => return Err(SyntaxErr::MissingColon(tokens[i-1].clone())),
                }
                let mut pos = i + 1;
                let mut parens = 1;
                loop {
                    let token = match tokens.get(pos) {
                        Some(token) => token,
                        None => return Err(SyntaxErr::MissingRightParen(tokens[pos-1].clone())),
                    };
                    match token.ttype {
                        TokenType::Operation(Operation::LeftParen) => parens += 1,
                        TokenType::Operation(Operation::RightParen) => parens -= 1,
                        _ => {},
                    }
                    if parens == 0 {
                        break;
                    }
                    pos += 1;
                }
                if pos == i + 1 {
                    return Err(SyntaxErr::MissingSumBody(tokens[i].clone()));
                }
                let mut args = args.to_vec();
                args.push(it.to_string());
                let mut expr = match compile_expr(&tokens[i+1..pos], &args, func_id) {
                    Err(err) => return Err(err),
                    Ok(instructions) => instructions,
                };
                match op {
                    TokenType::Keyword(Keyword::Sum) => instructions.push(Instruction::Sum(it.to_string(), *func_id)), 
                    TokenType::Keyword(Keyword::Prod) => instructions.push(Instruction::Prod(it.to_string(), *func_id)), 
                    _ => unreachable!(),
                }
                let begin = instructions.len();
                instructions.push(Instruction::PushOp(Operation::LeftParen));
                instructions.append(&mut expr);
                instructions.push(Instruction::PushOp(Operation::RightParen));
                match op {
                    TokenType::Keyword(Keyword::Sum) => instructions.push(Instruction::Jump(AccType::Sum, instructions.len() - begin)),
                    TokenType::Keyword(Keyword::Prod) => instructions.push(Instruction::Jump(AccType::Prod, instructions.len() - begin)),
                    _ => unreachable!(),
                }
                i = pos;
            },
            TokenType::Symbol(Symbol::Assign) => {
                return Err(SyntaxErr::AssignInsideOfExpr(tokens[i].clone()));
            },
            TokenType::Keyword(Keyword::Def) => {
                return Err(SyntaxErr::DefInsideOfExpr(tokens[i].clone()));
            },
            TokenType::Keyword(Keyword::With) => {
                return Err(SyntaxErr::UnexpectedWith(tokens[i].clone()));
            },
            TokenType::Symbol(Symbol::Comma) => {
                return Err(SyntaxErr::UnexpectedComma(tokens[i].clone()));
            },
            TokenType::Symbol(Symbol::Colon) => {
                return Err(SyntaxErr::UnexpectedColon(tokens[i].clone()));
            },
        }
        i += 1;
    }
    if parens.len() > 0 {
        return Err(SyntaxErr::MissingRightParen(parens[0].clone()));
    }
    Ok(instructions)
}

#[derive(Debug, Clone)]
struct Func {
    ident: String,
    args: Vec<String>,
    expr: Vec<Instruction>,
}

fn op_priority(op: &Operation) -> u8 {
    match op {
        Operation::Add | Operation::Sub => 1,
        Operation::Mul | Operation::Div => 2,
        Operation::Pow => 3,
        Operation::UnaryPlus | Operation::UnaryMinus => 4,
        Operation::LeftParen | Operation::RightParen => panic!("Parentheses has no priority and must be handled differently"),
    }
}

fn apply_op(numbers: &mut Vec<f64>, op: Operation) {
    match op {
        Operation::Add => {
            assert!(numbers.len() >= 2);
            let a = numbers.pop().unwrap();
            let b = numbers.pop().unwrap();
            numbers.push(b + a);
        },
        Operation::Sub => {
            assert!(numbers.len() >= 2);
            let a = numbers.pop().unwrap();
            let b = numbers.pop().unwrap();
            numbers.push(b - a);
        },
        Operation::Mul => {
            assert!(numbers.len() >= 2);
            let a = numbers.pop().unwrap();
            let b = numbers.pop().unwrap();
            numbers.push(b * a);
        },
        Operation::Div => {
            assert!(numbers.len() >= 2);
            let a = numbers.pop().unwrap();
            let b = numbers.pop().unwrap();
            numbers.push(b / a);
        },
        Operation::Pow => {
            assert!(numbers.len() >= 2);
            let a = numbers.pop().unwrap();
            let b = numbers.pop().unwrap();
            numbers.push(b.powf(a));
        }
        Operation::UnaryPlus => { assert!(numbers.len() > 0) },
        Operation::UnaryMinus => {
            assert!(numbers.len() > 0);
            let a = numbers.pop().unwrap();
            numbers.push(-a);
        },
        Operation::LeftParen | Operation::RightParen => panic!("Parentheses must be handled differently"),
    }
}

#[derive(Debug)]
enum EvalErr {
    ParamsCountDontMatch(String, usize, usize),
    UnknownFunction(String),
    RecursiveFunction,
}

const BUILTIN_FUNCS: [&str; 15] = [
    "sin",
    "cos",
    "tan",
    "asin",
    "acos",
    "atan",
    "abs",
    "floor",
    "ceil",
    "round",
    "ln",
    "log2",
    "log10",
    "min",
    "max",
];

fn builtin(ident: &str, params: &Vec<f64>) -> Result<f64, EvalErr> {
    assert!(BUILTIN_FUNCS.len() == 15);
    match ident {
        "sin" => {
            if params.len() != 1 {
                return Err(EvalErr::ParamsCountDontMatch("sin".to_string(), 1, params.len()));
            }
            let x = params.last().unwrap();
            return Ok(x.sin());
        },
        "cos" => {
            if params.len() != 1 {
                return Err(EvalErr::ParamsCountDontMatch("cos".to_string(), 1, params.len()));
            }
            let x = params.last().unwrap();
            return Ok(x.cos());
        },
        "tan" => {
            if params.len() != 1 {
                return Err(EvalErr::ParamsCountDontMatch("tan".to_string(), 1, params.len()));
            }
            let x = params.last().unwrap();
            return Ok(x.tan());
        },
        "asin" => {
            if params.len() != 1 {
                return Err(EvalErr::ParamsCountDontMatch("asin".to_string(), 1, params.len()));
            }
            let x = params.last().unwrap();
            return Ok(x.asin());
        },
        "acos" => {
            if params.len() != 1 {
                return Err(EvalErr::ParamsCountDontMatch("acos".to_string(), 1, params.len()));
            }
            let x = params.last().unwrap();
            return Ok(x.acos());
        },
        "atan" => {
            if params.len() != 1 {
                return Err(EvalErr::ParamsCountDontMatch("atan".to_string(), 1, params.len()));
            }
            let x = params.last().unwrap();
            return Ok(x.atan());
        },
        "abs" => {
            if params.len() != 1 {
                return Err(EvalErr::ParamsCountDontMatch("abs".to_string(), 1, params.len()));
            }
            let x = params.last().unwrap();
            return Ok(x.abs());
        },
        "ceil" => {
            if params.len() != 1 {
                return Err(EvalErr::ParamsCountDontMatch("ceil".to_string(), 1, params.len()));
            }
            let x = params.last().unwrap();
            return Ok(x.ceil());
        },
        "floor" => {
            if params.len() != 1 {
                return Err(EvalErr::ParamsCountDontMatch("floor".to_string(), 1, params.len()));
            }
            let x = params.last().unwrap();
            return Ok(x.floor());
        },
        "round" => {
            if params.len() != 1 {
                return Err(EvalErr::ParamsCountDontMatch("round".to_string(), 1, params.len()));
            }
            let x = params.last().unwrap();
            return Ok(x.round());
        },
        "ln" => {
            if params.len() != 1 {
                return Err(EvalErr::ParamsCountDontMatch("ln".to_string(), 1, params.len()));
            }
            let x = params.last().unwrap();
            return Ok(x.ln());
        },
        "log2" => {
            if params.len() != 1 {
                return Err(EvalErr::ParamsCountDontMatch("log2".to_string(), 1, params.len()));
            }
            let x = params.last().unwrap();
            return Ok(x.log2());
        },
        "log10" => {
            if params.len() != 1 {
                return Err(EvalErr::ParamsCountDontMatch("log10".to_string(), 1, params.len()));
            }
            let x = params.last().unwrap();
            return Ok(x.log10());
        },
        "max" => {
            if params.len() != 2 {
                return Err(EvalErr::ParamsCountDontMatch("max".to_string(), 2, params.len()));
            }
            let a = params[0];
            let b = params[1];
            return Ok(a.max(b));
        },
        "min" => {
            if params.len() != 2 {
                return Err(EvalErr::ParamsCountDontMatch("min".to_string(), 2, params.len()));
            }
            let a = params[0];
            let b = params[1];
            return Ok(a.min(b));
        },
        _ => unreachable!(),
    }
}

fn evaluate(functions: &HashMap<String, Func>, func: &Func, params: &Vec<f64>) -> Result<f64, EvalErr> {
    let mut numbers = vec![];
    let mut operations = vec![];
    let mut iters: Vec<(String, f64)> = vec![];
    let mut i = 0;
    'outer: while i < func.expr.len() {
        let instr = &func.expr[i];
        match instr {
            Instruction::PushNumber(num) => numbers.push(*num),
            Instruction::PushOp(Operation::LeftParen) => operations.push(Operation::LeftParen),
            Instruction::PushOp(Operation::RightParen) => {
                while let Some(last) = operations.pop() {
                    if last == Operation::LeftParen {
                        break;
                    }
                    apply_op(&mut numbers, last);
                }
            },
            Instruction::PushOp(op) => {
                if let None = operations.last() {
                    operations.push(op.clone());
                } else {
                    while let Some(last) = operations.pop() {
                        if last == Operation::LeftParen || op_priority(&last) < op_priority(&op) {
                            operations.push(last);
                            break
                        }
                        apply_op(&mut numbers, last);
                    }
                    operations.push(op.clone());
                }
            },
            Instruction::PushArg(arg) => {
                if func.args.len() != params.len() {
                    return Err(EvalErr::ParamsCountDontMatch(func.ident.clone(), func.args.len(), params.len()));
                }
                for (j, farg) in func.args.iter().enumerate() {
                    if *arg == *farg {
                        numbers.push(params[j]);
                        i += 1;
                        continue 'outer;
                    }
                }
                for iter in &iters {
                    if *arg == *iter.0 {
                        numbers.push(iter.1);
                        i += 1;
                        continue 'outer;
                    }
                }
                unreachable!("Error in compile_expr()");
            },
            Instruction::ParamBegin(_) => {},
            Instruction::FunctionCall(ident, params_count, _id) => {
                let params_count = *params_count;
                if BUILTIN_FUNCS.contains(&ident.as_str()) {
                    let mut params = vec![0f64; params_count];
                    for i in 0..params_count {
                        let param = numbers.pop().expect("In stack must be enough numbers for function call");
                        params[params_count - 1 - i] = param;
                    }
                    match builtin(&ident, &params) {
                        Ok(res) => {
                            numbers.push(res);
                            i += 1;
                            continue;
                        },
                        Err(err) => return Err(err),
                    }
                }

                let called_func = match functions.get(ident) {
                    Some(func) => func,
                    None => return Err(EvalErr::UnknownFunction(ident.clone())),
                };
                if *ident == func.ident {
                    return Err(EvalErr::RecursiveFunction);
                }
                if called_func.args.len() != params_count {
                    return Err(EvalErr::ParamsCountDontMatch(called_func.ident.clone(), called_func.args.len(), params_count));
                }
                let mut params = vec![0f64; params_count];
                for i in 0..params_count {
                    let param = numbers.pop().expect("In stack must be enough numbers for function call");
                    params[params_count - 1 - i] = param;
                }
                match evaluate(&functions, &called_func, &params) {
                    Ok(res) => numbers.push(res),
                    Err(err) => return Err(err),
                }
            },
            Instruction::Sum(it_ident, _id) => {
                assert!(numbers.len() >= 3);
                let begin = numbers[numbers.len()-3];
                iters.push((it_ident.clone(), begin));
                numbers.push(0.0);
            },
            Instruction::Prod(it_ident, _id) => {
                assert!(numbers.len() >= 3);
                let begin = numbers[numbers.len()-3];
                iters.push((it_ident.clone(), begin));
                numbers.push(1.0);
            },
            Instruction::Jump(instr_type, pos) => {
                let it = numbers.pop().unwrap();
                let count = numbers.len();
                let step = numbers[count-2];
                let end = numbers[count-3];
                let begin = numbers[count-4];
                match instr_type {
                    AccType::Sum => numbers[count-1] += it,
                    AccType::Prod => numbers[count-1] *= it,
                }
                let count = iters.len();
                if iters[count-1].1 + step < end {
                    iters[count-1].1 += step;
                    i = i - *pos;
                    continue;
                }
                let mut res = numbers.pop().unwrap();
                if begin >= end {
                    match instr_type {
                        AccType::Sum => res = 0.0,
                        AccType::Prod => res = 1.0,
                    }
                }
                numbers.pop().unwrap();
                numbers.pop().unwrap();
                numbers.pop().unwrap();
                numbers.push(res);
                iters.pop().unwrap();
            },
        }
        i += 1;
    }
    while let Some(last) = operations.pop() {
        apply_op(&mut numbers, last);
    }
    assert!(numbers.len() == 1);
    Ok(numbers.pop().unwrap())
}

const DEFAULT_TERM_WIDTH: u16 = 50;
const DEFAULT_TERM_HEIGHT: u16 = 50;

fn print_err(input: &str, message: &str, pos: usize) {
    let (twidth, _) = terminal_size();
    println!("[Error]: {message}");
    if input.len() < twidth as usize - 6 {
        print!(" ::  {input}");
        let mut pt = vec![b'-'; input.len()-1];
        pt[pos] = b'^';
        println!(" ::  {}", String::from_utf8(pt).expect("Error in pointer string"));
    } else {
        let width = ((twidth-8)/2) as usize;
        let begin = if width <= pos { pos - width } else { 0 };
        let end = if width+pos < input.len() { width+pos } else { input.len()-1 };
        let input = &input[begin..end];
        println!(" ::  ... {input} ...");
        let mut pt = vec![b'-'; end-begin];
        pt[pos - begin] = b'^';
        println!(" ::      {}", String::from_utf8(pt).expect("Error in pointer string"));
    }
}

fn syntax_err(input: &str, err: SyntaxErr) {
    match err {
        SyntaxErr::MissingArg(token) => {
            match token.ttype {
                TokenType::Operation(op) => {
                    let op_str = match op {
                        Operation::Add | Operation::UnaryPlus  => "+",
                        Operation::Sub | Operation::UnaryMinus => "-",
                        Operation::Mul => "*",
                        Operation::Div => "/",
                        Operation::Pow => "^",
                        Operation::LeftParen  => "(",
                        Operation::RightParen => ")",
                    };
                    print_err(&input, &format!("Missing argument for '{op_str}' operation"), token.pos);
                },
                other => unreachable!("Error in compile_expr(): {other:?}"),
            }
        },
        SyntaxErr::MissingFuncArg(token) => {
            print_err(&input, "Missing argument in function call", token.pos);
        },
        SyntaxErr::MissingOp(a, b) => {
            let a_str = match a.ttype {
                TokenType::Operation(Operation::LeftParen)  => "(".to_string(),
                TokenType::Operation(Operation::RightParen) => ")".to_string(),
                TokenType::Number(num)  => num.to_string(),
                TokenType::Ident(ident) => ident,
                _ => unreachable!("Error in compile_expr()"),
            };
            let b_str = match b.ttype {
                TokenType::Operation(Operation::LeftParen)  => "(".to_string(),
                TokenType::Operation(Operation::RightParen) => ")".to_string(),
                TokenType::Number(num)  => num.to_string(),
                TokenType::Ident(ident) => ident,
                _ => unreachable!("Error in compile_expr()"),
            };
            print_err(&input, &format!("Missing operation between '{a_str}' and '{b_str}'"), b.pos-1);
        },
        SyntaxErr::MissingRightParen(token) => {
            print_err(&input, &format!("Missing right parenthesis"), token.pos);
        },
        SyntaxErr::MissingLeftParen(token) => {
            print_err(&input, &format!("Missing left parenthesis"), token.pos);
        },
        SyntaxErr::EmptyParens(token) => {
            print_err(&input, &format!("Empty parentheses"), token.pos);
        },
        SyntaxErr::MissingFuncIdent(token) => {
            print_err(&input, &format!("Missing identifier of a function in definition"), token.pos);
        },
        SyntaxErr::MissingArgIdent(token) => {
            print_err(&input, &format!("Missing function argument in definition"), token.pos);
        },
        SyntaxErr::RepeatingArgIdent(token) => {
            match token.ttype {
                TokenType::Ident(arg) => {
                    print_err(&input, &format!("Argument '{arg}' already exists"), token.pos);
                },
                _ => unreachable!("Error in compile_function()"),
            }
        },
        SyntaxErr::MissingComma(token) => {
            print_err(&input, &format!("Missing comma between arguments in a function definition"), token.pos);
        },
        SyntaxErr::MissingAssign(token) => {
            print_err(&input, &format!("Missing assign in a function definition"), token.pos);
        },
        SyntaxErr::MissingFuncBody(token) => {
            print_err(&input, &format!("Missing body of a function in definition"), token.pos);
        },
        SyntaxErr::AssignInsideOfExpr(token) => {
            print_err(&input, &format!("Assigning (=) inside of expression, that allowed only in function definitions"), token.pos);
        },
        SyntaxErr::DefInsideOfExpr(token) => {
            print_err(&input, &format!("Trying to define functions inside of expression"), token.pos);
        },
        SyntaxErr::MissingLowerBound(token) => {
            print_err(&input, &format!("Missing lower bound inside of 'sum/prod' expression"), token.pos);
        },
        SyntaxErr::MissingUpperBound(token) => {
            print_err(&input, &format!("Missing upper bound inside of 'sum/prod' expression"), token.pos);
        },
        SyntaxErr::MissingStep(token) => {
            print_err(&input, &format!("Missing step inside of 'sum/prod' expression"), token.pos);
        },
        SyntaxErr::MissingSumBody(token) => {
            print_err(&input, &format!("Missing body of 'sum/prod' expression"), token.pos);
        },
        SyntaxErr::MissingColon(token) => {
            print_err(&input, &format!("Missing colon in 'sum/prod' expression"), token.pos);
        },
        SyntaxErr::UnexpectedColon(token) => {
            print_err(&input, &format!("Unexpected colon. Colon used only in 'sum/prod' expressions"), token.pos);
        },
        SyntaxErr::UnexpectedComma(token) => {
            print_err(&input, &format!("Unexpected comma. Commas used only in function calls and defenitions"), token.pos);
        },
        SyntaxErr::UnexpectedWith(token) => {
            print_err(&input, &format!("Unexpected 'with' keyword. 'with' keyword used only in 'sum/prod' expressions"), token.pos);
        },
        SyntaxErr::MissingWith(token) => {
            print_err(&input, &format!("Missing 'with' keyword in 'sum/prod' expression"), token.pos);
        },
        SyntaxErr::MissingItIdent(token) => {
            print_err(&input, &format!("Missing identifier for iterative variable in 'sum/prod' expression"), token.pos);
        },
        SyntaxErr::ItEqualsToArg(token) => {
            match token.ttype {
                TokenType::Ident(ident) => {
                    print_err(&input, &format!("Iterative variable identifier '{ident}' overlaps with argument of outer function"), token.pos);
                },
                _ => unreachable!("Error in compile_expr()"),
            }
        },
    }
}

fn eval_err(err: EvalErr) {
    match err {
        EvalErr::ParamsCountDontMatch(ident, args_cnt, params_cnt) => {
            println!("[Error]: Function '{ident}' requires {args_cnt} parameters, but found {params_cnt}");
        },
        EvalErr::UnknownFunction(ident) => {
            println!("[Error]: Function '{ident}' doesn't defined yet");
        },
        EvalErr::RecursiveFunction => {
            println!("[Error]: Recursive functions is not allowed");
        },
    }
}

fn usage() {
    println!("Usage:");
    println!(" -> <expression> | <command>");
    println!();
    println!("<expression>:");
    println!("  Expressions are divided into two types: simple expressions <expr> and function definitions <def>.");
    println!();
    println!("<expr>:");
    println!("  Mathematical expression which consist of operations <op>, numbers <num>, sums <sum>, products <prod> and function calls <call>.");
    println!("   <op>   - One of this basic operations: +, -, *, /, ^");
    println!("   <num>  - Arbitrary real number");
    println!("   <sum>  - Summarize sequence of expressions <expr>.");
    println!("     (<sum> lower_bound upper_bound step with <ident>: <expr>) - Where lower_bound is inclusive and upper_bound is exclusive. <ident> after keyword 'with' becomes an iterative variable which contains current value for each iteration.");
    println!("   <prod> - Multiplies sequence of expressions <expr>.");
    println!("     (<prod> lower_bound upper_bound step with <ident>: <expr>)");
    println!("   <call> - Call of the defined function. Function call have the next syntax:");
    println!("     <ident>(<params>) - Where <ident> is the name of the function and <params> is the list of parameters separated by comma. Where each parameter is an expression <expr>.");
    println!();
    println!("<def>:");
    println!("  def <ident>(<args>) = <expr>");
    println!();
    println!("<ident> | <arg>:");
    println!("  Names of functions and their arguments may consist of latin latters(A-Z|a-z), digits(0-9) and underscore(_).");
    println!();
    assert!(COMMANDS.len() == 10);
    println!("<command>:");
    println!("  help         - Prints this message");
    println!("  builtin      - Prints list of builtin functions");
    println!("  exit         - Exits the program");
    println!("  save <path>  - Saves all defined functions into file");
    println!("  load <path>  - Loads functions from provided file");
    println!("  list [flags] - Prints list of defined functions");
    println!("    flags: -l  - Prints full definitions");
    println!("  remove [flags] [ident] ... - Removes provided functions");
    println!("    flags: -a  - Removes all defined functions");
    println!("  plot <ident> - Plots given function");
    println!("    controls:");
    println!("      arrows - Move around");
    println!("      +/-    - Zoom in and out");
    println!("      q      - Exit plot mode");
    println!("      j      - Jump to specified position");
    println!();
    println!("  tokens <expr> - Prints tokens of expression");
    println!("  instr  <expr> - Prints instructions of expression");
    println!();
}

fn welcome() {
    println!("   ^ ^");
    println!("  (O,O)");
    println!("  (   ) Welcome to Daphne, a simple math shell. ");
    println!(" --\"-\"------------------------------------------------");
    println!("(If you don't know where to start, type command 'help')")
}

fn print_builtins() {
    assert!(BUILTIN_FUNCS.len() == 15);
    println!("  abs(x)    - Computes the absolute value of x.");
    println!("  sin(x)    - Computes the sine of x (in radians).");
    println!("  cos(x)    - Computes the cosine of x (in radians).");
    println!("  tan(x)    - Computes the tangent of x (in radians).");
    println!("  asin(x)   - Computes the arcsine of x. Return value is in radians in the range [-pi/2, pi/2] or NaN if x is outside the range [-1, 1].");
    println!("  acos(x)   - Computes the arccosine of x. Return value is in radians in the range [0, pi] or NaN if x is outside the range [-1, 1].");
    println!("  atan(x)   - Computes the arctangent of x. Return value is in radians in the range [-pi/2, pi/2].");
    println!("  ceil(x)   - Returns the smallest integer greater than or equal x.");
    println!("  floor(x)  - Returns the largest integer less than or equal to x.");
    println!("  round(x)  - Returns the nearest integer to x.");
    println!("  ln(x)     - Returns the natural logarithm of x.");
    println!("  log2(x)   - Returns the base 2 logarithm of x.");
    println!("  log10(x)  - Returns the base 10 logarithm of x.");
    println!("  max(a, b) - Returns the maximum of the two numbers.");
    println!("  min(a, b) - Returns the minimum of the two numbers.");
}

fn print_functions(functions: &HashMap::<String, Func>, full: bool) {
    if functions.len() == 0 {
        println!("  [empty]");
    }
    let mut sorted: Vec<Func> = functions.iter().map(|(_, func)| func.clone()).collect();
    sorted.sort_by(|a, b| a.ident.partial_cmp(&b.ident).unwrap());
    for func in sorted {
        let ident = func.ident;
        let mut buffer = vec![];
        write!(&mut buffer, "  {ident}(").unwrap();
        for arg in &func.args {
            write!(&mut buffer, "{arg}, ").unwrap();
        }
        if func.args.len() > 0 {
            buffer.pop().unwrap();
            buffer.pop().unwrap();
        }
        write!(&mut buffer, ")").unwrap();
        print!("{}", String::from_utf8(buffer).unwrap());
        if full {
            print!(" = ");
            write_expr(&mut stdout(), &func.expr).expect("Can't write in stdout");
        }
        println!();
    }
}

fn load_file(path: &str) -> Option<Vec<Func>> {
    let mut file = match File::open(path) {
        Ok(file) => file,
        Err(err) => {
            println!("[Error]: Can't load file '{path}': {err}");
            return None;
        },
    };
    let mut contents = String::new();
    if let Err(err) = file.read_to_string(&mut contents) {
        println!("[Error]: Can't read content of a file '{path}': {err}");
        return None;
    }
    let mut functions = vec![];
    for (i, line) in contents.lines().enumerate() {
        let mut line = line.to_string();
        line.push('\n');
        let tokens = match parse(&line) {
            Ok(tokens) => { tokens },
            Err((pos, err)) => {
                print!("({line_num}) ", line_num = i+1);
                print_err(&line, &format!("Unknown symbol in word '{err}'"), pos);
                return None;
            },
        };
        let func = match compile_function(&tokens) {
            Ok(func) => func,
            Err(err) => {
                print!("({line_num}) ", line_num = i+1);
                syntax_err(&line, err);
                return None;
            }
        };
        functions.push(func);
    }
    Some(functions)
}

fn save_file(functions: &HashMap<String, Func>, path: &str) -> std::io::Result<()> {
    let mut file = File::create(path)?;
    for (_, func) in functions.iter() {
        file.write_all(b"def ")?;
        file.write_all(func.ident.as_bytes())?;
        file.write_all(b"(")?;
        for (i, arg) in func.args.iter().enumerate() {
            file.write_all(arg.as_bytes())?;
            if i < func.args.len()-1 {
                file.write_all(b", ")?;
            }
        }
        file.write_all(b") = ")?;
        write_expr(&mut file, &func.expr)?;
        file.write_all(b"\n")?;
    }
    Ok(())
}

fn write_expr<T: Write>(file: &mut T, expr: &[Instruction]) -> std::io::Result<()> {
    let mut i = 0;
    let mut params_begin = 0;
    'outer: while i < expr.len() {
        let instr = &expr[i];
        match instr {
            Instruction::PushNumber(num) => file.write_all(num.to_string().as_bytes())?,
            Instruction::PushOp(op) => {
                match op {
                    Operation::Add | Operation::UnaryPlus => file.write_all(b"+")?,
                    Operation::Sub | Operation::UnaryMinus => file.write_all(b"-")?,
                    Operation::Mul => file.write_all(b"*")?,
                    Operation::Div => file.write_all(b"/")?,
                    Operation::Pow => file.write_all(b"^")?,
                    Operation::LeftParen => file.write_all(b"(")?,
                    Operation::RightParen => file.write_all(b")")?,
                }
            },
            Instruction::PushArg(arg) => file.write_all(arg.as_bytes())?,
            Instruction::ParamBegin(func_id) => {
                params_begin = i+1;
                loop {
                    match expr[i] {
                        Instruction::FunctionCall(_, _, id) => {
                            if id == *func_id {
                                continue 'outer;
                            }
                        },
                        Instruction::Sum(_, id) |
                        Instruction::Prod(_, id) => {
                            if id == *func_id {
                                continue 'outer;
                            }
                        },
                        _ => {},
                    }
                    i += 1;
                }
            }
            Instruction::FunctionCall(ident, params_count, id) => {
                let params_count = *params_count;
                file.write_all(ident.as_bytes())?;
                file.write_all(b"(")?;
                let mut begin = params_begin+1;
                let mut count = 0;
                for j in params_begin..i-1 {
                    match expr[j] {
                        Instruction::ParamBegin(func_id) => {
                            if *id == func_id {
                                write_expr(file, &expr[begin..j-1])?;
                                begin = j + 2;
                                count += 1;
                                if count < params_count {
                                    file.write_all(b", ")?;
                                }
                            }
                        },
                        _ => {},
                    }
                }
                if begin < i {
                    write_expr(file, &expr[begin..i-1])?;
                }
                file.write_all(b")")?;
            },
            op @ Instruction::Sum(it_ident, id)  |
            op @ Instruction::Prod(it_ident, id) => {
                match op {
                    Instruction::Sum(_, _) => file.write_all(b"(sum(")?,
                    Instruction::Prod(_, _) => file.write_all(b"(prod(")?,
                    _ => unreachable!(),
                }
                let params_count = 3;
                let mut begin = params_begin+1;
                let mut count = 0;
                for j in params_begin..i-1 {
                    match expr[j] {
                        Instruction::ParamBegin(func_id) => {
                            if *id == func_id {
                                write_expr(file, &expr[begin..j-1])?;
                                begin = j + 2;
                                count += 1;
                                if count < params_count {
                                    file.write_all(b", ")?;
                                }
                            }
                        },
                        _ => {},
                    }
                }
                if begin < i {
                    write_expr(file, &expr[begin..i-1])?;
                }
                file.write_all(format!(") with {it_ident}: ").as_bytes())?;
            },
            Instruction::Jump(_it_ident, _pos) => {
                file.write_all(b")")?;
            },
        }
        i += 1;
    }
    Ok(())
}

struct State {
    quit: bool,
    functions: HashMap<String, Func>,
    rl: Editor<CmdHelper, DefaultHistory>,
}

fn readline(editor: &mut Editor<CmdHelper, DefaultHistory>, message: &str) -> Option<String> {
    let input = match editor.readline(message) {
        Ok(line) => {
            match editor.add_history_entry(line.as_str()) {
                Err(err) => println!("[Info]: Can't add entry to history '{err}'"),
                Ok(_) => {},
            }
            line
        },
        Err(err) => {
            println!("{err}");
            return None;
        }
    };
    Some(input)
}

fn exec_command(state: &mut State, input: &str) -> bool {
    let command = input.split_ascii_whitespace().next().expect("There must be at least a command in input");
    let mut args = input.split_ascii_whitespace().skip(1);
    assert!(COMMANDS.len() == 10);
    match command {
        "exit" => {
            if let Some(arg) = args.next() {
                println!("[Error]: Unknown argument '{arg}' for command 'exit'");
                return true;
            }
            state.quit = true;
            return true;
        },
        // TODO: Add search to list and builtin commands
        "list" => {
            match args.next() {
                Some("-l") => print_functions(&state.functions, true),
                None => print_functions(&state.functions, false),
                Some(arg) => println!("[Error]: Unknown argument '{arg}' for command 'list'"),
            }
            return true;
        },
        "builtin" => {
            if let Some(arg) = args.next() {
                println!("[Error]: Unknown argument '{arg}' for command 'list'");
                return true;
            }
            print_builtins();
            return true;
        },
        "help" => {
            if let Some(arg) = args.next() {
                println!("[Error]: Unknown argument '{arg}' for command 'help'");
                return true;
            }
            usage();
            return true;
        },
        "load" => {
            let path = match args.next() {
                Some(arg) => arg,
                None => {
                    println!("[Error]: Missing argument '<path>' for command 'load'");
                    return true;
                },
            };
            if let Some(arg) = args.next() {
                println!("[Error]: Unknown argument '{arg}' for command 'load'");
                return true;
            }
            let funcs = match load_file(path) {
                Some(functions) => functions,
                None => return true,
            };
            let mut redefined = vec![];
            for func in &funcs {
                if BUILTIN_FUNCS.contains(&func.ident.as_str()) {
                    println!("[Error]: Attempt to redefine builtin function '{}'", func.ident);
                    return true;
                }
                if state.functions.contains_key(&func.ident) {
                    redefined.push(&func.ident);
                }
            }
            let count = redefined.len();
            if count > 0 {
                println!("Loading of this file will redefine some functions ({count}):");
                for rd in redefined {
                    println!("  {rd}");
                }
                match state.rl.readline("Do you want to continue? (y/n): ") {
                    Ok(res) => match res.to_lowercase().as_str() {
                        "y" => {
                            for func in &funcs {
                                state.functions.insert(func.ident.clone(), func.clone());
                            }
                            println!("  Successfully redefined {count} functions");
                        },
                        _ => {
                            println!("  Cancelled");
                            return true;
                        }
                    },
                    Err(err) => {
                        println!("{err}");
                        state.quit = true;
                        return true;
                    }
                }
            }
            let funcs_count = funcs.len();
            for func in funcs {
                state.functions.insert(func.ident.clone(), func);
            }
            println!("  Successfully loaded {funcs_count} functions");
            return true;
        },
        "save" => {
            let path = match args.next() {
                Some(arg) => arg,
                None => {
                    println!("[Error]: Missing argument 'path' for command 'save'");
                    return true;
                },
            };
            if let Some(arg) = args.next() {
                println!("[Error]: Unknown argument '{arg}' for command 'save'");
                return true;
            }
            if let Err(err) = save_file(&state.functions, &path) {
                println!("[Error]: Can't save functions in a file '{path}': {err}");
                return true;
            }
            println!("  Succsessfully saved {count} functions in '{path}'", count = state.functions.len());
            return true;
        },
        "plot" => {
            let ident = match args.next() {
                Some(arg) => arg,
                None => {
                    println!("[Error]: Missing argument 'ident' for command 'plot'");
                    return true;
                },
            };
            if let Some(arg) = args.next() {
                println!("[Error]: Unknown argument '{arg}' for command 'plot'");
                return true;
            }
            if BUILTIN_FUNCS.contains(&ident) {
                let builtin = Func {
                    ident: ident.to_string(),
                    args: vec!["x".to_string()],
                    expr: vec![
                        Instruction::ParamBegin(0),
                        Instruction::PushArg("x".to_string()),
                        Instruction::FunctionCall(ident.to_string(), 1, 0),
                    ],
                };
                match plot_mode(state, &builtin, 0.0, 0.0, 10.0, 10.0) {
                    Err(err) => println!("[Error]: {err}"),
                    Ok(()) => {},
                }
                return true;
            }
            let func = state.functions.iter().find(|(_, f)| f.ident == ident);
            match func {
                Some((_, func)) => {
                    match plot_mode(state, &func.clone(), 0.0, 0.0, 10.0, 10.0) {
                        Err(err) => println!("[Error]: {err}"),
                        Ok(()) => {},
                    }
                    return true;
                },
                None => {
                    println!("[Error]: Function with the name '{ident}' is not defined yet");
                    return true;
                }
            }
        },
        "remove" => {
            let mut funcs = vec![];
            match args.next() {
                Some("-a") => {
                    if let Some(arg) = args.next() {
                        println!("[Error]: Unexpected argument '{arg}' after flag '-a'");
                        return true;
                    }
                    let funcs_count = state.functions.len();
                    state.functions.clear();
                    println!("  Successfully removed {funcs_count} functions");
                    return true;
                },
                Some(arg) => funcs.push(arg),
                None => {
                    println!("[Error]: Missing argument '<ident>' for command 'remove'");
                    return true;
                },
            };
            while let Some(arg) = args.next() {
                funcs.push(arg);
            }
            for func in &funcs {
                if !state.functions.contains_key(*func) {
                    println!("[Error]: Function '{func}' is not defined");
                    return true;
                }
            }
            for func in &funcs {
                state.functions.remove(*func);
            }
            println!("  Successfully removed {} functions", funcs.len());
            return true;
        },
        "tokens" => {
            let mut expr = args.collect::<Vec<_>>().join(" ");
            expr.push('\n');
            let tokens = match parse(&expr) {
                Ok(tokens) => tokens,
                Err((pos, err)) => {
                    print_err(&expr, &format!("Unknown symbol in word '{err}'"), pos);
                    return true;
                },
            };
            println!("Tokens: [");
            for token in tokens {
                println!("  {{ ttype: {:?}, pos: {} }},", token.ttype, token.pos);
            }
            println!("]");
            return true;
        },
        "instr" => {
            let mut expr = args.collect::<Vec<_>>().join(" ");
            expr.push('\n');
            let tokens = match parse(&expr) {
                Ok(tokens) => tokens,
                Err((pos, err)) => {
                    print_err(&expr, &format!("Unknown symbol in word '{err}'"), pos);
                    return true;
                },
            };
            let is_def = tokens.len() > 0 && tokens[0].ttype == TokenType::Keyword(Keyword::Def);
            let instr = if is_def {
                match compile_function(&tokens) {
                    Ok(func) => func.expr,
                    Err(err) => {
                        syntax_err(&expr, err);
                        return true;
                    },
                }
            } else {
                let mut func_id = 0;
                match compile_expr(&tokens, &[], &mut func_id) {
                    Ok(tokens) => tokens,
                    Err(err) => {
                        syntax_err(&expr, err);
                        return true;
                    },
                }
            };
            println!("Instructions: [");
            for ins in instr {
                println!("  {:?},", ins);
            }
            println!("]");
            return true;
        }
        _ => return false,
    }
}

fn compile(state: &mut State, tokens: &Vec<Token>, input: &str) -> Option<Vec<Instruction>> {
    let is_def = tokens.len() > 0 && tokens[0].ttype == TokenType::Keyword(Keyword::Def);
    if is_def {
        match compile_function(&tokens) {
            Ok(func) => {
                let ident = func.ident.clone();
                if BUILTIN_FUNCS.contains(&ident.as_str()) {
                    println!("[Error]: Attempt to redefine builtin function '{ident}'");
                    return None;
                }
                if state.functions.contains_key(&ident) {
                    match state.rl.readline(format!("Function '{ident}' already exist, want to redefine? (y/n): ").as_str()) {
                        Ok(res) => match res.to_lowercase().as_str() {
                            "y" => {
                                state.functions.insert(ident.clone(), func.clone());
                                println!("  Successfully redefined");
                            },
                            _ => {
                                println!("  Cancelled");
                            }
                        },
                        Err(err) => {
                            println!("{err}");
                            state.quit = true;
                            return None;
                        }
                    }
                    return None;
                }
                state.functions.insert(ident.clone(), func.clone());
                println!("  Successfully defined");
                return None;
            },
            Err(err) => {
                syntax_err(&input, err);
                return None;
            },
        }
    } else {
        let mut func_id = 0;
        match compile_expr(&tokens, &vec![], &mut func_id) {
            Ok(expr) => return Some(expr),
            Err(err) => {
                syntax_err(&input, err);
                return None;
            }
        }
    }
}

fn terminal_size() -> (u16, u16) {
    match terminal::size() {
        Ok((twidth, theight)) => (twidth, theight),
        Err(err) => {
            println!("[Error]: Can't get the width of the terminal, formatting may be screwed: {err}");
            (DEFAULT_TERM_WIDTH, DEFAULT_TERM_HEIGHT)
        },
    }
}

fn plot_mode_err(message: &str) -> std::io::Result<()> {
    let (_, theight) = terminal_size();
    execute!(
        stdout(),
        cursor::MoveTo(0, theight-1),
        terminal::Clear(terminal::ClearType::CurrentLine),
        Print(message),
        cursor::Hide,
    )?;
    Ok(())
}

fn plot_mode(state: &mut State, function: &Func, mut x0: f64, mut y0: f64, mut width: f64, mut height: f64) -> std::io::Result<()> {
    let mut stdout = stdout();
    enable_raw_mode()?;
    execute!(
        stdout,
        terminal::Clear(terminal::ClearType::All),
        cursor::MoveTo(0, 0),
        cursor::Hide,
    )?;

    print_plot(&state.functions, &function, x0-width/2.0, x0+width/2.0, y0-height/2.0, y0+height/2.0)?;
    let mut editor = DefaultEditor::new().expect("Can't create editor");
    loop {
        let (twidth, theight) = terminal_size();
        let xstep = width/(twidth-2) as f64 * 2.0;
        let ystep = height/(theight-3) as f64 * 2.0;
        let event = read().unwrap();
        match event {
            Event::Key(event) => {
                if event.kind == KeyEventKind::Press {
                    match event.code {
                        KeyCode::Char('q') => break,
                        KeyCode::Char('j') => {
                            execute!(stdout, cursor::Show)?;
                            let input = editor.readline("jump to: ").expect("Can't read line");
                            execute!(stdout, terminal::ScrollDown(1))?;
                            let mut args = input.trim().split_ascii_whitespace();
                            match args.next() {
                                Some(arg) => {
                                    match arg.parse::<f64>() {
                                        Ok(x) => x0 = x,
                                        Err(err) => {
                                            print_plot(&state.functions, &function, x0-width/2.0, x0+width/2.0, y0-height/2.0, y0+height/2.0)?;
                                            plot_mode_err(&format!("[Error]: Can't parse value of 'x': {err}"))?;
                                            continue;
                                        },
                                    }
                                },
                                None => {
                                    print_plot(&state.functions, &function, x0-width/2.0, x0+width/2.0, y0-height/2.0, y0+height/2.0)?;
                                    plot_mode_err("[Error]: Missing argument 'x'")?;
                                    continue;
                                }
                            };
                            match args.next() {
                                Some(arg) => {
                                    match arg.parse::<f64>() {
                                        Ok(y) => y0 = y,
                                        Err(err) => {
                                            print_plot(&state.functions, &function, x0-width/2.0, x0+width/2.0, y0-height/2.0, y0+height/2.0)?;
                                            plot_mode_err(&format!("[Error]: Can't parse value of 'y': {err}"))?;
                                            continue;
                                        },
                                    }
                                },
                                None => {},
                            };
                            execute!(stdout, cursor::Hide)?;
                        },
                        KeyCode::Left  => { x0 -= xstep; },
                        KeyCode::Right => { x0 += xstep; },
                        KeyCode::Down  => { y0 -= ystep/2.0; },
                        KeyCode::Up    => { y0 += ystep/2.0; },
                        KeyCode::Char('+') => {
                            width -= xstep*2.0;
                            height -= ystep;
                            if width <= 0.0 {
                                width += xstep*2.0;
                            }
                            if height <= 0.0 {
                                height += ystep;
                            }
                        },
                        KeyCode::Char('-') => {
                            width += xstep*2.0;
                            height += ystep;
                        },
                        _ => {},
                    }
                }
            },
            _ => {},
        }
        print_plot(&state.functions, &function, x0-width/2.0, x0+width/2.0, y0-height/2.0, y0+height/2.0)?;
    }
    execute!(stdout, cursor::Show)?;
    disable_raw_mode()?;
    Ok(())
}

fn print_plot(functions: &HashMap<String, Func>, function: &Func, xmin: f64, xmax: f64, ymin: f64, ymax: f64) -> std::io::Result<()> {
    let (twidth, theight) = terminal_size();
    let theight = (theight - 1) as usize;
    let twidth = twidth as usize;

    let mut plot = vec!['·'; twidth*theight];
    // Frame
    for i in 0..twidth {
        plot[i] = '─';
        plot[(theight-1)*twidth+i] = '─';
    }
    for i in 0..theight {
        plot[twidth*i] = '│';
        plot[twidth*i+twidth-1] = '│';
    }
    plot[0] = '╭';
    plot[twidth-1] = '╮';
    plot[twidth*(theight-1)] = '╰';
    plot[twidth*theight-1] = '╯';

    // x axis
    if ymin <= 0.0 && ymax >= 0.0 {
        let mut y0 = ((0.0 - ymin) / (ymax-ymin) * (theight-2) as f64) as usize;
        y0 = (theight-2)-y0;
        for i in 0..twidth {
            plot[y0*twidth+i] = '─';
        }
        plot[y0*twidth] = '├';
        plot[y0*twidth+twidth-1] = '┤';
    }
    // y axis
    if xmin <= 0.0 && xmax >= 0.0 {
        let x0 = ((0.0 - xmin) / (xmax-xmin) * (twidth-2) as f64) as usize + 1;
        for i in 0..theight {
            plot[twidth*i+x0] = '│';
        }
        plot[x0] = '┬';
        plot[twidth*(theight-1)+x0] = '┴';
    }
    // center
    if ymin <= 0.0 && ymax >= 0.0 && xmin <= 0.0 && xmax >= 0.0 {
        let mut y0 = ((0.0 - ymin) / (ymax-ymin) * (theight-2) as f64) as usize;
        y0 = (theight-2)-y0;
        let x0 = ((0.0 - xmin) / (xmax-xmin) * (twidth-2) as f64) as usize + 1;
        plot[y0*twidth+x0] = '┼';
    }

    for i in 1..twidth-1 {
        if i < twidth/2 { 
            plot[theight/2*twidth+i] = '╴';
        } else {
            plot[theight/2*twidth+i] = '╶';
        }
    }
    for i in 1..theight-1 {
        if i < theight/2 { 
            plot[twidth*i+twidth/2] = '╵';
        } else {
            plot[twidth*i+twidth/2] = '╷';
        }
    }
    plot[theight/2*twidth+twidth/2] = '•';

    let mut stdout = stdout();
    execute!(
        stdout,
        cursor::MoveTo(0, 0),
        SetForegroundColor(Color::Rgb{r: 110, g: 115, b: 141}),
        Print(plot.iter().collect::<String>()),
        ResetColor
    )?;

    let xstep = (xmax-xmin)/(twidth-2) as f64;
    let ystep = (ymax-ymin)/(theight-2) as f64;
    let mut x = xmin;
    while x <= xmax - xstep {
        match evaluate(&functions, function, &vec![x]) {
            Ok(y) => {
                if y.is_nan() || y > ymax - ystep || y < ymin {
                    x += xstep;
                    continue;
                }
                let x = ((x - xmin) / (xmax-xmin) * (twidth-2) as f64).round() as usize;
                let y = ((y - ymin) / (ymax-ymin) * (theight-2) as f64).round() as usize;
                let y = (theight-2)-y;
                let x = x + 1;
                execute!(
                    stdout,
                    cursor::MoveTo(x as u16, y as u16),
                    Print('•'),
                )?;
            },
            Err(err) => {
                eval_err(err);
                return Ok(());
            },
        }
        x += xstep;
    }
    let x = (twidth as f64 / 2.0) / twidth as f64 * (xmax-xmin) + xmin;
    let y = match evaluate(&functions, function, &vec![x]) {
        Ok(y) => y,
        Err(_err) => todo!(),
    };
    execute!(
        stdout,
        cursor::MoveTo(0, theight as u16 + 1),
        terminal::Clear(terminal::ClearType::CurrentLine),
        Print(format!("x = {:.2}, y = {:.2}, {}(x) = {:.2}", x, ymin+(ymax-ymin)/2.0, function.ident, y)),
    )?;
    Ok(())
}

const COMMANDS: [&str; 10] = [
    "help",
    "builtin",
    "exit",
    "save",
    "load",
    "list",
    "remove",
    "plot",
    "tokens",
    "instr",
];

struct CmdHelper {
    functions: Vec<String>,
}

impl Completer for CmdHelper {
    type Candidate = String;
    fn complete(&self, line: &str, pos: usize, _ctx: &Context<'_>) -> rustyline::Result<(usize, Vec<Self::Candidate>)> {
        let mut candidates = vec![];
        if line.len() == 0 {
            for ident in BUILTIN_FUNCS {
                candidates.push(ident.to_string());
            }
            for ident in &self.functions {
                candidates.push(ident.to_string());
            }
            for cmd in COMMANDS {
                candidates.push(cmd.to_string());
            }
            candidates.sort_by(|a, b| a.partial_cmp(&b).unwrap());
            return Ok((0, candidates));
        }
        let mut begin = pos-1;
        while begin > 0 {
            match line.chars().nth(begin) {
                Some(ch) => {
                    if is_valid_ident_char(ch) {
                        begin -= 1;
                        continue;
                    }
                    begin += 1;
                    break;
                },
                None => unreachable!(),
            }
        }
        let word = &line[begin..pos];
        for ident in BUILTIN_FUNCS {
            if ident.len() >= word.len() && &ident[..word.len()] == word {
                candidates.push(ident.to_string());
            }
        }
        for ident in &self.functions {
            if ident.len() >= word.len() && &ident[..word.len()] == word {
                candidates.push(ident.to_string());
            }
        }
        for cmd in COMMANDS {
            if cmd.len() >= word.len() && &cmd[..word.len()] == word {
                candidates.push(cmd.to_string());
            }
        }
        candidates.sort_by(|a, b| a.partial_cmp(&b).unwrap());
        Ok((begin, candidates))
    }
}

impl Hinter for CmdHelper { type Hint = String; }
impl Validator for CmdHelper { }
impl Highlighter for CmdHelper { }
impl Helper for CmdHelper { }

// TODO: Update usage() and README.md
fn main() -> rustyline::Result<()> {
    let config = Config::builder()
        .history_ignore_space(true)
        .completion_type(CompletionType::Circular)
        .build();
    let mut state = State {
        quit: false,
        functions: HashMap::<String, Func>::new(),
        rl: Editor::<CmdHelper, DefaultHistory>::with_config(config)?,
    };
    let helper = CmdHelper {
        functions: vec![],
    };
    state.rl.set_helper(Some(helper));
    welcome();
    while !state.quit {
        state.rl.helper_mut().expect("There must be a helper")
            .functions = state.functions.iter().map(|(ident, _)| ident.clone()).collect();
        let mut input = match readline(&mut state.rl, "-> ") {
            Some(input) => input,
            None => {
                state.quit = true;
                continue;
            },
        };
        if let None = input.split_ascii_whitespace().next() {
            continue;
        }
        if exec_command(&mut state, &input) {
            continue;
        }
        input.push('\n');
        let tokens = match parse(&input) {
            Ok(tokens) => { tokens },
            Err((pos, err))   => {
                print_err(&input, &format!("Unknown symbol in word '{err}'"), pos);
                continue;
            },
        };
        match compile(&mut state, &tokens, &input) {
            Some(instructions) => {
                let main = Func {
                    ident: "main".to_string(),
                    args: vec![],
                    expr: instructions,
                };
                match evaluate(&state.functions, &main, &vec![]) {
                    Ok(res) => println!(" = {res}"),
                    Err(err) => eval_err(err),
                }
            },
            None => continue,
        }
    }
    Ok(())
}
