use std::io::{Write, Read};
use std::fs::File;
use std::collections::HashMap;
use terminal_size::terminal_size;
use rustyline::DefaultEditor;

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
}

#[derive(Debug, Clone, PartialEq)]
enum Keyword {
    Def,
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
        TokenType::Operation(_) => true,
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
        ',', '=', '(', ')',
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
    MissingOp(Token, Token),
    MissingLeftParen(Token),
    MissingRightParen(Token),
    EmptyParens(Token),
    MissingFuncIdent(Token),
    MissingArgIdent(Token),
    RepeatingArgIdent(Token),
    MissingComma(Token),
    MissingAssign(Token),
    MissingFuncBody(Token),
    AssignInsideOfExpr(Token),
    DefInsideOfExpr(Token),
}

fn create_function(tokens: &[Token]) -> Result<Func, SyntaxErr> {
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
    let expr = match create_expr(&tokens[pos..], &args) {
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
enum Instruction {
    PushOp(Operation),
    PushNumber(f64),
    PushArg(String),
    FunctionCall(String, Vec<Vec<Instruction>>),
}

fn create_expr(tokens: &[Token], args: &[String]) -> Result<Vec<Instruction>, SyntaxErr> {
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
                let mut params = vec![];
                let mut parens = 1;
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
                        if pos - last > 0 {
                            match create_expr(&tokens[last..pos], &args) {
                                Err(err) => return Err(err),
                                Ok(instructions) => params.push(instructions),
                            }
                        }
                        break;
                    }
                    if token.ttype == TokenType::Symbol(Symbol::Comma) {
                        if pos - last <= 0 {
                            return Err(SyntaxErr::MissingArg(tokens[last].clone()));
                        }
                        if parens > 1 { pos += 1; continue; }
                        match create_expr(&tokens[last..pos], &args) {
                            Err(err) => return Err(err),
                            Ok(instructions) => params.push(instructions),
                        }
                        if pos >= tokens.len()-1 {
                            return Err(SyntaxErr::MissingArg(tokens[pos].clone()));
                        }
                        match tokens[pos+1].ttype {
                            TokenType::Operation(Operation::RightParen) => {
                                return Err(SyntaxErr::MissingArg(tokens[pos].clone()));
                            },
                            _ => {},
                        }
                        last = pos + 1;
                    }
                    pos += 1;
                }
                instructions.push(Instruction::FunctionCall(ident.clone(), params));
                i = pos;
            },
            TokenType::Symbol(Symbol::Assign) => {
                return Err(SyntaxErr::AssignInsideOfExpr(tokens[i].clone()));
            },
            TokenType::Keyword(Keyword::Def) => {
                return Err(SyntaxErr::DefInsideOfExpr(tokens[i].clone()));
            },
            TokenType::Symbol(Symbol::Comma) => unreachable!(),
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

fn evaluate(functions: &HashMap<String, Func>, func: &Func, params: &Vec<f64>) -> Result<f64, EvalErr> {
    let mut numbers = vec![];
    let mut operations = vec![];
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
                unreachable!("Error in create_expr()");
            },
            Instruction::FunctionCall(ident, fparams) => {
                let called_func = match functions.get(ident) {
                    Some(func) => func,
                    None => return Err(EvalErr::UnknownFunction(ident.clone())),
                };
                if *ident == func.ident {
                    return Err(EvalErr::RecursiveFunction);
                }
                if called_func.args.len() != fparams.len() {
                    return Err(EvalErr::ParamsCountDontMatch(called_func.ident.clone(), called_func.args.len(), fparams.len()));
                }
                let mut eval_params = vec![];
                for param in fparams {
                    let temp = Func {
                        ident: "temp".to_string(),
                        args: func.args.clone(),
                        expr: param.to_vec(),
                    };
                    match evaluate(&functions, &temp, &params) {
                        Ok(res) => eval_params.push(res),
                        Err(err) => return Err(err),
                    }
                }
                match evaluate(&functions, &called_func, &eval_params) {
                    Ok(res) => numbers.push(res),
                    Err(err) => return Err(err),
                }
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

fn print_err(input: &str, message: &str, pos: usize) {
    const DEFAULT_TERM_WIDTH: u16 = 50;
    const DEFAULT_TERM_HEIGHT: u16 = 50;
    let (terminal_size::Width(twidth), _) = match terminal_size() {
        Some(tsize) => tsize,
        None => {
            println!("[Error]: Can't get the width of the terminal, formatting may be screwed");
            (terminal_size::Width(DEFAULT_TERM_WIDTH), terminal_size::Height(DEFAULT_TERM_HEIGHT))
        },
    };
    println!("[Error]: {message}");
    if input.len() < twidth as usize - 6 {
        print!(" ::  {input}");
        let mut pt = vec![b'-'; input.len()-1];
        pt[pos] = b'^';
        println!(" ::  {}", String::from_utf8(pt).expect("Error in pointer string"));
    } else {
        let width = ((twidth-8)/2) as usize;
        let begin = if width <= pos {
            pos - width
        } else {
            0
        };
        let end = if width+pos < input.len() {
            width+pos
        } else {
            input.len()-1
        };
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
                _ => unreachable!("Error in create_expr()"),
            }
        },
        SyntaxErr::MissingOp(a, b) => {
            let a_str = match a.ttype {
                TokenType::Operation(Operation::LeftParen)  => "(".to_string(),
                TokenType::Operation(Operation::RightParen) => ")".to_string(),
                TokenType::Number(num)  => num.to_string(),
                TokenType::Ident(ident) => ident,
                _ => unreachable!("Error in create_expr()"),
            };
            let b_str = match b.ttype {
                TokenType::Operation(Operation::LeftParen)  => "(".to_string(),
                TokenType::Operation(Operation::RightParen) => ")".to_string(),
                TokenType::Number(num)  => num.to_string(),
                TokenType::Ident(ident) => ident,
                _ => unreachable!("Error in create_expr()"),
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
                _ => unreachable!("Error in create_function()"),
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

// TODO: Update help
fn usage() {
    println!("Usage:");
    println!(" -> <expression> | <command>");
    println!();
    println!("<expression>:");
    println!("  Expressions are divided into two types: simple expressions <expr> and function definitions <def>.");
    println!();
    println!("<expr>:");
    println!("  Mathematical expression which consist of operations <op>, numbers <num> and function calls <call>.");
    println!("   <op>   - One of this basic operations: +, -, *, /, ^");
    println!("   <num>  - Arbitrary real number");
    println!("   <call> - Call of the defined function. Function call have the next syntax:");
    println!("     <ident>(<params>) - Where <ident> is the name of the function and <params> is the list of parameters separated by comma. Parameters can be just numbers <num> or expressions <expr>.");
    println!();
    println!("<def>:");
    println!("  def <ident>(<args>) = <expr>");
    println!();
    println!("<ident> | <arg>:");
    println!("  Names of functions and their arguments may consist of latin latters(A-Z|a-z) and underscore(_).");
    println!();
    println!("<command>:");
    println!("  help        - Prints this message");
    println!("  list        - Prints list of defined functions");
    println!("  exit        - Exits the program");
    println!("  save <path> - Saves all defined functions into file");
    println!("  load <path> - Loads functions from provided file");
    println!();
}

fn welcome() {
    println!("   ^ ^                       ");
    println!("  (O,O) Welcome to Daphne,   ");
    println!("  (   ) a simple math shell. ");
    println!(" --\"-\"------------------------------------------------");
    println!("(If you don't know where to start, type command 'help')")
}

fn print_functions(functions: &HashMap::<String, Func>) {
    if functions.len() == 0 {
        println!("    empty");
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
        println!("{}", String::from_utf8(buffer).unwrap());
    }
}

fn load_file(path: &str) -> Option<Vec<Func>> {
    let mut file = match File::open(path) {
        Ok(file) => file,
        Err(err) => {
            println!("[Error]: Can't load file 'path': {err}");
            return None;
        },
    };
    let mut contents = String::new();
    if let Err(err) = file.read_to_string(&mut contents) {
        println!("[Error]: Can't read content of a file 'path': {err}");
        return None;
    }
    let mut functions = vec![];
    for line in contents.lines() {
        let mut line = line.to_string();
        line.push('\n');
        let tokens = match parse(&line) {
            Ok(tokens) => { tokens },
            Err((pos, err))   => {
                print_err(&line, &format!("Unknown symbol in word '{err}'"), pos);
                return None;
            },
        };
        let func = match create_function(&tokens) {
            Ok(func) => func,
            Err(err) => {
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
        save_expr(&mut file, &func.expr)?;
        file.write_all(b"\n")?;
    }
    Ok(())
}

fn save_func_call(file: &mut File, ident: &str, params: &Vec<Vec<Instruction>>) -> std::io::Result<()> {
    file.write_all(ident.as_bytes())?;
    file.write_all(b"(")?;
    for (i, expr) in params.iter().enumerate() {
        save_expr(file, &expr)?;
        if i != params.len()-1 {
            file.write_all(b", ")?;
        }
    }
    file.write_all(b")")?;
    Ok(())
}

fn save_expr(file: &mut File, expr: &Vec<Instruction>) -> std::io::Result<()> {
    for instr in expr {
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
            Instruction::FunctionCall(ident, params) => {
                save_func_call(file, &ident, &params)?;
            },
        }
    }
    Ok(())
}

fn main() -> rustyline::Result<()> {
    welcome();
    let mut rl = DefaultEditor::new()?;
    let mut functions = HashMap::<String, Func>::new();
    loop {
        let mut input = match rl.readline("-> ") {
            Ok(line) => {
                match rl.add_history_entry(line.as_str()) {
                    Err(err) => println!("[Info]: Can't add entry to history '{err}'"),
                    Ok(_) => {},
                }
                line
            },
            Err(err) => {
                println!("{err}");
                break;
            }
        };
        let command = match input.split_ascii_whitespace().next() {
            Some(command) => command,
            None => continue,
        };
        let mut args = input.split_ascii_whitespace().skip(1);
        match command {
            "exit" => {
                if let Some(arg) = args.next() {
                    println!("[Error]: Unknown argument '{arg}' for command 'exit'");
                    continue;
                }
                break
            },
            "list" => {
                if let Some(arg) = args.next() {
                    println!("[Error]: Unknown argument '{arg}' for command 'list'");
                    continue;
                }
                print_functions(&functions);
                continue;
            },
            "help" => {
                if let Some(arg) = args.next() {
                    println!("[Error]: Unknown argument '{arg}' for command 'help'");
                    continue;
                }
                usage();
                continue;
            },
            "load" => {
                let path = match args.next() {
                    Some(arg) => arg,
                    None => {
                        println!("[Error]: Missing argument 'path' for command 'load'");
                        continue;
                    },
                };
                if let Some(arg) = args.next() {
                    println!("[Error]: Unknown argument '{arg}' for command 'load'");
                    continue;
                }
                let funcs = match load_file(path) {
                    Some(functions) => functions,
                    None => continue,
                };
                let mut count = 0;
                for func in &funcs {
                    if functions.contains_key(&func.ident) {
                        count += 1;
                    }
                }
                if count > 0 {
                    match rl.readline(format!("Loading of this file will redefine some functions ({count}). Do you want to continue? (y/n): ").as_str()) {
                        Ok(res) => match res.to_lowercase().as_str() {
                            "y" => {
                                for func in &funcs {
                                    functions.insert(func.ident.clone(), func.clone());
                                }
                                println!("  Successfully redefined");
                            },
                            _ => {
                                println!("  Cancelled");
                                continue;
                            }
                        },
                        Err(err) => {
                            println!("{err}");
                            break;
                        }
                    }
                }
                for func in funcs {
                    functions.insert(func.ident.clone(), func);
                }
                continue;
            },
            "save" => {
                let path = match args.next() {
                    Some(arg) => arg,
                    None => {
                        println!("[Error]: Missing argument 'path' for command 'save'");
                        continue;
                    },
                };
                if let Some(arg) = args.next() {
                    println!("[Error]: Unknown argument '{arg}' for command 'save'");
                    continue;
                }
                if let Err(err) = save_file(&functions, &path) {
                    println!("[Error]: Can't save functions in a file '{path}': {err}");
                    continue;
                }
                continue;
            },
            _ => {},
        }
        input.push('\n');

        let tokens = match parse(&input) {
            Ok(tokens) => { tokens },
            Err((pos, err))   => {
                print_err(&input, &format!("Unknown symbol in word '{err}'"), pos);
                continue;
            },
        };

        let is_def = tokens.len() > 0 && tokens[0].ttype == TokenType::Keyword(Keyword::Def);
        let mut expr = Ok(vec![]);
        if is_def {
            match create_function(&tokens) {
                Ok(func) => {
                    let ident = func.ident.clone();
                    if functions.contains_key(&ident) {
                        match rl.readline(format!("Function '{ident}' already exist, want to redefine? (y/n): ").as_str()) {
                            Ok(res) => match res.to_lowercase().as_str() {
                                "y" => {
                                    functions.insert(ident.clone(), func.clone());
                                    println!("  Successfully redefined");
                                },
                                _ => {
                                    println!("  Cancelled");
                                }
                            },
                            Err(err) => {
                                println!("{err}");
                                break;
                            }
                        }
                        continue;
                    }
                    functions.insert(ident.clone(), func.clone());
                    println!("  Successfully defined");
                },
                Err(err) => expr = Err(err),
            }
        } else {
            expr = create_expr(&tokens, &vec![]);
        }

        match expr {
            Ok(instructions) => {
                if !is_def {
                    let main = Func {
                        ident: "main".to_string(),
                        args: vec![],
                        expr: instructions,
                    };
                    match evaluate(&functions, &main, &vec![]) {
                        Ok(res) => println!(" = {res}"),
                        Err(err) => eval_err(err),
                    }
                }
            },
            Err(err) => syntax_err(&input, err),
        }
    }
    Ok(())
}
