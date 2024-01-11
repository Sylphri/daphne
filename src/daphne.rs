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

#[derive(Debug)]
enum ParseErr {
    UnknownWord(usize),
}

fn is_operation(token: &Token) -> bool {
    match token.ttype {
        TokenType::Operation(_) => true,
        _ => false,
    }
}

fn is_number(token: &Token) -> bool {
    match token.ttype {
        TokenType::Number(_) => true,
        _ => false,
    }
}

fn is_numeric(ch: char) -> bool {
    ('0'..='9').contains(&ch) || ch == '.'
}

fn is_latin_letter(ch: char) -> bool {
    ('a'..='z').contains(&ch) || ('A'..='Z').contains(&ch)
}

fn is_unary(pos: usize, tokens: &Vec<Token>, word: &str) -> bool {
    let ch = match word.chars().nth(pos+1) {
        Some(ch) => ch,
        None => return false,
    };
    (tokens.len() == 0 || is_operation(&tokens[tokens.len()-1]))
        && (is_numeric(ch) || is_latin_letter(ch))
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
        if is_latin_letter(c) { continue; }
        return false;
    }
    true
}

fn parse_word(word: &str) -> Option<TokenType> {
    if let Some(keyword) = parse_keyword(word) {
        return Some(TokenType::Keyword(keyword));
    }
    if is_valid_ident(word) {
        return Some(TokenType::Ident(word.to_string()));
    }
    if let Ok(num) = word.parse() {
        return Some(TokenType::Number(num));
    }
    None
}

fn parse(input: &str) -> Result<Vec<Token>, ParseErr> {
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
                match parse_word(&word[0..i]) {
                    Some(ttype) => tokens.push(Token {
                        ttype: ttype,
                        pos: begin,
                    }),
                    None => return Err(ParseErr::UnknownWord(begin)),
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
                None => return Err(ParseErr::UnknownWord(begin)),
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
    MissingIdent(Token),
    MissingComma(Token),
    MissingAssign(Token),
    MissingFuncBody(Token),
    UnexpectedToken(Token),
    DefInsideOfExpr(Token),
}

fn create_function(tokens: &[Token]) -> Result<(String, Func), SyntaxErr> {
    assert!(tokens.len() > 0);
    let mut pos = 0;
    assert!(tokens[pos].ttype == TokenType::Keyword(Keyword::Def));
    let def = &tokens[pos];
    pos += 1;
    let ident = match &tokens.get(pos) {
        Some(Token {ttype: TokenType::Ident(ident), ..}) => ident,
        _ =>  return Err(SyntaxErr::MissingIdent(def.clone())),
    };
    pos += 1;
    match tokens.get(pos) {
        Some(Token {ttype: TokenType::Operation(Operation::LeftParen), ..}) => {},
        _ =>  return Err(SyntaxErr::MissingLeftParen(tokens[pos].clone())),
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
                args.push(arg.clone());
            },
            _ => return Err(SyntaxErr::MissingIdent(tokens[pos-1].clone())),
        }
        pos += 1;
        if pos >= tokens.len() {
            return Err(SyntaxErr::MissingComma(tokens[pos-1].clone()));
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
        return Err(SyntaxErr::MissingFuncBody(tokens[pos].clone()));
    }
    if let Err(err) = expr_check(&tokens[pos..], &args) {
        return Err(err);
    }
    let expr = match expr_check(&tokens[pos..], &args) {
        Ok(instructions) => instructions,
        Err(err) => return Err(err),
    };
    Ok((ident.clone(), Func {
        ident: ident.clone(),
        args: (*args).to_vec(),
        expr: expr,
    }))
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
    LeftParen,
    RightParen,
    PushArg(String),
    FunctionCall(String, Vec<Vec<Instruction>>),
}

fn expr_check(tokens: &[Token], args: &[String]) -> Result<Vec<Instruction>, SyntaxErr> {
    let mut parens = vec![];
    let mut instructions = vec![];
    let mut i = 0;
    'outer: while i < tokens.len() {
        let ttype = &tokens[i].ttype;
        match ttype {
            // )|num|ident bin unary|(|num|ident
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
                if i == tokens.len()-1 || !is_number(&tokens[i+1]) {
                    unreachable!("Probably error in parse()");
                }
                instructions.push(Instruction::PushOp(op.clone()));
            },
            // op|lparen|none lparen lparen|num|ident|unary
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
                instructions.push(Instruction::LeftParen);
            },
            // num|arg|rparen rparen op|rparen|none
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
                instructions.push(Instruction::RightParen);
            },
            // op|lparen|none num !unary|rparen|none
            TokenType::Number(num) => {
                if i != 0 {
                    match tokens[i-1].ttype {
                        TokenType::Operation(Operation::RightParen) => {
                            return Err(SyntaxErr::MissingOp(tokens[i-1].clone(), tokens[i].clone()));
                        }
                        TokenType::Operation(_) => {},
                        _ => return Err(SyntaxErr::MissingOp(tokens[i-1].clone(), tokens[i].clone())),
                    }
                }
                if i < tokens.len()-1 {
                    match tokens[i+1].ttype {
                        TokenType::Operation(Operation::Add) |
                        TokenType::Operation(Operation::Sub) |
                        TokenType::Operation(Operation::Mul) |
                        TokenType::Operation(Operation::Div) |
                        TokenType::Operation(Operation::Pow) |
                        TokenType::Operation(Operation::RightParen) => {},
                        _ => return Err(SyntaxErr::MissingOp(tokens[i].clone(), tokens[i+1].clone())),
                    }
                }
                instructions.push(Instruction::PushNumber(*num));
            },
            TokenType::Symbol(Symbol::Comma) => unreachable!(),
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
                            match expr_check(&tokens[last..pos], &args) {
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
                        match expr_check(&tokens[last..pos], &args) {
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
                return Err(SyntaxErr::UnexpectedToken(tokens[i].clone()));
            },
            TokenType::Keyword(Keyword::Def) => {
                return Err(SyntaxErr::DefInsideOfExpr(tokens[i].clone()));
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
    UnknownArg(String, String),
    UnknownFunction(String),
}

fn evaluate(functions: &HashMap<String, Func>, func: &Func, params: &Vec<f64>) -> Result<f64, EvalErr> {
    println!("{:?}", func.expr);
    let mut numbers = vec![];
    let mut operations = vec![];
    let mut i = 0;
    'outer: while i < func.expr.len() {
        let instr = &func.expr[i];
        match instr {
            Instruction::PushNumber(num) => numbers.push(*num),
            Instruction::LeftParen => operations.push(Operation::LeftParen),
            Instruction::RightParen => {
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
                return Err(EvalErr::UnknownArg(func.ident.clone(), arg.clone()));
            },
            Instruction::FunctionCall(ident, fparams) => {
                let fcall = match functions.get(ident) {
                    Some(func) => func,
                    None => return Err(EvalErr::UnknownFunction(ident.clone())),
                };
                if fcall.args.len() != fparams.len() {
                    return Err(EvalErr::ParamsCountDontMatch(fcall.ident.clone(), fcall.args.len(), params.len()));
                }
                let mut eval_params = vec![];
                for param in fparams {
                    let temp = Func {
                        ident: "temp".to_string(),
                        args: fcall.args.clone(),
                        expr: param.to_vec(),
                    };
                    match evaluate(&functions, &temp, &params) {
                        Ok(res) => eval_params.push(res),
                        Err(err) => return Err(err),
                    }
                }
                match evaluate(&functions, &fcall, &eval_params) {
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

const DEFAULT_TERM_WIDTH: u16 = 50;
const DEFAULT_TERM_HEIGHT: u16 = 50;

fn print_err(input: &str, message: &str, pos: usize) {
    let (terminal_size::Width(twidth), _) = match terminal_size() {
        Some(tsize) => tsize,
        None => {
            println!("[Error]: Can't get the width of the terminal, formatting may be screwed");
            (terminal_size::Width(DEFAULT_TERM_WIDTH),
             terminal_size::Height(DEFAULT_TERM_HEIGHT))
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

fn usage() {
    println!("Welcome to Daphne, a simple math shell.");
    println!("Usage:");
    println!("  > expr");
    println!();
    println!("Expr:");
    println!("  Mathematical expression which consist of <op> and <num>");
    println!("   <op>  - One of this basic operations: +, -, *, /, ^");
    println!("   <num> - Arbitrary real number");
    println!();
}

fn main() -> rustyline::Result<()> {
    usage();
    let mut rl = DefaultEditor::new()?;
    let mut functions = HashMap::<String, Func>::new();
    loop {
        let readline = rl.readline("> ");
        let mut input = match readline {
            Ok(line) => {
                match rl.add_history_entry(line.as_str()) {
                    Err(err) => println!("[Info]: Can't add entry to history '{err}"),
                    Ok(_) => {},
                }
                line
            },
            Err(err) => {
                println!("{err}");
                break;
            }
        };
        match input.trim() {
            "" => continue,
            "exit" => break,
            "funcs" => {
                for (ident, func) in functions.iter() {
                    print!("  {ident}(");
                    for arg in &func.args {
                        print!("{arg}, ");
                    }
                    println!(")");
                }
                continue;
            },
            "help" => {
                usage();
                continue;
            },
            _ => {},
        }
        input.push('\n');

        let tokens = match parse(&input) {
            Ok(tokens) => {
                println!("[Info]: [");
                for token in &tokens {
                    println!("    {:?},", token);
                }
                println!("]");
                println!("[Info]: {} tokens in total", tokens.len());
                tokens
            },
            Err(err)   => {
                match err {
                    ParseErr::UnknownWord(pos) => {
                        let end = input[pos..].find(|x: char| x.is_ascii_whitespace())
                            .expect("Input must contain at least \\n at the end");
                        let word = &input[pos..pos+end];
                        print_err(&input, &format!("Unknown word {word}"), pos);
                    }
                }
                continue;
            },
        };

        let is_def = tokens.len() > 0 && tokens[0].ttype == TokenType::Keyword(Keyword::Def);
        let mut syntax_err = Ok(vec![]);
        if is_def {
            match create_function(&tokens) {
                Ok((ident, func)) => {
                    if functions.contains_key(&ident) {
                        todo!("Properly report error");
                    }
                    functions.insert(ident.clone(), func.clone());
                    println!("[Info]: added function {ident}({args:?}) = ",args = func.args);
                    for instr in &func.expr {
                        println!("    {:?},", instr);
                    }
                },
                Err(err) => syntax_err = Err(err),
            }
        } else {
            syntax_err = expr_check(&tokens, &vec![]);
            if let Ok(ref instructions) = syntax_err {
                println!("[Info]: instructions");
                for instr in instructions {
                    println!("    {:?},", instr);
                }
                println!("[Info]: {} instructions in total", instructions.len());
            }
        }

        match syntax_err {
            Ok(instructions) => {
                if !is_def {
                    let main = Func {
                        ident: "main".to_string(),
                        args: vec![],
                        expr: instructions,
                    };
                    match evaluate(&functions, &main, &vec![]) {
                        Ok(res) => println!("=> {res}"),
                        Err(err) => println!("{:?}", err),
                    }
                }
            },
            Err(SyntaxErr::MissingArg(token)) => {
                print_err(&input, &format!("Missing argument for '{:?}'", token.ttype), token.pos);
                continue;
            },
            Err(SyntaxErr::MissingOp(a, b)) => {
                print_err(&input, &format!("Missing operation between '{:?}' and '{:?}'", a.ttype, b.ttype), b.pos-1);
                continue;
            },
            Err(SyntaxErr::MissingRightParen(token)) => {
                print_err(&input, &format!("Missing right paren"), token.pos);
                continue;
            },
            Err(SyntaxErr::MissingLeftParen(token)) => {
                print_err(&input, &format!("Missing left paren"), token.pos);
                continue;
            },
            Err(SyntaxErr::EmptyParens(token)) => {
                print_err(&input, &format!("Empty parentheses"), token.pos);
                continue;
            },
            Err(SyntaxErr::MissingIdent(token)) => {
                print_err(&input, &format!("Missing identifier in function declaration"), token.pos);
                continue;
            },
            Err(SyntaxErr::MissingComma(token)) => {
                print_err(&input, &format!("Missing comma in function declaration"), token.pos);
                continue;
            },
            Err(SyntaxErr::MissingAssign(token)) => {
                print_err(&input, &format!("Missing assign in function declaration"), token.pos);
                continue;
            },
            Err(SyntaxErr::MissingFuncBody(token)) => {
                print_err(&input, &format!("Missing body of function in declaration"), token.pos);
                continue;
            },
            Err(SyntaxErr::UnexpectedToken(token)) => {
                print_err(&input, &format!("Unexpected token '{:?}'", token.ttype), token.pos);
                continue;
            },
            Err(SyntaxErr::DefInsideOfExpr(token)) => {
                print_err(&input, &format!("'def' keyword inside of expression"), token.pos);
                continue;
            },
        }

    }
    Ok(())
}
