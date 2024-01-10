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
        '+', '-', '*', '/', '^', '(', ')',
        // symbols
         ',', '=',
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

fn def_check(tokens: &[Token]) -> Result<(), SyntaxErr> {
    assert!(tokens.len() > 0);
    assert!(tokens[0].ttype == TokenType::Keyword(Keyword::Def));
    let def = &tokens[0];
    match tokens.get(1) {
        Some(Token {ttype: TokenType::Ident(_), ..}) => {},
        _ =>  return Err(SyntaxErr::MissingIdent(def.clone())),
    }
    match tokens.get(2) {
        Some(Token {ttype: TokenType::Operation(Operation::LeftParen), ..}) => {},
        _ =>  return Err(SyntaxErr::MissingLeftParen(tokens[1].clone())),
    }
    let mut i = 3;
    if i >= tokens.len() {
        return Err(SyntaxErr::MissingIdent(tokens[i-1].clone()));
    }
    loop {
        let token = match tokens.get(i) {
            None => return Err(SyntaxErr::MissingRightParen(tokens[i-1].clone())),
            Some(token) => token,
        };
        if token.ttype == TokenType::Operation(Operation::RightParen)
            && tokens[i-1].ttype != TokenType::Symbol(Symbol::Comma) {
            break;
        }
        match token.ttype {
            TokenType::Ident(_) => {},
            _ => return Err(SyntaxErr::MissingIdent(tokens[i-1].clone())),
        }
        i += 1;
        if i >= tokens.len() {
            return Err(SyntaxErr::MissingComma(tokens[i-1].clone()));
        }
        match tokens[i].ttype {
            TokenType::Symbol(Symbol::Comma) => {i += 1},
            TokenType::Operation(Operation::RightParen) => break,
            _ => return Err(SyntaxErr::MissingComma(tokens[i-1].clone())),
        }
    }
    i += 1;
    match tokens.get(i) {
        Some(Token {ttype: TokenType::Symbol(Symbol::Assign), ..}) => {},
        _ => return Err(SyntaxErr::MissingAssign(tokens[i-1].clone())),
    }
    if i+1 >= tokens.len() {
        return Err(SyntaxErr::MissingFuncBody(tokens[i].clone()));
    }
    expr_check(&tokens[i+1..])
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

fn expr_check(tokens: &[Token]) -> Result<(), SyntaxErr>{
    let mut parens = vec![];
    for (i, Token {ttype, ..}) in tokens.iter().enumerate() {
        match ttype {
            TokenType::Operation(Operation::Add) |
            TokenType::Operation(Operation::Sub) |
            TokenType::Operation(Operation::Mul) |
            TokenType::Operation(Operation::Div) |
            TokenType::Operation(Operation::Pow) => {
                if let Err(token) = binary_op_check(i, tokens) {
                    return Err(SyntaxErr::MissingArg(token));
                }
            },
            TokenType::Operation(Operation::UnaryPlus) |
            TokenType::Operation(Operation::UnaryMinus) => {
                if i == tokens.len()-1 || !is_number(&tokens[i+1]) {
                    unreachable!("Probably error in parse()");
                }
            },
            TokenType::Operation(Operation::LeftParen) => {
                if i != 0 {
                    match tokens[i-1].ttype {
                        TokenType::Operation(Operation::RightParen) |
                        TokenType::Number(_) => return Err(SyntaxErr::MissingOp(tokens[i-1].clone(), tokens[i].clone())),
                        _ => {},
                    }
                }
                if i == tokens.len()-1 {
                    return Err(SyntaxErr::MissingRightParen(tokens[i].clone()));
                }
                match tokens[i+1].ttype {
                    TokenType::Operation(Operation::UnaryPlus)  |
                    TokenType::Operation(Operation::UnaryMinus) |
                    TokenType::Operation(Operation::LeftParen)  |
                    TokenType::Operation(Operation::RightParen) |
                    TokenType::Ident(_)                         |
                    TokenType::Number(_) => {},
                    // TokenType::Operation(Operation::RightParen) => return Err(SyntaxErr::EmptyParens(tokens[i].clone())),
                    _ => return Err(SyntaxErr::MissingArg(tokens[i+1].clone())),
                }
                parens.push(&tokens[i]);
            },
            TokenType::Operation(Operation::RightParen) => {
                if i == 0 {
                    return Err(SyntaxErr::MissingLeftParen(tokens[i].clone()));
                }
                match tokens[i-1].ttype {
                    TokenType::Operation(Operation::RightParen) |
                    TokenType::Operation(Operation::LeftParen) |
                    TokenType::Number(_) => {},
                    _ => return Err(SyntaxErr::MissingArg(tokens[i-1].clone())),
                }
                if i < tokens.len()-1 {
                    match tokens[i+1].ttype {
                        TokenType::Operation(Operation::Add)        |
                        TokenType::Operation(Operation::Sub)        |
                        TokenType::Operation(Operation::Div)        |
                        TokenType::Operation(Operation::Mul)        |
                        TokenType::Operation(Operation::RightParen) |
                        TokenType::Symbol(Symbol::Comma) => {},
                        _ => return Err(SyntaxErr::MissingOp(tokens[i].clone(), tokens[i+1].clone())),
                    }
                }
                if parens.len() > 0 {
                    parens.pop();
                } else {
                    return Err(SyntaxErr::MissingLeftParen(tokens[i].clone()));
                }
            },
            TokenType::Number(_) => {
                if i < tokens.len()-1 {
                    match tokens[i+1].ttype {
                        TokenType::Number(_) |
                        TokenType::Ident(_)  |
                        TokenType::Operation(Operation::LeftParen) => return Err(SyntaxErr::MissingOp(tokens[i].clone(), tokens[i+1].clone())),
                        _ => {},
                    }
                }
            },
            TokenType::Symbol(Symbol::Comma) => {
                // )|num , ident|num
                if i == 0 {
                    return Err(SyntaxErr::UnexpectedToken(tokens[i].clone()));
                }
                match tokens[i-1].ttype {
                    TokenType::Operation(Operation::RightParen) |
                    TokenType::Number(_) => {},
                    _ => return Err(SyntaxErr::MissingArg(tokens[i].clone())),
                }
                if i == tokens.len()-1 {
                    return Err(SyntaxErr::UnexpectedToken(tokens[i].clone()));
                }
                match tokens[i+1].ttype {
                    TokenType::Ident(_) | TokenType::Number(_) => {},
                    _ => return Err(SyntaxErr::MissingArg(tokens[i].clone())),
                }
            },
            TokenType::Symbol(Symbol::Assign) => {
                return Err(SyntaxErr::UnexpectedToken(tokens[i].clone()));
            },
            TokenType::Keyword(Keyword::Def) => {
                return Err(SyntaxErr::DefInsideOfExpr(tokens[i].clone()));
            },
            TokenType::Ident(_) => {
                if i < tokens.len()-1 {
                    match tokens[i+1].ttype {
                        TokenType::Number(_) |
                        TokenType::Ident(_)  => return Err(SyntaxErr::MissingOp(tokens[i].clone(), tokens[i+1].clone())),
                        _ => {},
                    }
                }
            },
        }
    }
    if parens.len() > 0 {
        return Err(SyntaxErr::MissingRightParen(parens[0].clone()));
    }
    Ok(())
}

fn syntax_check(tokens: &[Token]) -> Result<(), SyntaxErr>{
    assert!(tokens.len() > 0);
    match tokens[0].ttype {
        TokenType::Keyword(Keyword::Def) => def_check(tokens),
        _ => expr_check(tokens),
    }
}

fn semantic_check(expr: &[Token], args: &[String]) -> Result<(), String> {
    let mut func_call = false;
    'outer: for (i, Token {ttype, ..}) in expr.iter().enumerate() {
        match ttype {
            TokenType::Ident(ident) => {
                for arg in args {
                    if ident == arg {
                        continue 'outer;
                    }
                }
                func_call = true;
                if i == expr.len()-1 {
                    return Err("Missing left paren".to_string());
                }
                match expr[i+1].ttype {
                    TokenType::Operation(Operation::LeftParen) => {},
                    _ => return Err("Missing left paren".to_string()),
                }
            },
            TokenType::Operation(Operation::RightParen) => {
                match expr[i-1].ttype {
                    TokenType::Operation(Operation::LeftParen) => {
                        if !func_call {
                            return Err("Empty parentheses".to_string());
                        }
                    },
                    _ => {},
                }
            },
            _ => {},
        }
    }
    Ok(())
}

#[derive(Debug)]
struct Func {
    args: Vec<String>,
    expr: Vec<Token>,
}

fn add_function(functions: &mut HashMap<String, Func>, tokens: &[Token]) -> bool {
    assert!(tokens.len() >= 2);
    let ident = match &tokens[1].ttype {
        TokenType::Ident(ident) => ident,
        _ => unreachable!("Error in syntax checking"),
    };
    if functions.contains_key(ident) {
        return false;
    }
    let mut i = 3;
    let mut args = vec![];
    loop {
        match tokens.get(i) {
            Some(token) => match &token.ttype {
                TokenType::Ident(ident) => args.push(ident.clone()),
                TokenType::Operation(Operation::RightParen) => {
                    i += 1;
                    break;
                },
                _ => unreachable!("Error in syntax checking"),
            },
            None => unreachable!("Error in syntax checking"),
        }
        match tokens.get(i+1) {
            Some(token) => match token.ttype {
                TokenType::Operation(Operation::RightParen) => {
                    i += 2;
                    break
                },
                TokenType::Symbol(Symbol::Comma) => {},
                _ => unreachable!("Error in syntax checking"),
            },
            None => unreachable!("Error in syntax checking"),
        }
        i += 2;
    }
    match tokens.get(i) {
        Some(Token {ttype: TokenType::Symbol(Symbol::Assign), ..}) => {},
        _ => unreachable!("Error in syntax checking"),
    }
    assert!(i+1 < tokens.len());
    let expr = tokens[i+1..].to_vec();
    functions.insert(ident.clone(), Func { args, expr });
    true
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

fn eval_func(functions: &HashMap<String, Func>, func: &Func, args: &Vec<f64>) -> f64 {
    let mut expr = func.expr.clone();
    for i in 0..expr.len() {
        match expr[i].ttype.clone() {
            TokenType::Ident(ident) => {
                for (j, arg) in func.args.iter().enumerate() {
                    if *arg == ident {
                        expr[i] = Token {
                            ttype: TokenType::Number(args[j]),
                            pos: 0,
                        };
                    }
                }
            },
            _ => {},
        }
    }
    evaluate(&expr, &functions)
}

fn evaluate(tokens: &[Token], functions: &HashMap<String, Func>) -> f64 {
    let mut numbers = vec![];
    let mut operations = vec![];
    let mut i = 0;
    while i < tokens.len() {
        let token = &tokens[i];
        match &token.ttype {
            TokenType::Number(num) => numbers.push(*num),
            TokenType::Operation(Operation::LeftParen) => operations.push(Operation::LeftParen),
            TokenType::Operation(Operation::RightParen) => {
                while let Some(last) = operations.pop() {
                    if last == Operation::LeftParen {
                        break;
                    }
                    apply_op(&mut numbers, last);
                }
            },
            TokenType::Operation(op) => {
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
            TokenType::Ident(ident) => {
                if !functions.contains_key(ident) {
                    todo!("Properly report error");
                }
                let mut args = vec![];
                let mut j = i + 2;
                let mut parens = 0;
                'outer: while parens != 0 || tokens[j].ttype != TokenType::Operation(Operation::RightParen) {
                    match tokens[j].ttype {
                        TokenType::Operation(Operation::LeftParen) => parens += 1,
                        TokenType::Operation(Operation::RightParen) => parens -= 1,
                        _ => {},
                    }
                    let mut comma_pos = j;
                    let mut parens = 1;
                    while !(tokens[comma_pos].ttype == TokenType::Symbol(Symbol::Comma) && parens == 1) {
                        match tokens[comma_pos].ttype {
                            TokenType::Operation(Operation::LeftParen) => parens += 1,
                            TokenType::Operation(Operation::RightParen) => parens -= 1,
                            _ => {},
                        }
                        if tokens[comma_pos].ttype == TokenType::Operation(Operation::RightParen) && parens == 0 {
                            args.push(evaluate(&tokens[j..comma_pos+1], functions));
                            j = comma_pos + 1;
                            break 'outer;
                        }
                        comma_pos += 1;
                    }
                    args.push(evaluate(&tokens[j..comma_pos], functions));
                    j = comma_pos + 1;
                }
                numbers.push(eval_func(&functions, &functions[ident], &args));
                i = j + 1;
            },
            TokenType::Symbol(Symbol::Comma) => unreachable!(),
            TokenType::Symbol(Symbol::Assign) => unreachable!(),
            TokenType::Keyword(Keyword::Def) => unreachable!(),
        }
        i += 1;
    }
    while let Some(last) = operations.pop() {
        apply_op(&mut numbers, last);
    }
    assert!(numbers.len() == 1);
    return numbers.pop().unwrap();
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
    let mut functions = HashMap::new();
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

        match syntax_check(&tokens) {
            Ok(()) => {},
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

        // TODO: Properly handle adding the function
        if tokens[0].ttype == TokenType::Keyword(Keyword::Def) {
            if !(add_function(&mut functions, &tokens)) {
                print_err(&input, &format!("Function with the name '{:?}' already exists", tokens[1].ttype), tokens[1].pos);
                continue;
            }
            let name = match &tokens[1].ttype {
                TokenType::Ident(ident) => ident,
                _ => unreachable!(),
            };
            let func = functions.get(name).unwrap();
            match semantic_check(&func.expr, &func.args) {
                Err(err) => { 
                    println!("[Error]: {err}");
                    continue;
                },
                Ok(()) => {},
            }
            println!("[Info]: list of functions");
            println!("{functions:?}");
        } else {
            let main = Func {
                expr: tokens,
                args: vec![],
            };
            let answer = eval_func(&functions, &main, &vec![]);
            println!("=> {}", answer);
        }
    }
    Ok(())
}
