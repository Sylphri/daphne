use std::io;
use std::io::Write;

mod external;

use external::{TermSize, get_terminal_size};

#[derive(Debug, Clone)]
enum OpType {
    Add,
    Sub,
    Mul,
    Div,
    UnaryPlus,
    UnaryMinus,
}

#[derive(Debug, Clone)]
enum TokenType {
    Number(f64),
    Operation(OpType),
}

#[derive(Debug, Clone)]
struct Token {
    ttype: TokenType,
    pos: usize,
}

#[derive(Debug)]
enum ParseErr {
    UnknownToken(String),
}

fn is_operation(token: &Token) -> bool {
    match token.ttype {
        TokenType::Operation(_) => true,
        _ => false,
    }
}

fn is_numeric(byte: u8) -> bool {
    (b'0'..=b'9').contains(&byte) || byte == b'.'
}

fn is_unary(pos: usize, tokens: &Vec<Token>, word: &str) -> bool {
    (tokens.len() == 0 || is_operation(&tokens[tokens.len()-1]))
    && pos+1 < word.len() && is_numeric(word.as_bytes()[pos+1])
}

fn trim_start(line: &str) -> (&str, usize) {
    let mut begin = 0;
    while begin < line.len() && line.as_bytes()[begin].is_ascii_whitespace() {
        begin += 1;
    }
    (&line[begin..], begin)
}

fn parse(input: &str) -> Result<Vec<Token>, ParseErr> {
    let operators = &['+', '-', '*', '/'];
    let mut tokens = vec![];
    let (mut input, mut begin) = trim_start(input);
    while let Some(i) = input.find(|c: char| c.is_ascii_whitespace()) {
        let mut word = &input[0..i];
        input = &input[word.len()..];
        while let Some(i) = word.find(operators) {
            if i != 0 {
                match word[0..i].parse() {
                    Ok(num) => tokens.push(Token {
                            ttype: TokenType::Number(num),
                            pos: begin,
                        }),
                    Err(_) => return Err(ParseErr::UnknownToken(word[0..i].to_string())),
                };
            }
            tokens.push(Token {
                ttype: match word.as_bytes()[i] {
                    b'+' => {
                        if is_unary(i, &tokens, &word) {
                            TokenType::Operation(OpType::UnaryPlus)
                        } else {
                            TokenType::Operation(OpType::Add)
                        }
                    },
                    b'-' => {
                        if is_unary(i, &tokens, &word) {
                            TokenType::Operation(OpType::UnaryMinus)
                        } else {
                            TokenType::Operation(OpType::Sub)
                        }
                    },
                    b'*' => TokenType::Operation(OpType::Mul),
                    b'/' => TokenType::Operation(OpType::Div),
                    _ => unreachable!(),
                },
                pos: begin+i,
            });
            word = &word[i+1..];
            begin += i+1;
        }
        if word.len() > 0 {
            match word.parse() {
                Ok(num) => tokens.push(Token {
                    ttype: TokenType::Number(num),
                    pos: begin,
                }),
                Err(_) => return Err(ParseErr::UnknownToken(word.to_string())),
            };
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
    None,
    MissingArg(Token),
    MissingOp(Token, Token),
}

fn binary_op_check(pos: usize, tokens: &Vec<Token>) -> Result<(), Token> {
    if pos == 0 || pos == tokens.len()-1 
        || is_operation(&tokens[pos-1]) {
        return Err(tokens[pos].clone());
    } else {
        match tokens[pos+1].ttype {
            TokenType::Operation(OpType::UnaryPlus) => {},
            TokenType::Operation(OpType::UnaryMinus) => {},
            TokenType::Number(_) => {},
            _ => return Err(tokens[pos].clone()),
        }
    }
    Ok(())
}

fn syntax_check(tokens: &Vec<Token>) -> SyntaxErr {
    for (i, Token {ttype, ..}) in tokens.iter().enumerate() {
        match ttype {
            TokenType::Operation(OpType::Add) |
            TokenType::Operation(OpType::Sub) |
            TokenType::Operation(OpType::Mul) |
            TokenType::Operation(OpType::Div) => {
                if let Err(token) = binary_op_check(i, tokens) {
                    return SyntaxErr::MissingArg(token);
                }
            },
            TokenType::Operation(OpType::UnaryPlus) |
            TokenType::Operation(OpType::UnaryMinus) => {
                if i == tokens.len()-1 {
                    unreachable!("Probably error in parse()");
                } else {
                    match tokens[i+1].ttype {
                        TokenType::Number(_) => {},
                        _ => unreachable!("Probably error in parse()"),
                    }
                }
            },
            TokenType::Number(_) => {
                if i < tokens.len()-1 && !is_operation(&tokens[i+1]) {
                    return SyntaxErr::MissingOp(tokens[i].clone(), tokens[i+1].clone());
                }
            },
        }
    }
    SyntaxErr::None
}

fn op_priority(op: &OpType) -> u8 {
    match op {
        OpType::Add | OpType::Sub => 1,
        OpType::Mul | OpType::Div => 2,
        OpType::UnaryPlus | OpType::UnaryMinus => 3,
    }
}

fn apply_op(numbers: &mut Vec<f64>, op: OpType) {
    match op {
        OpType::Add => {
            assert!(numbers.len() >= 2);
            let a = numbers.pop().unwrap();
            let b = numbers.pop().unwrap();
            numbers.push(b + a);
        },
        OpType::Sub => {
            assert!(numbers.len() >= 2);
            let a = numbers.pop().unwrap();
            let b = numbers.pop().unwrap();
            numbers.push(b - a);
        },
        OpType::Mul => {
            assert!(numbers.len() >= 2);
            let a = numbers.pop().unwrap();
            let b = numbers.pop().unwrap();
            numbers.push(b * a);
        },
        OpType::Div => {
            assert!(numbers.len() >= 2);
            let a = numbers.pop().unwrap();
            let b = numbers.pop().unwrap();
            numbers.push(b / a);
        },
        OpType::UnaryPlus => { assert!(numbers.len() > 0) },
        OpType::UnaryMinus => {
            assert!(numbers.len() > 0);
            let a = numbers.pop().unwrap();
            numbers.push(-a);
        },
    }
}

fn evaluate(tokens: &Vec<Token>) -> f64 {
    let mut numbers = vec![];
    let mut operations = vec![];
    for token in tokens {
        match &token.ttype {
            TokenType::Number(num) => numbers.push(*num),
            TokenType::Operation(op) => {
                if let None = operations.last() {
                    operations.push(op.clone());
                } else {
                    while let Some(last) = operations.pop() {
                        if op_priority(&last) < op_priority(&op) {
                            operations.push(last);
                            break
                        }
                        apply_op(&mut numbers, last);
                    }
                    operations.push(op.clone());
                }
            }
        }
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
    let tsize = match get_terminal_size() {
        Some(tsize) => tsize,
        None => {
            println!("[Error]: Can't get the width of the terminal, formatting may be screwed");
            TermSize {
                width: DEFAULT_TERM_WIDTH,
                height: DEFAULT_TERM_HEIGHT,
            }
        },
    };
    println!("[Error]: {message}");
    if input.len() < tsize.width as usize - 6 {
        print!(" ::  {input}");
        let mut pt = vec![b'-'; input.len()-1];
        pt[pos] = b'^';
        println!(" ::  {}", String::from_utf8(pt).expect("Error in pointer string"));
    } else {
        let width = ((tsize.width-8)/2) as usize;
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

fn main() {
    loop {
        print!("> ");
        io::stdout().flush().unwrap();
        let mut input = String::new();
        io::stdin()
            .read_line(&mut input)
            .expect("Failed to read line");
        match input.trim() {
            "" => continue,
            "exit" => break,
            _ => {},
        }

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
                    ParseErr::UnknownToken(token) => {
                        let pos = input.find(&token).expect("Unknown token must be in input");
                        print_err(&input, &format!("Unknown character in token '{}'", token), pos);
                    }
                }
                continue;
            },
        };

        match syntax_check(&tokens) {
            SyntaxErr::None => {},
            SyntaxErr::MissingArg(token) => {
                print_err(&input, &format!("Missing argument for '{:?}'", token.ttype), token.pos);
                continue;
            },
            SyntaxErr::MissingOp(a, b) => {
                print_err(&input, &format!("Missing operation between '{:?}' and '{:?}'", a.ttype, b.ttype), b.pos-1);
                continue;
            }
        }

        let answer = evaluate(&tokens);
        println!("=> {}", answer);
    }
}
