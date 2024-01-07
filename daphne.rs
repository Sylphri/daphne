use std::io;
use std::io::Write;

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

fn parse(input: &str) -> Result<Vec<Token>, ParseErr> {
    let operators = &['+', '-', '*', '/'];
    let mut tokens = vec![];
    let mut begin = 0;
    while begin < input.len() && input.as_bytes()[begin].is_ascii_whitespace() {
        begin += 1;
    }
    let mut input = &input[begin..];
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
            match word.as_bytes()[i] {
                b'+' => {
                    if is_unary(i, &tokens, &word) {
                        tokens.push(Token {
                            ttype: TokenType::Operation(OpType::UnaryPlus),
                            pos: begin+i,
                        });
                    } else {
                        tokens.push(Token {
                            ttype: TokenType::Operation(OpType::Add),
                            pos: begin+i,
                        });
                    }
                },
                b'-' => {
                    if is_unary(i, &tokens, &word) {
                        tokens.push(Token {
                            ttype: TokenType::Operation(OpType::UnaryMinus),
                            pos: begin+i,
                        });
                    } else {
                        tokens.push(Token {
                            ttype: TokenType::Operation(OpType::Sub),
                            pos: begin+i,
                        });
                    }
                },
                b'*' => {
                    tokens.push(Token {
                        ttype: TokenType::Operation(OpType::Mul),
                        pos: begin+i,
                    });
                },
                b'/' => {
                    tokens.push(Token {
                        ttype: TokenType::Operation(OpType::Div),
                        pos: begin+i,
                    });
                },
                _ => unreachable!(),
            };
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
        let mut offset = 0;
        while offset < input.len() && input.as_bytes()[offset].is_ascii_whitespace() {
            offset += 1;
        }
        if offset == 0 {
            break;
        }
        input = &input[offset..];
        begin += offset;
    }
    Ok(tokens)
}

#[derive(Debug)]
enum SyntaxErr {
    None,
    MissingArg(Token),
}

fn binary_op_check(pos: usize, tokens: &Vec<Token>) -> SyntaxErr {
    if pos == 0 || pos == tokens.len()-1 
        || is_operation(&tokens[pos-1]) {
        return SyntaxErr::MissingArg(tokens[pos].clone());
    } else {
        match tokens[pos+1].ttype {
            TokenType::Operation(OpType::UnaryPlus) => {},
            TokenType::Operation(OpType::UnaryMinus) => {},
            TokenType::Number(_) => {},
            _ => return SyntaxErr::MissingArg(tokens[pos].clone()),
        }
    }
    SyntaxErr::None
}

fn syntax_check(tokens: &Vec<Token>) -> SyntaxErr {
    for (i, Token {ttype, ..}) in tokens.iter().enumerate() {
        match ttype {
            TokenType::Operation(OpType::Add) |
            TokenType::Operation(OpType::Sub) |
            TokenType::Operation(OpType::Mul) |
            TokenType::Operation(OpType::Div) => {
                match binary_op_check(i, tokens) {
                    SyntaxErr::MissingArg(pos) => return SyntaxErr::MissingArg(pos),
                    SyntaxErr::None => {},
                };
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
            TokenType::Number(_) => {},
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

fn main() {
    loop {
        print!("> ");
        io::stdout().flush().unwrap();
        let mut input = String::new();
        io::stdin()
            .read_line(&mut input)
            .expect("Failed to read line");

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
                    // TODO: Properly handle long expressions
                    ParseErr::UnknownToken(token) => {
                        println!("[Error]: Unknown character in token '{}'", token);
                        print!(" ::  {}", input);
                        let pos = input.find(&token).expect("Unknown token must be in input");
                        let mut pt = vec![b'-'; input.len()-1];
                        pt[pos] = b'^';
                        println!(" ::  {}", String::from_utf8(pt).expect("Error in pointer string"));
                    }
                }
                continue;
            },
        };

        match syntax_check(&tokens) {
            SyntaxErr::None => {},
            SyntaxErr::MissingArg(token) => {
                println!("[Error]: Missing argument for '{:?}'", token.ttype);
                print!(" ::  {}", input);
                let mut pt = vec![b'-'; input.len()-1];
                pt[token.pos] = b'^';
                println!(" ::  {}", String::from_utf8(pt).expect("Error in pointer string"));
                continue;
            },
        }

        let answer = evaluate(&tokens);
        println!("=> {}", answer);
    }
}
