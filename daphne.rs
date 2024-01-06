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

#[derive(Debug)]
enum Token {
    Number(f64),
    Operation(OpType),
}

#[derive(Debug)]
enum ParseError {
    UnknownToken(String),
}

fn is_operation(token: &Token) -> bool {
    match *token {
        Token::Operation(_) => true,
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

fn parse(input: &str) -> Result<Vec<Token>, ParseError> {
    let mut tokens = vec![];
    let operators = &['+', '-', '*', '/'];
    for mut word in input.split_ascii_whitespace() {
        while let Some(i) = word.find(operators) {
            if i != 0 {
                match word[0..i].parse() {
                    Ok(num) => tokens.push(Token::Number(num)),
                    Err(_) => return Err(ParseError::UnknownToken(word[0..i].to_string())),
                };
            }
            match word.as_bytes()[i] {
                b'+' => {
                    if is_unary(i, &tokens, &word) {
                        tokens.push(Token::Operation(OpType::UnaryPlus));
                    } else {
                        tokens.push(Token::Operation(OpType::Add));
                    }
                },
                b'-' => {
                    if is_unary(i, &tokens, &word) {
                        tokens.push(Token::Operation(OpType::UnaryMinus));
                    } else {
                        tokens.push(Token::Operation(OpType::Sub))
                    }
                },
                b'*' => { tokens.push(Token::Operation(OpType::Mul)) },
                b'/' => { tokens.push(Token::Operation(OpType::Div)) },
                _ => unreachable!(),
            };
            word = &word[i+1..];
        }
        if word.len() > 0 {
            match word.parse() {
                Ok(num) => tokens.push(Token::Number(num)),
                Err(_) => return Err(ParseError::UnknownToken(word.to_string())),
            };
        }
    }
    Ok(tokens)
}

#[derive(Debug)]
enum SyntaxErr {
    None,
    MissingArg(usize),
}

fn binary_op_check(pos: usize, tokens: &Vec<Token>) -> SyntaxErr {
    if pos == 0 || pos == tokens.len()-1 
        || is_operation(&tokens[pos-1]) {
        return SyntaxErr::MissingArg(pos);
    } else {
        match tokens[pos+1] {
            Token::Operation(OpType::UnaryPlus) => {},
            Token::Operation(OpType::UnaryMinus) => {},
            Token::Number(_) => {},
            _ => return SyntaxErr::MissingArg(pos),
        }
    }
    SyntaxErr::None
}

fn syntax_check(tokens: &Vec<Token>) -> SyntaxErr {
    for (i, token) in tokens.iter().enumerate() {
        match token {
            Token::Operation(OpType::Add) |
            Token::Operation(OpType::Sub) |
            Token::Operation(OpType::Mul) |
            Token::Operation(OpType::Div) => {
                match binary_op_check(i, tokens) {
                    SyntaxErr::MissingArg(pos) => return SyntaxErr::MissingArg(pos),
                    SyntaxErr::None => {},
                };
            },
            Token::Operation(OpType::UnaryPlus) |
            Token::Operation(OpType::UnaryMinus) => {
                if i == tokens.len()-1 {
                    unreachable!("Probably error in parse()");
                } else {
                    match tokens[i+1] {
                        Token::Number(_) => {},
                        _ => unreachable!("Probably error in parse()"),
                    }
                }
            },
            Token::Number(_) => {},
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
        match token {
            Token::Number(num) => numbers.push(*num),
            Token::Operation(op) => {
                if let None = operations.last() {
                    operations.push(op.clone());
                } else {
                    while let Some(last) = operations.pop() {
                        if op_priority(&last) <= op_priority(&op) {
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
                tokens
            },
            Err(err)   => {
                println!("[Error]: {:?}", err);
                continue;
            },
        };

        match syntax_check(&tokens) {
            SyntaxErr::None => {},
            err => {
                println!("[Error]: {:?}", err);
                continue;
            }
        }

        let answer = evaluate(&tokens);
        println!("=> {}", answer);
    }
}
