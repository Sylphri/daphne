use std::io;
use std::io::Write;

#[derive(Debug)]
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
    tokens.len() > 0 && pos+1 < word.len()
    && is_operation(&tokens[tokens.len()-1])
    && is_numeric(word.as_bytes()[pos+1])
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

fn main() {
    loop {
        print!("> ");
        io::stdout().flush().unwrap();
        let mut input = String::new();
        io::stdin()
            .read_line(&mut input)
            .expect("Failed to read line");

        match parse(&input) {
            Ok(tokens) => println!("{:?}", tokens),
            Err(err)   => println!("{:?}", err),
        }
    }
}
