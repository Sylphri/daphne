use std::io;
use std::io::Write;

#[derive(Debug)]
enum OpType {
    Plus,
}

#[derive(Debug)]
enum Token {
    Number(f64),
    Operation(OpType),
}

#[derive(Debug)]
enum ParseError {
    UnknownSym(char),
}

// TODO: first split by whitespaces then by operators
fn parse(input: &str) -> Result<Vec<Token>, ParseError> {
    let mut tokens = vec![];
    let mut last = String::new();
    for ch in input.chars() {
        match ch {
            '+' => {
                if last.len() > 0 {
                    tokens.push(Token::Number(
                        last.parse().expect("Unreachable")
                    ));
                    last.clear();
                }
                tokens.push(Token::Operation(OpType::Plus));
            },
            ' ' | '\n' => {
                if last.len() > 0 {
                    tokens.push(Token::Number(
                        last.parse().expect("Unreachable")
                    ));
                    last.clear();
                }
            },
            '0'..='9' | '.' => {
                last.push(ch);
            },
            _ => return Err(ParseError::UnknownSym(ch)),
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
