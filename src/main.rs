mod ast_printer;
mod callable;
mod class;
mod error;
mod expr;
mod interpreter;
mod lexer;
mod object;
mod parser;
mod resolver;
mod stmt;
mod token;
mod token_kind;

use core::panic;
use std::env::args;
use std::fs::File;
use std::io::Read;
use std::process::exit;

use crate::lexer::Lexer;
use crate::token::Token;

use ast_printer::AstPrinter;
use error::LoxError::{
    Lexer as LexerError, Parser as ParserError, Resolver as ResolverError, Runtime,
};
use error::LoxResult;

use interpreter::Interpreter;
use parser::Parser;
use resolver::Resolver;
use rustyline::DefaultEditor;

fn run(source: &str, interpreter: &mut Interpreter) -> LoxResult<()> {
    let mut lexer = Lexer::new(source);
    let tokens: &Vec<Token> = match lexer.scan_tokens() {
        Ok(vec) => vec,
        Err(e) => return Err(LexerError(e)),
    };

    let stmts = match Parser::new(tokens.to_vec()).parse() {
        Ok(stmts) => stmts,
        Err(e) => return Err(ParserError(e)),
    };

    println!("{}", AstPrinter::new().print(&stmts));

    let mut resolver = Resolver::new(interpreter);
    match resolver.resolve_stmts(&stmts) {
        Ok(()) => {}
        Err(e) => return Err(ResolverError(e)),
    }

    match interpreter.interpret(&stmts) {
        Ok(()) => {}
        Err(e) => return Err(Runtime(e)),
    };

    Ok(())
}

fn run_file(fpath: &str) {
    let mut file = File::open(fpath).unwrap_or_else(|error| {
        panic!("Error opening file: {error:?}");
    });

    let mut str = String::new();
    _ = file.read_to_string(&mut str).unwrap_or_else(|error| {
        panic!("Error reading from file: {error:?}");
    });

    let mut interpreter = Interpreter::new();
    // Kinda looks weird
    match run(&str, &mut interpreter) {
        Ok(()) => (),
        Err(e) => eprintln!("{e}"),
    }
}

fn run_prompt() {
    let mut rl = DefaultEditor::new().unwrap_or_else(|e| {
        panic!("Error starting Rustyline: {e:?}");
    });

    let mut interpreter = Interpreter::new();

    while let Ok(line) = rl.readline("Lox > ") {
        println!();
        _ = rl.add_history_entry(line.as_str());

        if line.is_empty() {
            continue;
        }

        match run(line.as_str(), &mut interpreter) {
            Ok(()) => (),
            Err(e) => eprintln!("{e}"),
        }
    }
}

fn main() {
    let args: Vec<String> = args().collect();

    match args.len() {
        1 => run_prompt(),
        2 => run_file(&args[1]),
        _ => {
            eprintln!("[ * ] Usage: loxrs <script.lox>");
            exit(64);
        }
    }
}
