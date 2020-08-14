#![feature(type_ascription, pattern)] // For self-documenting code.
#![feature(core_intrinsics)]          // For `discriminent_value()`

// nom's type inference requirements.
// See https://github.com/rust-lang/rust/issues/54540
#![type_length_limit="4180863"]
#![allow(dead_code)]

#[macro_use] extern crate prettytable;
#[macro_use] extern crate lazy_static;
extern crate strum;
#[macro_use] extern crate strum_macros;
extern crate nom;
extern crate bimap;

mod wvm;
mod cell;
mod address;
mod functor;
mod bytecode;
mod compiler;


use crate::wvm::WVM;
use crate::compiler::Compilation;

fn main() {

  #[cfg(feature = "trace_computation")]
  println!("Computation Tracing ENABLED");

  /*
    It is important in M_2 that no two facts have the same head atom. Otherwise, the failure of
    the first version of the atom will have changed the registers because of its partial
    unification, and the matching program will not be matching against the original query but
    rather a query partially unified with some other program.
  */
  let text = "\t?-p(Z,h(Z,W),f(W)), q(Z, W).
  \tb(X, P):-j(f(Y), r(X), f(P, Q)).
  \ty(f(Y), r(X), f(P, Q)).
  \tp(f(X), h(Y, f(a)), Y).";
  println!("Input Expression: \n{}\n", text);

  /*
  // High level WVM:
  let result = WVM::from_source(text);
  match result{
    Some(mut machine) => {
      machine.run();
    }
    None => { *//**//* }
  }
  */

  // Parser:
  let result = compiler::parse(text);
  match result {

    Ok((programs, query)) => {
      println!("\nQuery:\n");
      if let Some(q) = query {
        println!("{}", q);
      }
      println!("Programs:\n");
      for p in programs{
        println!("{}", p);
      }
    }

    Err(_) => {}

  }

  /*
  // Compiler:
  match Compilation::compile(text, true) {
    Some(mut compilation) => {
      let mut machine = WVM::from_compilation(&mut compilation);
      machine.run();
    },
    None => {
      // The errors should have already been printed.
      println!("Failed to compile.");
    }
  };
  */
  // Assembly:
  /*
  let path = "/Users/rljacobson/Development/wam/ignore/assembly.txt";
  let assembly = std::fs::read_to_string(path).unwrap();
  // println!("File contents: \n{}\n", assembly);
  let mut machine = WVM::new();
  machine.compile_assembly(assembly.as_str(), true);
  */


}
