#![feature(type_ascription, pattern)]
#![allow(dead_code)]
// ToDo: Remove the above `allow` attribute.

#[macro_use] extern crate prettytable;
#[macro_use] extern crate lazy_static;

mod wzero;
mod cell;
mod address;
mod parser;
mod chariter;
mod term;
mod functor;

use crate::wzero::WZero;

fn main() {
  #[cfg(debug_print)]
  println!("debug_print ENABLED");

  let text = "?-p(Z,h(Z,W),f(W))";
  let ast = parser::parse(text);
  println!("Query Expression: {}", text);
  // println!("Parse Tree:\n{}\n", ast);

  let mut machine = WZero::new();
  machine.compile(&ast);

  // println!("Internal Machine State:\n{}", machine);


  let text = "p(f(X), h(Y, f(a)), Y)";
  let ast = parser::parse(text);
  println!("Program Expression: {}", text);
  // println!("Parse Tree:\n{}\n", ast);

  machine.compile(&ast);

  // println!("Internal Machine State:\n{}", machine);

}
