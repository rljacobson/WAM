/*!

This module parses a string representing a term.

Terms have the following recursively defined lexical structure:

  * Variables: A single uppercase letter other than `X`
  * Registers: An `X` followed by an integer
  * Constants: A single lowercase letter
  * Functors: A single lowercase letter followed by a comma-delineated list of terms enclosed in
              parentheses
  * A Program: The top-most (outer-most) term with no prefix (cp. Query).
  * A Query: The literal `?-` followed by a top-most (outer-most) term

Whitespace is ignored. It is an error to have two terms adjacent except if separated by a comma
as part of a functor. We allow `X` as a variable name but emit a warning. We allow
comma-delineated lists to have a trailing comma without warning.

The internal representation of a functor is a structure, which has a name and a list of arguments.
Constants are represented as a structure with a zero-length argument list.

*/

use std::cell::RefCell;
use std::rc::Rc;

use crate::chariter::CharIter;
use crate::term::{Term, RefTermVec, new_ref_term_vec};
use crate::functor::{Functor, ArityType};
use std::ops::Deref;
use crate::address::Address;

type RcRefCell<T> = Rc<RefCell<T>>;

static WARN_ON_X_VAR: bool = false;


pub fn parse<'b>(mut input: &'b str) -> Term{
  if input.is_empty() {
    return Term::Empty;
  }

  let mut is_query = false;
  if input.starts_with("?-"){
    // Query
    is_query = true;
    input = &input[2..];
  }

  // We need multiple mutable borrows for recursive calls to parsing functions.
  let text_ref: RcRefCell<CharIter<'b>> =
    Rc::new(
      RefCell::new(
        CharIter::<'b>::new(input)
      )
    );
  let inner = parse_aux(Rc::clone(&text_ref));
  let mut text = RefCell::borrow_mut(&*text_ref);

  // Allow trailing whitespace.
  text.trim_left();
  if !text.is_empty(){
    eprintln!("Error: Expected end of term or query.");
    panic!();
  }

  match is_query{
    true => inner.to_query(),
    false => inner.to_program()
  }
}


fn parse_aux(text_ref: RcRefCell<CharIter>) -> Term {
  let mut text = RefCell::borrow_mut(&*text_ref);

  text.trim_left();
  let next_char: char;

  match text.next(){
    Some(c) => {
      next_char = c;
    },
    None => {
      return Term::Empty;
    }
  }

  if next_char.is_lowercase(){
    // `parse_arg_list` needs mutable access to the `CharIter`.
    drop(text);
    let args = parse_arg_list(Rc::clone(&text_ref) );
    let args_ref = (args.deref()).borrow();
    Term::Structure {
      functor: Functor{name: next_char, arity: args_ref.len() as ArityType},
      args: Rc::clone(&args) }
  }
  else if next_char == ',' || next_char == ')' {
    // The assumption here is that `parse_aux` was called recursively, and the current term has
    // been completely parsed.
    Term::Empty
  }
  else if next_char == 'X'{
    // Either a register or a variable
    if let Some(number) = text.get_prefix_match(char::is_numeric) {
        // A register.
        Term::Register(
          Address::RegPtr(
            number.parse::<usize>().unwrap().into()
          )
        )
    } else {
      // A variable, not a register. Warn about strange use of `X`.
      if WARN_ON_X_VAR {
        eprintln!("Warning: `X` used as a variable, which should be avoided.");
      }
      Term::Variable(next_char)
    }
  } // end if register or a variable
  else if next_char.is_uppercase(){
    // Variable
    Term::Variable(next_char)
  }
  else {
    eprintln!("Error: Unexpected character `{}`", next_char);
    panic!();
    // Term::Epsilon
  }
}

fn parse_arg_list(text_ref: RcRefCell<CharIter>) -> RefTermVec {
  let args: RefTermVec = new_ref_term_vec();
  let mut args_ref     = args.borrow_mut();
  let mut text         = RefCell::borrow_mut(&*text_ref);

  text.trim_left();
  if let Some(c) = text.peek(){
    if c != '(' {
      // Constants can omit parentheses, as they have no arguments.
      drop(args_ref);
      return args;
    } else {
      // Eat `(`
      text.next();
    }
  }

  let mut term: Term;

  loop {
    // `parse_aux` needs a mutable borrow of text, so drop this one.
    drop(text);
    term = parse_aux(text_ref.clone());
    text = RefCell::borrow_mut(&*text_ref);
    if text.is_empty(){
      eprintln!("Reached EOL while looking for `)`.");
      panic!();
    }
    if term != Term::Empty {
      args_ref.push(term);
    } else {
      drop(args_ref);
      return args;
    }

    text.trim_left();
    match text.peek(){
      Some(',') => {
        // Eat the `,` character.
        text.next();
      },
      Some(')') => {
        // Eat the `)` character and return.
        text.next();
        break;
      },
      _ => {
        // Don't advance (pass).
      }
    } // end match peek
  } // end while
  drop(args_ref);
  return args;
}

#[cfg(test)]
mod tests {
  use super::*;
  use crate::term::{new_struct_of_term, make_struct};
  use crate::term::tests::new_constant;

  #[test]
  fn expr0() {
    let ast = parse("X");
    assert_eq!(
      Term::Variable('X').to_program(),
      ast
    );
  }

  #[test]
  fn expr1() {
    let text = "p(f(X6), h( Y, f(a )),Y) ";
    let ast  = parse(text);


    let f = new_struct_of_term(
      'f',
      &Term::Register(Address::from_reg_idx(6))
    );

    let h = make_struct(
      'h',
      &mut vec![
        Term::Variable('Y'),
        new_struct_of_term(
          'f',
          &new_constant('a')
        )
      ]
    );

    let answer = make_struct(
      'p',
      &mut vec![
        f,
        h,
        Term::Variable('Y')
      ]
    ).to_program();

    assert_eq!(answer, ast);
  }

  #[test]
  fn expr2() {
    let text = "p(f(X6), h( Y, f(a )),Y) ";
    let ast  = parse(text);

    let f = new_struct_of_term(
      'f',
      &Term::Register(Address::RegPtr(6))
    );

    let h = make_struct(
      'h',
      &mut vec![
        Term::Variable('Y'),
        new_struct_of_term(
          'f',
          &new_constant('a')
        )
      ]
    );

    let answer = make_struct(
      'p',
      &mut vec![
        f,
        h,
        Term::Variable('Y')
      ]
    ).to_program();

    // Poor man's tree comparison.
    assert_eq!(format!("{}", answer), format!("{}", ast));
  }

  #[test]
  fn expr3() {
    let text = "?- p(f(X123456), h( Y, f(a )),Y,) ";
    let ast  = parse(text);

    let f = new_struct_of_term(
      'f',
      &Term::Register(Address::RegPtr(123456))
    );

    let h = make_struct(
      'h',
      &mut vec![
        Term::Variable('Y'),
        new_struct_of_term(
          'f',
          &new_constant('a')
        )
      ]
    );

    let answer = make_struct(
      'p',
      &mut vec![
        f,
        h,
        Term::Variable('Y')
      ]
    ).to_query();

    // Poor man's tree comparison.
    assert_eq!(format!("{}", answer), format!("{}", ast));
  }

  #[test]
  fn exp4(){
    let text = "p(f(X), h(Y, f(a)), Y)";
    let ast = parse(text);

    let answer = make_struct(
      'p',
      &mut vec![
        new_struct_of_term(f, &Term::Variable('X')),
        make_struct(
          'h',
          &mut vec![
            Term::Variable('X'),
            new_struct_of_term(f, &new_constant('a'))
          ]
        ),
        Term::Variable('Y')
      ]
    );

    assert_eq!(ast, answer);
  }


  #[test]
  #[should_panic]
  fn malformed1() {
    parse("&&");
  }

  #[test]
  #[should_panic]
  fn malformed2() {
    parse("p(A, f(X, X11)");
  }

  #[test]
  #[should_panic]
  fn malformed3() {
    parse("p(q, R)Z");
  }

}
