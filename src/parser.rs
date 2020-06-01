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

The internal representation of a functor is a (functor) structure, which has a name and
a list of arguments, themselves `Terms`. This is the only tree-like (recursive) `Term`
variant. Constants are represented as a structure with a zero-length argument list.

*/

use std::cell::RefCell;
use std::rc::Rc;

use string_cache::DefaultAtom;

use crate::chariter::CharIter;
use crate::term::{RcTerm, Term, TermVec};
use crate::functor::{Functor, ArityType};


/// Parses text to produce an abstract syntax tree made of `Term`s.
pub fn parse<'b>(input: &'b str) -> RcTerm{
  // A caller intending to compile the term should have done this already, but the parse function
  // may be called for other reasons, e.g., to print out the AST.
  let input =
    if input.starts_with("?-"){ &input[2..] }
    else { input };

  if input.is_empty() {
    return Rc::new(Term::Empty);
  }

  // We need multiple mutable borrows for recursive calls to parsing functions.
  let text_ref: Box<RefCell<CharIter>> =
    Box::new(
      RefCell::new(
        CharIter::<'b>::new(input)
      )
    );
  let ast = parse_aux(&text_ref);
  let mut text = RefCell::borrow_mut(&*text_ref);

  // Allow trailing whitespace, but nothing more.
  text.trim_left();
  if !text.is_empty(){
    eprintln!("Error: Expected end of term or query.");
    panic!();
  }

  ast
}


fn parse_aux(text_ref: &Box<RefCell<CharIter>>) -> RcTerm {
  let mut text = RefCell::borrow_mut(&*text_ref);

  text.trim_left();
  let next_char: char;

  match text.next(){
    Some(c) => {
      next_char = c;
    },
    None => {
      return Rc::new(Term::Empty);
    }
  }

  // A giant string of if-else's.

  if next_char.is_lowercase(){
    // Functor //
    // Get the rest of the functor's name
    let name =
      match  text.get_prefix_match(char::is_alphanumeric){

        Some(rest) => DefaultAtom::from(
          format!("{}{}", next_char, rest)
        ),

        None => DefaultAtom::from(next_char.to_string())

    };

    // `parse_arg_list` needs mutable access to the `CharIter`.
    drop(text);
    let args = parse_arg_list(text_ref);

    Rc::new(
      Term::Structure {
      functor: Functor{name, arity: args.len() as ArityType},
      args: args.clone()
    })
  }

  else if next_char == ',' || next_char == ')' {
    // The assumption here is that `parse_aux` was called recursively, and the current term has
    // been completely parsed.
    Rc::new(Term::Empty)
  }

  else if next_char.is_uppercase(){
    // Either a register or a variable

    /*
    if next_char == 'X'{
      if let Some(number) = text.get_prefix_match(char::is_numeric) {
        // A register.
        return Rc::new(Term::Register(
          Address::RegPtr(
            number.parse::<usize>().unwrap().into()
          )
        ));
      }
    } // end if register
    */

    // A variable, not a register. Get the rest of the name.
    let name =
      match  text.get_prefix_match(char::is_alphanumeric){

        Some(rest)  => DefaultAtom::from(
          format!("{}{}", next_char, rest)
        ),

        None => DefaultAtom::from(next_char.to_string())

      };
    Rc::new(Term::Variable(name))
  }

  else if next_char == '?' {
    // Note: There is no equivalent case for `Term::Program`. The caller should be handling this.
    match text.peek() {
      Some('-') => {
        text.next(); // Eat '-'
        // `parse_aux` needs a mutable borrow of text, so drop this one.
        drop(text);
        let term = parse_aux(text_ref);
        Rc::new(Term::Query(term))
      }
      _ => {
        eprintln!("Error: Unexpected character `{}`", next_char);
        panic!();
      }
    }
  }

  else {
    eprintln!("Error: Unexpected character `{}`", next_char);
    panic!();
  }
}

fn parse_arg_list(text_ref: &Box<RefCell<CharIter>>) -> TermVec {
  let mut args: Box<Vec<RcTerm>> = Box::new(Vec::new());
  let mut text = RefCell::borrow_mut(&*text_ref);

  text.trim_left();
  if let Some(c) = text.peek(){
    if c != '(' {
      // Constants can omit parentheses, as they have no arguments.
      return args.into();
    } else {
      // Eat `(`
      text.next();
    }
  }

  let mut term: RcTerm;

  loop {
    // `parse_aux` needs a mutable borrow of text, so drop this one.
    drop(text);
    term = parse_aux(text_ref);
    text = RefCell::borrow_mut(&*text_ref);
    if text.is_empty(){
      eprintln!("Reached EOL while looking for `)`.");
      panic!();
    }

    if *term != Term::Empty {
      args.push(term.into());
    } else {
      return args.into();
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
        // Don't advance.
      }

    } // end match peek
  } // end while
  return args.into();
}
