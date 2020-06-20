/*!

This module parses a string representing a term.

Terms have the following recursively defined lexical structure:

  * Variables: A single uppercase letter other than `X`
  * Registers: An `X` followed by an integer
  * Constants: A single lowercase letter
  * Functors: A single lowercase letter followed by a comma-delineated list of terms
              enclosed in parentheses
  * A Program: The top-most (outer-most) term with no prefix (cp. Query).
  * A Query: The literal `?-` followed by a top-most (outer-most) term

Whitespace is ignored. It is an error to have two terms adjacent except if separated
by a comma as part of a functor. We allow `X` as a variable name but emit a
warning. We allow comma-delineated lists to have a trailing comma without warning.

The internal representation of a functor is a (functor) structure, which has a name and
a list of arguments, themselves `Terms`. This is the only tree-like (recursive) `Term`
variant. Constants are represented as a structure with a zero-length argument list.

*/

use std::rc::Rc;

use string_cache::DefaultAtom;

use super::chariter::CharIter;
use super::term::{Term, TermVec};
use crate::functor::{Functor, ArityType};

struct Parser<'a>{
  text   : CharIter<'a>,
  errors : Vec<String>
}


/// Parses text to produce abstract syntax trees made of `Term`s.
/// Returns a tuple (atoms, queries).
pub fn parse(input: &str) -> Result<(TermVec, TermVec), ()> {
  let mut parser = Parser::new(input);

  parser.parse()
}

impl<'a> Parser<'a> {

  pub fn new(input: &'a str) -> Self{
    Parser{
      text: CharIter::new(input),
      errors: vec![]
    }
  }

  /// Parses text to produce abstract syntax trees made of `Term`s.
  /// Returns a tuple (atoms, queries).
  pub fn parse(&mut self) -> Result<(TermVec, TermVec), ()> {
    // For M_1, all atoms are facts.
    let mut atoms   :  TermVec  = Vec::new();
    let mut queries :  TermVec  = Vec::new();
    let mut success :  bool     = true;

    if self.text.is_empty() {
      eprintln!("Input is empty.");
      return Err(());
    }

    loop {
      let parse_result = self.parse_aux();
      match parse_result {
        Ok(Some(Term::Query(term))) => {
          queries.push(Rc::try_unwrap(term).unwrap());
        }

        Ok(Some(term)) => {
          atoms.push(term);
        }

        Err(errors) => {
          for error in errors{
            eprintln!("{}", error);
          }
          // We keep attempting to parse to accumulate all errors.
          success = false;
        }

        Ok(None) => {
          // Empty
          success = false;
          break;
        }
      };

      // Allow trailing whitespace, but nothing more.
      self.text.trim_left();
      if self.text.is_empty() {
        break;
      }
    } // end loop

    match success {
      true => Ok((atoms, queries)),
      false => {
        for error in &self.errors{
          eprintln!("{}", error);
        }
        Err(())
      }
    }
  }


  fn parse_aux(&mut self) -> Result<Option<Term>, Vec<String>> {
    let mut next_char    :  char;
    let mut local_errors :  Vec<String>  = Vec::new();

    // Handle Comments, which require resetting `next_char`.
    loop {
      self.text.trim_left();
      match self.text.next() {
        Some(c) => {
          next_char = c;
        },
        None => {
          return Ok(None);
        }
      }

      // Single-line comments
      if next_char == '%' {
        // Eat until EOL or EOF.
        while self.text.peek() != None && self.text.next() != Some('\n') {
          // Gobble
        }
      }
      // In-line comments
      else if next_char == '/' && self.text.peek() == Some('*') {
        let (row, column) = self.text.location();
        self.text.next();
        // Eat until EOL or EOF.
        loop {
          if self.text.peek() == None {
            local_errors.push(format!("Error on line {}, column {}: Unterminated `/*`.", row, column));
            return Err(local_errors);
          }
          // Gobble
          next_char = self.text.next().unwrap();
          if next_char == '*' && self.text.peek() == Some('/') {
            self.text.next();
            // Break inner while-loop, reset `next_char`
            break;
          }
        }
      } else {
        // Not a comment, break outer loop.
        break;
      }

      //reset `next_char` at the top of the loop.
    } // end comment loop

    // A giant string of if-else's //

    if next_char.is_lowercase() {
      // Functor Structure //
      // Get the rest of the functor's name
      let name = match self.text.get_prefix_match(char::is_alphanumeric) {
        Some(rest) => DefaultAtom::from(
          format!("{}{}", next_char, rest)
        ),

        None => DefaultAtom::from(next_char.to_string())
      };

      match self.parse_arg_list(){

        Ok(args) => {
          Ok(Some(Term::Structure {
            functor: Functor { name, arity: args.len() as ArityType },
            args
          }))
        }

        Err(errors) => Err(errors)

      }

    }
    else if next_char == ',' || next_char == ')' {
      // The assumption here is that `parse_aux` was called recursively, and the current term has
      // been completely parsed.
      Ok(None)
    }
    else if next_char.is_uppercase() {
      // A variable. Get the rest of the name.
      let name = match self.text.get_prefix_match(char::is_alphanumeric) {
        Some(rest) => DefaultAtom::from(
          format!("{}{}", next_char, rest)
        ),

        None => DefaultAtom::from(next_char.to_string())
      };
      Ok(Some(Term::Variable(name)))
    }
    else if next_char == '?' {
      // Note: There is no equivalent case for `Term::Program`. Every non-query is a program.
      match self.text.next() {

        Some('-') => {
          let term_option = self.parse_aux();
          match term_option {

            Ok(Some(term)) => Ok(Some(Term::Query(Rc::new(term)))),

            otherwise => otherwise,

          }
        }

        _ => {
          let (row, column) = self.text.location();
          local_errors.push(format!("Error on line {}, column {}: Unexpected character `{}`",
                                    row, column, next_char));
          Err(local_errors)
        }
      }
    } else {
      let (row, column) = self.text.location();
      local_errors.push(format!("Error on line {}, column {}: Unexpected character `{}`",
                                row, column, next_char));
      Err(local_errors)
    }
  }

  /**
  Parses a comma separated list of terms and returns them in a vector. As functors can have zero
  arguments (constants), the vector returned may be empty.
*/ fn parse_arg_list(&mut self) -> Result<TermVec, Vec<String>> {
    let mut args         :  TermVec      = Vec::new();
    let mut local_errors :  Vec<String>  = Vec::new();


    self.text.trim_left();
    if let Some(c) = self.text.peek() {
      if c != '(' {
        // Constants can omit parentheses, as they have no arguments.
        return Ok(args);
      } else {
        // Eat `(`
        self.text.next();
      }
    }

    loop {
      match self.parse_aux(){

        Ok(Some(term)) => {
          args.push(term);
        }

        Ok(None) => {
          // Why is it None?
          if self.text.is_empty() {
            // Is this error distinct from `incomplete argument list`?
            let (row, column) = self.text.location();
            local_errors.push(
              format!(
                "Error on line {}, column {}: Reached EOL while looking for `)`.",
                row, column)
            );
            return Err(local_errors);
          }
          return Ok(args);
        }

        Err(mut msg) => {
          local_errors.append(&mut msg);
        }
      }


      self.text.trim_left();
      let peek_char = self.text.peek();
      match peek_char {
        Some(',') => {
          // Eat the `,` character.
          self.text.next();
        },

        Some(')') => {
          // Eat the `)` character and return.
          self.text.next();
          break;
        },

        Some(unexpected) => {
          // Expressions must be separated by a `,` in an argument list.
          let (row, column) = self.text.location();
          local_errors.push(
            format!(
              "Error on line {}, column {}: Expected `,` or `)`, but got `{}`. Inserting `,`.",
                                    row, column, unexpected));
        }

        None => {
          let (row, column) = self.text.location();
          local_errors.push(
            format!(
              "Error on line {}, column {}: Incomplete argument list. Perhaps you are missing `)`?",
                    row, column));
        }
      } // end match peek
    } // end while

    match local_errors.is_empty(){
      true => Ok(args),
      false => Err(local_errors)
    }

  }
}
