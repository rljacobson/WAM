#![allow(dead_code)]

use std::str::{Chars, pattern::Pattern};
use std::str::pattern::Searcher;


#[derive(Debug)]
pub struct CharIter<'d> {
  chars      :   Chars<'d>,
  next_char  :   Option<char>,
  row        :  usize,
  column     :  usize
}


impl<'d> Iterator for CharIter<'d>{
  type Item = char;

  fn next(&mut self) -> Option<char>{
    match self.next_char {

      None => {
        let c = self.chars.next();
        self.increment_location(c)
      },

      otherwise => {
        self.next_char = None;
        self.increment_location(otherwise)
      }

    } // end match
  } // end fn next
}


impl<'d> CharIter<'d>{

  pub fn new(text: &'d str) -> Self{
    CharIter{
      chars      :  text.chars(),
      next_char  :  None,
      row        :  1,
      column     :  1
    }
  }


  /// Returns the next character without consuming it or incrementing row/column.
  pub fn peek(&mut self) -> Option<char>{
    match self.next_char {

      None => {
        self.next_char = self.chars.next();
        self.next_char
      },

      otherwise => otherwise

    }
  }


  pub fn location(&self) -> (usize, usize) {
    return (self.row, self.column)
  }


  /// Passes next_char through while incrementing `self.row` or `self.column` as necessary.
  fn increment_location(&mut self, next_char: Option<char>) -> Option<char>{
    match next_char{

      Some('\n')      => {
        self.row   += 1;
        self.column = 0;
        Some('\n')
      }

      Some(character) => {
        self.column += 1;
        Some(character)
      }

      None            => None

    }
  }


  pub fn is_empty(&mut self) -> bool{
    self.peek() == None
  }


  /// Gives the underlying string slice.
  pub fn data(&self) -> &'d str {
    self.chars.as_str()
  }


  // We include several useful methods from `std::str`.

  /// Determines whether or not the underlying string starts with the given pattern.
  pub fn starts_with<P, T>(&self, pat: P) -> bool
    where P :  Pattern<'d, Searcher = T>,
          T :  Searcher<'d>
  {
    let text: &'d str = self.data();
    text.starts_with(pat)
  }


  /// Trims in place, unlike `str::trim_left_matches()`.
  pub fn trim_left_matches<P, T>(&mut self, pat: P)
    where P: Pattern<'d, Searcher=T>,
          T: Searcher<'d>
  {
    // We must account for the fact that the "first" `char` might be in `next_char`.
    if let Some(next_char) = self.next_char{
      // ToDo: Need to iterate over matched chars.

      if next_char.is_whitespace(){
        self.increment_location(Some(next_char));
        self.next_char = None;
      } else{
        // If `next_char` is not a whitespace, don't trim the beginning of `self.chars`.
        return;
      }
    }
    // Update the location.
    let original = self.data();
    let trimmed = original.trim_start_matches(pat);
    let length = original.len() - trimmed.len();
    self.chars = trimmed.chars();
    for c in original[..length].chars(){
      self.increment_location(Some(c));
    }
  }


  // Trims whitespace in place, unlike `str::trim_left()`.
  pub fn trim_left(&mut self){
    self.trim_left_matches(|c: char|{c.is_whitespace()});
  }


  /// Consumes the prefix string for which each character `c` matches `pred(c)`, returning the
  /// prefix.
  //  ToDo: This would be better if it took a `std::str::Pattern`, since patterns can be
  //        constructed from predicates.
  pub fn get_prefix_match(&mut self, pred: fn(char)->bool) -> Option<&str> {
    // We look for the index of the first character (byte) that doesn't match `pred`.
    let text = self.data();
    if let Some(end) = text.find(
      |c: char|{
        !pred(c)
      }
    ) {
      match end{

        0 => None,

        _ => {
          for c in text[..end].chars(){
            self.increment_location(Some(c));
          }
          let result = Some(&text[0..end]);
          // Consume the first `end - 1` `chars` in `data`.
          self.chars.nth(end-1);
          result
        }

      }
    } else {
      // All `char`s match predicate. Empty `self.chars` and return entire string.
      let result = self.data();
      // ToDo: Why can't I say, `for c in self`?
      loop {
        let c = self.next();
        if c == None { break; }
        self.increment_location(c);
      }
      // If the original string was empty, then all `char`s in the string match.
      match result.is_empty(){

        true => None,

        false => {
          self.chars = "".chars(); // empty<char>() isn't a Char.
          Some(result)
        }

      } // end match `result.is_empty`
    } // end else (All `char`s matched)
  }

}
