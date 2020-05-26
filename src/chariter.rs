#![allow(dead_code)]

use std::str::{Chars, pattern::Pattern};
use std::cell::{RefCell, Cell};
use std::str::pattern::Searcher;

#[derive(Debug)]
pub struct CharIter<'d> {
  chars     :  RefCell<Chars<'d>>,
  next_char :  Cell<Option<char>>
}

impl<'d> Iterator for CharIter<'d>{
  type Item = char;

  fn next(&mut self) -> Option<char>{
    let next_char_ref = self.next_char.get();
    match next_char_ref {
      None => {
        self.chars.borrow_mut().next()
      },
      _c => {
        self.next_char.set(None);
        _c
      }
    } // end match
  } // end fn next
}

impl<'d> CharIter<'d>{

  pub fn new(text: &'d str) -> Self{
    CharIter{
      chars     : RefCell::new(text.chars()),
      next_char : Cell::new(None)
    }
  }

  /// Returns the next character without consuming it.
  pub fn peek(&self) -> Option<char>{
    let next_char_ref = self.next_char.get();
    match next_char_ref {
      None => {
        let mut chars_ref = self.chars.borrow_mut();
        let c: Option<char> = chars_ref.next();
        self.next_char.set(c);
        c
      },
      _c => _c
    }
  }

  pub fn is_empty(&self) -> bool{
    self.peek() == None
  }

  /// Gives the underlying string slice.
  pub fn data(&self) -> &'d str {
    self.chars.borrow().as_str()
  }

  // We include several useful methods from `std::str`.

  /// Determines whether or not the underlying string starts with the given pattern.
  pub fn starts_with<P, T>(&self, pat: P) -> bool
    where P: Pattern<'d, Searcher=T>,
    T: Searcher<'d>{
    let text: &'d str = self.data();
    text.starts_with(pat)
  }

  /// Trims in place, unlike `str::trim_left_matches()`.
  pub fn trim_left_matches<P, T>(&mut self, pat: P)
    where P: Pattern<'d, Searcher=T>,
          T: Searcher<'d>{
    // We must account for the fact that the "first" `char` might be in `next_char`.
    if let Some(next_char) = self.next_char.get(){
      if next_char.is_whitespace(){
        self.next_char.set(None);
      } else{
        // If `next_char` is not a whitespace, don't trim the beginning of `self.chars`.
        return;
      }
    }
    self.chars = RefCell::new(self.data().trim_start_matches(pat).chars());
  }

  // Trims whitespace in place, unlike `str::trim_left()`.
  pub fn trim_left(&mut self){
    self.trim_left_matches(|c: char|{c.is_whitespace()});
  }

  /// Consumes the prefix string for which each character `c` matches `pred(c)`, returning the
  /// prefix.
  //  ToDo: This would be better if it took a `std::str::Pattern`, since patterns can be
  //        constructed from predicates.
  pub fn get_prefix_match(&mut self, pred: fn(char)->bool)
    -> Option<&str> {
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
          let result = Some(&text[0..end]);
          // Consume the first `end - 1` `chars` in `data`.
          let mut chars_ref = self.chars.borrow_mut();
          chars_ref.nth(end-1);
          result
        }
      }
    } else {
      // All `char`s match predicate. Empty `self.chars` and return entire string.
      let result = self.data();
      // If the original string was empty, then all `char`s in the string match.
      match result.is_empty(){
        true => None,
        false => {
          let mut chars_ref = self.chars.borrow_mut();
          *chars_ref = "".chars(); // empty<char>() isn't a Char.
          Some(result)
        }
      } // end match `result.is_empty`
    } // end else (All `char`s matched)
  }
}


#[cfg(test)]
mod tests {
  use super::*;

  #[test]
  fn peek_and_next(){
    let mut c = CharIter::new("abcd");
    assert_eq!(c.peek(), Some('a'));
    assert_eq!(c.next(), Some('a'));
    assert_eq!(c.next(), Some('b'));
    assert_eq!(c.next(), Some('c'));
    assert_eq!(c.peek(), Some('d'));
    assert_eq!(c.next(), Some('d'));
    assert_eq!(c.next(), None);
    assert_eq!(c.next(), None);
  }

  #[test]
  fn empty_chars(){
    let mut c = CharIter::new("");
    assert!(c.is_empty());
    let mut c2 = CharIter::new("");
    assert_eq!(c2.next(), None);
    assert_eq!(c2.next(), None);
  }

  #[test]
  fn get_empty_prefix(){
    let mut c = CharIter::new("");
    let result = c.get_prefix_match(|ch: char|{ ch.is_uppercase()});
    assert_eq!(result, None);
    assert_eq!(c.next(), None);
    assert_eq!(c.data(), "");
  }

  #[test]
  fn trim_left_str(){
    let mut c = CharIter::new("aaaabcd");
    c.trim_left_matches("a");
    assert_eq!(c.data(), "bcd");
  }


  #[test]
  fn trim_left_char(){
    let mut c = CharIter::new("aaaabcd");
    c.trim_left_matches('a');
    assert_eq!(c.data(), "bcd");
  }


  #[test]
  fn trim_left_pred(){
    let mut c = CharIter::new("aaaabcd");
    c.trim_left_matches(|ch: char |{ ch == 'a'});
    assert_eq!(c.data(), "bcd");
  }


  #[test]
  fn trim_whitespace(){
    let mut c = CharIter::new("  \t\n   abcd");
    c.trim_left();
    assert_eq!(c.data(), "abcd");
  }

  #[test]
  fn get_prefix(){
    let mut c = CharIter::new("ABCDEFGabcd");
    let result = c.get_prefix_match(|ch: char|{ ch.is_uppercase()});
    assert_eq!(result, Some("ABCDEFG"));
    assert_eq!(c.next(), Some('a'));
    assert_eq!(c.data(), "bcd");
  }

}
