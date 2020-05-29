
/**
  An abstraction over the idea of a tree (or tree node) that possibly has children that can be
  iterated over.
*/
pub trait Tree<T>
  where T:IntoIterator{
  fn children() -> Option<T>;
}

/**
  An iterator that traverses a tree breadth first. The type it iterates over must implement the
  `Tree` trait above.
*/
#[derive(Debug)]
pub struct BreadthFirstIter<T, U> where U: Clone{
  children_deque: VecDeque<Rc<U>>, // A stack of terms/tokens yet to be visited.
}

impl<T> BreadthFirstIter<T, U> {
  pub fn new(start: &Rc<U>) -> BreadthFirstIter<T, U> {
    BreadthFirstIter {
      children_deque: VecDeque::from(vec![start.deref().clone()])
    }
  }
}

/// Iterates over the terms in the term tree breadth first.
impl<T, U> Iterator for BreadthFirstIter<T, U>
  where T: IntoIterator{
  type Item = U;

  fn next(&mut self) -> Option<Self::Item> {
    let current: &Rc<U> = self.children_deque.pop_front();
    match &current {
      Some(item) => {
        match item.children(){
          Some(children) => {
            self.children_deque.extend(children.iter().cloned());
            current,
          },
          _t => {
            current
          }
        }
      },
      None => {
        None
      }
    }
  }
}
