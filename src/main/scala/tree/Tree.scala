package tree

case object Empty extends Tree[Nothing]

case class Node[+A](element: A, left: Tree[A], right: Tree[A]) extends Tree[A]

trait Tree[+A] {

  def insert[B >: A](elem: B)(implicit cmp: Ordering[B]): Tree[B] = this match {
    case Empty => Node(elem, Empty, Empty)
    case Node(element, left, right) if (cmp.lt(elem, element)) => Node(element, left.insert(elem), right)
    case Node(element, left, right) if (cmp.gt(elem, element)) => Node(element, left, right.insert(elem))
    case Node(element, _, _) if (cmp.equiv(elem, element))     => this
  }

  def search[B >: A](elem: B)(implicit cmp: Ordering[B]): Boolean = this match {
    case Empty => false
    case Node(element, _, _) if (cmp.equiv(elem, element)) => true
    case Node(element, left, _) if (cmp.lt(elem, element)) => left.search(elem)
    case Node(element, _, right) if (cmp.gt(elem, element)) => right.search(elem)
  }

  def delete[B >: A](elem: B): Tree[B] = Empty

}

object Tree {
  def apply[A]: Tree[A] = Empty
}
