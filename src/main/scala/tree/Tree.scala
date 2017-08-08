package tree

case object Empty extends Tree[Nothing]

case class Node[+A <% Ordered[A]](element: A, left: Tree[A], right: Tree[A]) extends Tree[A]

abstract class Tree[+A <% Ordered[A]] {

  def insert[B >: A <% Ordered[B]](elem: B): Tree[B] = this match {
    case Empty => Node(elem, Empty, Empty)
    case Node(element, left, right) if elem < element => Node(element, left.insert(elem), right)
    case Node(element, left, right) if elem > element => Node(element, left, right.insert(elem))
    case Node(element, _, _) if elem == element       => this
  }

  def search[B >: A <% Ordered[B]](elem: B): Boolean = this match {
    case Empty => false
    case Node(element, _, _) if elem == element    => true
    case Node(element, left, _) if elem < element  => left.search(elem)
    case Node(element, _, right) if elem > element => right.search(elem)
  }

  def delete[B >: A <% Ordered[B]](elem: B): Tree[B] = this match {
    case Empty => Empty
    case Node(element, left, right) if elem < element   => Node(element, left.delete(elem), right)
    case Node(element, left, right) if elem > element   => Node(element, left, right.delete(elem))
    case Node(element, Empty, Empty) if elem == element => Empty
    case Node(element, left, Empty) if elem == element  => left
    case Node(element, Empty, right) if elem == element => right
    case n @ Node(element, _, right) if elem == element =>
      val replacement = leftMostChild(right)
      Node(replacement.element, n.left, n.right.delete(replacement.element))
  }

  // check if this covers all cases
  def leftMostChild[B >: A](tree: Tree[B]): Node[B] = tree match {
    case n @ Node(_, Empty, _) => n
    case Node(_, left, _)      => leftMostChild(left)
  }

  def preOrder[B >: A](tree: Tree[B] = this): List[B] = tree match {
    case Empty => Nil
    case Node(element, left, right) => element :: preOrder(left) ::: preOrder(right)
  }

  def inOrder[B >: A](tree: Tree[B] = this): List[B] = tree match {
    case Empty => Nil
    case Node(element, left, right) => inOrder(left) ::: element :: inOrder(right)
  }

  def postOrder[B >: A](tree: Tree[B] = this): List[B] = tree match {
    case Empty => Nil
    case Node(element, left, right) => postOrder(left) ::: postOrder(right) ::: List(element)
  }

}

object Tree {
  def apply[A]: Tree[A] = Empty
}
