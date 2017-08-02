package tree

case object Empty extends Tree[Nothing]

case class Node[+A](element: A, left: Tree[A], right: Tree[A]) extends Tree[A]

trait Tree[+A] {

  def insert[B >: A](elem: B): Tree[B] = Node(elem, Empty, Empty)

  def delete[B >: A](elem: B): Tree[B] = Node(elem, Empty, Empty)

}

