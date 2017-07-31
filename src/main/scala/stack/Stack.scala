package stack

import linkedlist.{Empty, LinkedList, Node}

case class Stack[+A] private (list: LinkedList[A] = Empty) extends Stackable[A]

trait Stackable[+A] {
  this: Stack[A] =>

  def size = list.size

  def push[B >: A](element: B): Stack[B] = Stack(
    list = Node(element, list)
  )

  def pop: (Option[A], Stack[A]) = list match {
    case Empty               => (None, this)
    case Node(element, next) => (Some(element), Stack(next))
  }

}

object Stack {

  def apply[A](): Stack[A] = new Stack()

}