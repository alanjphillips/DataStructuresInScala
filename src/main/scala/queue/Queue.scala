package queue

import linkedlist.{Empty, LinkedList, Node}

case class Queue[+A] private (list: LinkedList[A] = Empty) extends Queueable[A]

trait Queueable[+A] {
  this: Queue[A] =>

  def size = list.size

  def enqueue[B >: A](elem: B): Queue[B] =
    Queue(
      list = Node(elem, list)
    )

  def dequeue: (Option[A], Queue[A]) = list.reverse match {
    case Empty               => (None, Queue(Empty))
    case Node(element, next) => (Some(element), Queue(next.reverse))
  }

}

object Queue {
  def apply[A]: Queue[A] = new Queue()
}
