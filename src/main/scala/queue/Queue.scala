package queue

import linkedlist.{Empty, LinkedList, Node}

case class Queue[+A] private (list: LinkedList[A] = Empty) extends Queueable[A]


/**
  * Further improvements:
  * A better implementation would involve using 2 lists to represent Queue, an 'in' list and an 'out' list
  * Instead of calling reverse for every dequeue operation, reverse will only be called on the 'in' list when it
  * replaces the empty 'out' list
  */
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
