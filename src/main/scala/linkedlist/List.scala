package linkedlist

import scala.annotation.tailrec

case object Empty extends LinkedList[Nothing] {
  val size = 0
}

case class Node[A](element: A, tail: LinkedList[A]) extends LinkedList[A] {
  val size = 1 + tail.size
}

trait LinkedList[+A] {

  def size: Int

  def prepend[B >: A](element: B): LinkedList[B] = Node(element, this)

  @tailrec
  final def foldLeft[B](acc: B)(f: (B, A) => B): B = this match {
    case Empty => acc
    case Node(element, next) => next.foldLeft(f(acc, element))(f)
  }

  def foldRight[B](acc: B)(f: (B, A) => B): B =
    reverse.foldLeft(acc)(f)

  def reverse: LinkedList[A] =
    foldLeft(LinkedList[A]()) {
      (acc, next) => {
        if (next != Empty)
          Node(next, acc)
        else
          acc
      }
    }

  def filter(f: A => Boolean): LinkedList[A] =
    foldRight(LinkedList[A]()) {
      (acc, next) => {
        if (f(next))
          Node(next, acc)
        else
          acc
      }
    }

  def remove[B >: A](elem: B): LinkedList[B] =
    foldRight(LinkedList[B]()) {
      (acc, next) => {
        if (next != elem)
          Node(next, acc)
        else
          acc
      }
    }

  def findByPosition(position: Int): Option[A] = {
    @tailrec
    def inner(curr: Int, pos: Int, node: LinkedList[A]): Option[A] = node match {
      case Empty => None
      case Node(element, next) => {
        if (curr == pos) Some(element)
        else inner(curr + 1, pos, next)
      }
    }
    if (position >= 0 && position < size)
      inner(0, position, this)
    else
      None
  }

  def findByPositionFold(position: Int): Option[A] =
    if (position >= 0 && position < size)
      foldLeft((0, Option.empty[A])) {
        (acc, next) => {
          if (position == acc._1)
            (acc._1 + 1, Some(next))
          else
            (acc._1 + 1, acc._2)
        }
      }._2
    else
      None

  def removeByPosition(position: Int): LinkedList[A] =
    if (position >= 0 && position < size)
      foldLeft((0, LinkedList[A]())) {
        (acc, next) => {
          if (position == acc._1)
            (acc._1 + 1, acc._2)
          else
            (acc._1 + 1, Node(next, acc._2))
        }
      }._2.reverse
    else
      this

}

object LinkedList {

  def apply[A](a: A*): LinkedList[A] = {
    @tailrec
    def inner(lst: LinkedList[A], seq: A*): LinkedList[A] = {
      if (seq.isEmpty)
        lst
      else
        inner(Node(seq.head, lst), seq.tail: _*)
    }
    inner(Empty, a.reverse: _*)
  }

}