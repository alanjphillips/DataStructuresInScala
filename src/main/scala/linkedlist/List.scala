package linkedlist

import scala.annotation.tailrec

case object Empty extends LinkedList[Nothing]

case class Node[A](element: A, tail: LinkedList[A]) extends LinkedList[A]

trait LinkedList[+A] {

  def prepend[B >: A](element: B): LinkedList[B] = Node(element, this)

  @tailrec
  final def foldLeft[B](acc: B)(f: (B, A) => B): B = this match {
    case Empty               => acc
    case Node(element, next) => next.foldLeft(f(acc, element))(f)
  }

  def foldRight[B](acc: B)(f: (B, A) => B): B =
    reverse.foldLeft(acc)(f)

  def reverse: LinkedList[A] =
    foldLeft(LinkedList[A]()){
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