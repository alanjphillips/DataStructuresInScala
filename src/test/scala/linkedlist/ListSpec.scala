package linkedlist

import org.scalatest.{Matchers, WordSpec}

class ListSpec extends WordSpec with Matchers {

  "LinkedList" should {

    "create a empty LinkedList" in {
      val emptyList = LinkedList()
      emptyList shouldBe Empty
    }

    "create a single node LinkedList" in {
      val listOne = LinkedList(22)
      listOne shouldBe Node(22, Empty)
    }

    "create a multi node LinkedList" in {
      val listMulti = LinkedList(22, 33, 44, 55)
      listMulti shouldBe Node(22, Node(33, Node(44, Node(55, Empty))))
    }

    "prepend a new node to existing LinkedList" in {
      val listMulti = LinkedList(22, 33, 44, 55)
      val listUpdated = listMulti.prepend(11)
      listUpdated shouldBe Node(11, Node(22, Node(33, Node(44, Node(55, Empty)))))
    }

    "foldLeft over a LinkedList giving an accumulated result" in {
      val listMulti = LinkedList(22, 33, 44, 55)
      val listResult = listMulti.foldLeft(LinkedList[Int]()) {
        (acc, next) =>
          if (next != Empty)
            Node(next, acc)
          else
            acc
      }
      listResult shouldBe Node(55, Node(44, Node(33, Node(22, Empty))))
    }

    "foldRight over a LinkedList giving an accumulated result" in {
      val listMulti = LinkedList(22, 33, 44, 55)
      val listResult = listMulti.foldRight(LinkedList[Int]()) {
        (acc, next) =>
          if (next != Empty)
            Node(next, acc)
          else
            acc
      }
      listResult shouldBe Node(22, Node(33, Node(44, Node(55, Empty))))
    }

    "reverse a LinkedList" in {
      val listMulti = LinkedList(22, 33, 44, 55)
      listMulti.reverse shouldBe Node(55, Node(44, Node(33, Node(22, Empty))))
    }

    "filter a LinkedList" in {
      val listMulti = LinkedList(22, 33, 44, 55)
      listMulti.filter(a => a != 44) shouldBe Node(22, Node(33, Node(55, Empty)))
    }

    "remove nodes with value == 44 from a LinkedList" in {
      val listMulti = LinkedList(22, 33, 44, 55)
      listMulti.remove(44) shouldBe Node(22, Node(33, Node(55, Empty)))
    }
  }

}