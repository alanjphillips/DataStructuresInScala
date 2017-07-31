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

    "find node by position in a LinkedList" in {
      val listMulti = LinkedList(22, 33, 44, 55)
      listMulti.findByPosition(0) shouldBe Some(22)
      listMulti.findByPosition(1) shouldBe Some(33)
      listMulti.findByPosition(2) shouldBe Some(44)
      listMulti.findByPosition(3) shouldBe Some(55)
      listMulti.findByPosition(4) shouldBe None
    }

    "find node by position using fold in a LinkedList" in {
      val listMulti = LinkedList(22, 33, 44, 55)
      listMulti.findByPositionFold(0) shouldBe Some(22)
      listMulti.findByPositionFold(1) shouldBe Some(33)
      listMulti.findByPositionFold(2) shouldBe Some(44)
      listMulti.findByPositionFold(3) shouldBe Some(55)
      listMulti.findByPositionFold(4) shouldBe None
    }

    "remove node by position from a LinkedList" in {
      val listMulti = LinkedList(22, 33, 44, 55)
      listMulti.removeByPosition(0) shouldBe Node(33, Node(44, Node(55, Empty)))
      listMulti.removeByPosition(1) shouldBe Node(22, Node(44, Node(55, Empty)))
      listMulti.removeByPosition(2) shouldBe Node(22, Node(33, Node(55, Empty)))
      listMulti.removeByPosition(3) shouldBe Node(22, Node(33, Node(44, Empty)))
      listMulti.removeByPosition(4) shouldBe Node(22, Node(33, Node(44, Node(55, Empty))))
    }

    "return size of a LinkedList" in {
      LinkedList().size shouldBe 0
      LinkedList(22).size shouldBe 1
      LinkedList(22, 33, 44, 55).size shouldBe 4
    }

    "check if LinkedList is a palindrome" in {
      val listPal = LinkedList(22, 33, 44, 55, 44, 33, 22)
      listPal.isPalindrome shouldBe true

      val listNon = LinkedList(22, 33, 44, 55, 33, 22)
      listNon.isPalindrome shouldBe false
    }

  }

}