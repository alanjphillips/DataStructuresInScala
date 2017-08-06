package tree

import org.scalatest.{Matchers, WordSpec}
import TreeSpec._

class TreeSpec extends WordSpec with Matchers {

  "Tree" should {

    "create an empty Binary Search Tree" in {
      val tree = Tree[Int]
      tree shouldBe Empty
    }

    "create a Binary Search Tree and insert 3 elements" in {
      val tree = Tree[Int].insert(5).insert(3).insert(7)
      tree shouldBe tree3Elements
    }

    "search a Binary Search Tree" in {
      val tree = Tree[Int].insert(5).insert(3).insert(7)
      tree.search(5) shouldBe true
      tree.search(3) shouldBe true
      tree.search(7) shouldBe true
      tree.search(2) shouldBe false
      tree.search(4) shouldBe false
      tree.search(8) shouldBe false
    }

  }

}

object TreeSpec {

  val tree3Elements =
    Node(5,
      left = Node(3, Empty, Empty),
      right = Node(7, Empty, Empty)
    )

}