package tree

import org.scalatest.{Matchers, WordSpec}
import TreeSpec._

class TreeSpec extends WordSpec with Matchers {

  "Tree" should {

    "create an empty Binary Search Tree" in {
      val tree = Tree[Int]
      tree shouldBe Empty
    }

    "create an Binary Search Tree and insert 3 elements" in {
      val tree = Tree[Int].insert(5).insert(3).insert(7)
      tree shouldBe tree3Elements
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