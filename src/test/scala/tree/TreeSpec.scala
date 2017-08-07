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

    "delete element with no left child from a Binary Search Tree" in {
      val tree = Tree[Int].insert(5).insert(3).insert(7)
      tree.delete(3) shouldBe tree2ElementsR
    }

    "delete element with no right child from a Binary Search Tree" in {
      val tree = Tree[Int].insert(5).insert(3).insert(7)
      tree.delete(7) shouldBe tree2ElementsL
    }

    "delete element with 2 children a Binary Search Tree" in {
      val tree = treeBig.copy()
      tree.delete(12) shouldBe treeBigMinus12
    }

  }

}

object TreeSpec {

  val tree3Elements =
    Node(5,
      left = Node(3, Empty, Empty),
      right = Node(7, Empty, Empty)
    )

  val tree2ElementsR =
    Node(5,
      left = Empty,
      right = Node(7, Empty, Empty)
    )

  val tree2ElementsL =
    Node(5,
      left = Node(3, Empty, Empty),
      right = Empty
    )

  val treeBig =
    Node(5,
      Node(2,
        Node(-4 , Empty, Empty),
        Node(3 , Empty, Empty)
      ),
      Node(12,
        Node(9, Empty, Empty),
        Node(21,
          Node(19, Empty, Empty),
          Node(25, Empty, Empty)
        )
      )
    )

  val treeBigMinus12 =
    Node(5,
      Node(2,
        Node(-4 , Empty, Empty),
        Node(3 , Empty, Empty)
      ),
      Node(19,
        Node(9, Empty, Empty),
        Node(21,
          Empty,
          Node(25, Empty, Empty)
        )
      )
    )

}