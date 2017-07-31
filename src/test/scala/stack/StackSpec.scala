package stack

import org.scalatest.{Matchers, WordSpec}

class StackSpec extends WordSpec with Matchers {

  "Stack" should {

    "create an empty Stack" in {
      val stack = Stack()
      stack.size shouldBe 0
    }

    "push onto Stack" in {
      val stack = Stack()

      val stack1 = stack.push(22)
      stack1.size shouldBe 1

      val stack2 = stack1.push(33)
      stack2.size shouldBe 2
    }

    "push and pop from Stack" in {
      val stack = Stack()

      val stack1 = stack.push(22)
      val stack2 = stack1.push(33)

      val (res1, stack3) = stack2.pop
      val (res2, stack4) = stack3.pop
      val (res3, stack5) = stack4.pop

      res1 shouldBe Some(33)
      stack3.size shouldBe 1

      res2 shouldBe Some(22)
      stack4.size shouldBe 0

      res3 shouldBe None
      stack5.size shouldBe 0
    }

  }

}