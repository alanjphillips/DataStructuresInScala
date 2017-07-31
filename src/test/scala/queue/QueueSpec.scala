package queue

import org.scalatest.{Matchers, WordSpec}

class QueueSpec extends WordSpec with Matchers {

  "Queue" should {

    "create an empty Queue" in {
      val queue = Queue()
      queue.size shouldBe 0
    }

    "enqueue on Queue" in {
      val queue = Queue()

      val queue1 = queue.enqueue(22)
      queue1.size shouldBe 1

      val queue2 = queue1.enqueue(33)
      queue2.size shouldBe 2
    }

    "enqueue and then dequeue" in {
      val queue = Queue()

      val queue1 = queue.enqueue(22)
      val queue2 = queue1.enqueue(33)

      val (res1, queue3) = queue2.dequeue
      val (res2, queue4) = queue3.dequeue
      val (res3, queue5) = queue4.dequeue

      res1 shouldBe Some(22)
      queue3.size shouldBe 1

      res2 shouldBe Some(33)
      queue4.size shouldBe 0

      res3 shouldBe None
      queue5.size shouldBe 0
    }

  }

}