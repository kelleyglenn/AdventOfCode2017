package day17

object Spinlock {
  def nextAfterInsertions(insertionCt: Int, stepsPerInsert: Int): Int = {
    doInsertions(insertionCt, stepsPerInsert).next
  }

  private def doInsertions(insertionCt: Int, stepsPerInsert: Int): SteppingInserter[Int] = {
    val inserter: SteppingInserter[Int] = SteppingInserter(0, stepsPerInsert)
    (1 until insertionCt).foreach { i => inserter.stepAndInsert(i) }
    inserter
  }

  def after0AfterInsertionsSlow(insertionCt: Int, stepsPerInsert: Int): Int = {
    doInsertions(insertionCt, stepsPerInsert).after0
  }

  def after0AfterInsertions(insertionCt: Int, stepsPerInsert: Int): Int = {
    var lastInsertedIndex = 0
    var nodeCount = 1
    var curValueAfter0 = 0
    (1 until insertionCt).foreach { i =>
      lastInsertedIndex = ((lastInsertedIndex + stepsPerInsert) % nodeCount) + 1
      nodeCount += 1
      if (lastInsertedIndex == 1) curValueAfter0 = i
    }
    curValueAfter0
  }

  case class SteppingInserter[A](initialValue: A, stepsPerInsert: Int) {
    val zeroNode: Node = Node(initialValue)
    var curNode: Node = zeroNode
    var nodeCount = 1
    curNode.next = curNode

    def stepAndInsert(newValue: A): Unit = {
      (1 to stepsPerInsert % nodeCount).foreach { _ => curNode = curNode.next }
      val newNode = Node(newValue)
      newNode.next = curNode.next
      curNode.next = newNode
      curNode = newNode
      nodeCount += 1
    }

    def next: A = {
      curNode.next.value
    }

    def after0: A = {
      zeroNode.next.value
    }

    case class Node(value: A) {
      var next: Node = _
    }

  }

}
