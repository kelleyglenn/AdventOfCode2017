package day10

object KnotHash {
  def hash(count: Int, swapsString: String): String = {
    val swaps: List[Int] = swapsString.map((c: Char) => c.toInt).toList ++ List(17, 31, 73, 47, 23)
    val linkedList = new CircularLinkedList(count)
    var skipSize = 0
    (1 to 64).foreach { _: Int =>
      skipSize = swapAndSkip(linkedList, swaps, skipSize)
    }
    val sparseHash: List[Int] = linkedList.toList
    val grouped: List[List[Int]] = sparseHash.grouped(Math.sqrt(count).toInt).toList
    val denseHash: List[Int] = grouped.map(xor)
    hexString(denseHash)
  }

  def hexString(denseHash: List[Int]): String = {
    denseHash.map((i: Int) => ("0" + i.toHexString).takeRight(2)).mkString
  }

  def xor(l: List[Int]): Int = {
    l.foldLeft(0) { case (result, number) => result ^ number }
  }

  def firstTwoNumbersAfterSwaps(count: Int, swaps: Iterable[Int]): (Int, Int) = {
    val list = new CircularLinkedList(count)
    swapAndSkip(list, swaps, 0)
    (list.head.number, list.head.next.get.number)
  }

  private def swapAndSkip(linkedList: CircularLinkedList, swaps: Iterable[Int], skips: Int): Int = {
    var skipSize: Int = skips
    swaps.foreach((swap: Int) => {
      linkedList.swapAndSkip(swap)
      linkedList.skip(skipSize)
      skipSize += 1
    })
    skipSize
  }

  class CircularLinkedList(count: Int) {
    var curNode: Node = {
      val nodes: Iterable[Node] = (0 until count).map((n: Int) => new Node(n))
      linkNodes(nodes)
      nodes.last.linkToNext(nodes.head)
      nodes.head
    }
    val head: Node = curNode

    def swapAndSkip(count: Int): Unit = {
      curNode = swapAndSkip(curNode, count)
    }

    private def swapAndSkip(from: Node, count: Int): Node = {
      if (count == 1) from.next.get
      else if (count > 1) {
        var starting: Node = from
        var ending: Node = skipFrom(starting, count - 1)
        val skipTo: Node = ending.next.get
        (1 to (count / 2)).foreach { _: Int =>
          val startingNumber: Int = starting.number
          starting.number = ending.number
          ending.number = startingNumber
          starting = starting.next.get
          ending = ending.prev.get
        }
        skipTo
      } else from
    }

    private def skipFrom(from: Node, count: Int): Node = {
      var cur: Node = from
      (1 to count).foreach((_: Int) => cur = cur.next.get)
      cur
    }

    def skip(count: Int): Unit = {
      curNode = skipFrom(curNode, count)
    }

    def toList: List[Int] = {
      var numbers: List[Int] = List.empty
      var n: Node = head
      numbers = numbers.appended(n.number)
      n = n.next.get
      while (n != head) {
        numbers = numbers.appended(n.number)
        n = n.next.get
      }
      numbers
    }

    @scala.annotation.tailrec
    private def linkNodes(nodes: Iterable[Node]): Unit = {
      if (nodes.size > 1) {
        nodes.head.linkToNext(nodes.tail.head)
        linkNodes(nodes.tail)
      }
    }

    class Node(var number: Int) {
      var next: Option[Node] = None
      var prev: Option[Node] = None

      def linkToNext(next: Node): Unit = {
        this.next = Some(next)
        next.prev = Some(this)
      }
    }

  }

}
