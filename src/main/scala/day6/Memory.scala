package day6

object Memory {
  def cyclesUntilRepeat(memory: List[Int]): Int = {
    var curMemory = memory
    var states: Set[List[Int]] = Set.empty
    while (!states.contains(curMemory)) {
      states = states + curMemory
      curMemory = reallocate(curMemory)
    }
    states.size
  }

  def reallocate(memory: List[Int]): List[Int] = {
    val withIndex = memory.zipWithIndex
    val sourceIndex = withIndex.foldLeft((Int.MinValue, Int.MaxValue)) {
      case ((maxValue, minIndex), (curValue, curIndex)) =>
        if (curValue > maxValue) (curValue, curIndex)
        else (maxValue, minIndex)
    }._2
    val baseAmount: Int = memory(sourceIndex) / memory.size
    val remainder: Int = memory(sourceIndex) % memory.size
    val ctAfterSource = memory.length - sourceIndex - 1
    withIndex.map { case (origValue, i) =>
      baseAmount +
        (if (i == sourceIndex) 0 else origValue) +
        (if (remainder >= ((i + ctAfterSource) % memory.length + 1)) 1 else 0)
    }
  }

  def sizeOfLoop(memory: List[Int]): Int = {
    var curMemory = memory
    var curCycle = 0
    var states: Map[List[Int], Int] = Map.empty
    while (!states.contains(curMemory)) {
      states = states + (curMemory -> curCycle)
      curCycle += 1
      curMemory = reallocate(curMemory)
    }
    states.size - states(curMemory)
  }
}