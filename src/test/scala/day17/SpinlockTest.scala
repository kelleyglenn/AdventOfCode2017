package day17

import org.scalatest.flatspec.AnyFlatSpec

class SpinlockTest extends AnyFlatSpec {
  behavior of "nextAfterInsertions"
  it should "handle the examples" in {
    assert(Spinlock.nextAfterInsertions(10, 3) == 5)
    assert(Spinlock.nextAfterInsertions(2018, 3) == 638)
  }
  it should "solve the first puzzle" in {
    assert(Spinlock.nextAfterInsertions(2018, 376) == 777)
  }
  behavior of "after0AfterInsertionsSlow"
  it should "handle the examples" in {
    assert(Spinlock.after0AfterInsertionsSlow(10, 3) == 9)
  }
  behavior of "after0AfterInsertions"
  it should "give the same answers as after0AfterInsertionsSlow" in {
    assert(Spinlock.after0AfterInsertions(1, 3) ==
      Spinlock.after0AfterInsertionsSlow(1, 3))
    assert(Spinlock.after0AfterInsertions(10, 3) ==
      Spinlock.after0AfterInsertionsSlow(10, 3))
    assert(Spinlock.after0AfterInsertions(10, 376) ==
      Spinlock.after0AfterInsertionsSlow(10, 376))
    assert(Spinlock.after0AfterInsertions(1000, 376) ==
      Spinlock.after0AfterInsertionsSlow(1000, 376))
  }
  it should "solve the second puzzle" in {
    assert(Spinlock.after0AfterInsertions(50000001, 376) == 39289581)
  }
}
