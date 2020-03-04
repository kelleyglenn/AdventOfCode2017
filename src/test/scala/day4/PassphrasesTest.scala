package day4

import org.scalatest.flatspec.AnyFlatSpec
import util.SetupPuzzleData

class PassphrasesTest extends AnyFlatSpec {
  behavior of "containsNoDuplicateWords"
  it should "handle the examples" in {
    assert(Passphrases.containsNoDuplicateWords("aa bb cc dd ee"))
    assert(!Passphrases.containsNoDuplicateWords("aa bb cc dd aa"))
    assert(Passphrases.containsNoDuplicateWords("aa bb cc dd aaa"))
  }
  behavior of "countNoDuplicates"
  it should "solve the first puzzle" in new SetupPuzzleData("input") {
    assert(Passphrases.countNoDuplicates(lines) == 477)
  }
  behavior of "containsNoAnagrams"
  it should "handle the examples" in {
    assert(Passphrases.containsNoAnagrams("abcde fghij"))
    assert(!Passphrases.containsNoAnagrams("abcde xyz ecdab"))
    assert(Passphrases.containsNoAnagrams("a ab abc abd abf abj"))
    assert(Passphrases.containsNoAnagrams("iiii oiii ooii oooi oooo"))
    assert(!Passphrases.containsNoAnagrams("oiii ioii iioi iiio"))
  }
  behavior of "countNoAnagrams"
  it should "solve the second puzzle" in new SetupPuzzleData("input") {
    assert(Passphrases.countNoAnagrams(lines) == 167)
  }
}
