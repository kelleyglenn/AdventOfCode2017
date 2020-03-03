package day4

object Passphrases {
  def countNoDuplicates(passphrases: Iterable[String]): Int = {
    passphrases.count(containsNoDuplicateWords)
  }

  def containsNoDuplicateWords(passphrase: String): Boolean = {
    val words = passphrase.split(' ')
    words.length == words.toSet.size
  }

  def countNoAnagrams(passphrases: Iterable[String]): Int = {
    passphrases.count(containsNoAnagrams)
  }

  def containsNoAnagrams(passphrase: String): Boolean = {
    val words = passphrase.split(' ').toList.map(_.toList.sorted.mkString)
    words.length == words.toSet.size
  }
}
