import java.security.MessageDigest

object Day4 {
  def md5(s: String) =
    MessageDigest.getInstance("MD5").digest(s.getBytes).map("%02X".format(_)).mkString

  def matches(s: String, zeroes: Int) = s.startsWith("0" * zeroes)

  def main(args: Array[String]) {
    val input = "iwrupvqb"
    val fiveZeroes = Stream.iterate(0)(_ + 1).find(key => matches(md5(input + key), 5))
    println(fiveZeroes)
    println(Stream.iterate(fiveZeroes.getOrElse(0))(_ + 1).find(key => matches(md5(input + key), 6)))
  }
}
