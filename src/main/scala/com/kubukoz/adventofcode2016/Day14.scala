package com.kubukoz.adventofcode2016

import com.kubukoz.adventofcode2016.Commons._

object Day14 {
  def md5Times(s: String, times: Int): String = (0 until times).foldLeft(s) { case (a, _) => md5(a).toLowerCase }

  def keys(salt: String, times: Int): Stream[Int] = {
    val hashes = Stream.iterate(0)(_ + 1).map(i => md5Times(salt + i, times))

    hashes.zipWithIndex.collect {
      case (firstTripleInHash(charAsFive), key)
        if hashes.slice(key + 1, key + 1001)
          .exists(_.contains(charAsFive)) => key
    }
  }

  object firstTripleInHash {
    def unapply(hash: String): Option[String] = {
      hash.sliding(3).find(_.distinct.length == 1).map(_.head.toString * 5)
    }
  }

  def hasSame(count: Int)(hash: String): Boolean = hash.sliding(count).exists(_.distinct.length == 1)

  def main(args: Array[String]): Unit = {
    println(keys("ahsbgdzn", times = 1)(63))
    println(keys("ahsbgdzn", times = 2017)(63))
  }
}
