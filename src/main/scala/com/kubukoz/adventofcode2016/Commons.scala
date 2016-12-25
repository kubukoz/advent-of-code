package com.kubukoz.adventofcode2016

object Commons {
  private val md = java.security.MessageDigest.getInstance("MD5")
  def md5(s: String): String = "%1$032X".format(BigInt(1, md.digest(s.getBytes)))
}
