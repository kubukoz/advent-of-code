package com.kubukoz.adventofcode2018

import java.security.MessageDigest

object Commons {
  private val md = MessageDigest.getInstance("MD5")

  def md5(s: String): String = md5With(s, md)

  def md5Separated(s: String): String =
    md5With(s, MessageDigest.getInstance("MD5"))

  private def md5With(s: String, md: MessageDigest) =
    "%1$032X".format(BigInt(1, md.digest(s.getBytes)))
}
