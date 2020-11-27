package com.kubukoz

import zio.Has

package object aoc {
  type ZUtil = Has[ZUtil.Service]
}
