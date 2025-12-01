//> using dep com.lihaoyi::os-lib:0.11.6

// val input =
//   """L68
// L30
// R48
// L5
// R60
// L55
// L1
// L99
// R14
// L82""".linesIterator.map(_.trim).filterNot(_.isEmpty).toList

val input = os.read.lines(os.pwd / "input.txt").map(_.trim).filterNot(_.isEmpty).toList

case class Movement(direction: Int, distance: Int)

val moves = input.map {
  case s"L$steps" => Movement(-1, steps.toInt)
  case s"R$steps" => Movement(1, steps.toInt)
}

val init = 50

moves
  .view
  .scanLeft(init) { case (state, move) => (state + move.direction * move.distance) % 100 }
  .count(_ == 0)

moves
  .view
  .flatMap { m =>
    List.fill(m.distance)(Movement(m.direction, 1))
  }
  .scanLeft(init) { case (state, move) => (state + move.direction * move.distance) % 100 }
  .count(_ == 0)
