import scala.annotation.tailrec

object Day25 {
  @tailrec
  def point(x: Int, y: Int, mem: Int = 0): Int = {
    require(x >= 1 && y >= 1)
    x == 1 match {
      case true if y == 1 => mem + 1
      case true => point(y - 1, 1, mem+1)
      case _ => point(x - 1, y + 1, mem+1)
    }
  }

  @tailrec
  def num(i: Int, temp: Long = 20151125): Long = i match{
    case 1 => temp
    case _ => num(i-1, compute(temp))
  }

  def compute(i: Long): Long = (i * 252533) % 33554393

  def main(args: Array[String]) {
    //part 1
    val (x, y) = (3029, 2947)
    val index = point(x, y)
    println(index)
    println(num(index))
  }
}
