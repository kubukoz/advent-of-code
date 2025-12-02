val input =
  "385350926-385403705,48047-60838,6328350434-6328506208,638913-698668,850292-870981,656-1074,742552-796850,4457-6851,138-206,4644076-4851885,3298025-3353031,8594410816-8594543341,396-498,1558-2274,888446-916096,12101205-12154422,2323146444-2323289192,37-57,101-137,46550018-46679958,79-96,317592-341913,495310-629360,33246-46690,14711-22848,1-17,2850-4167,3723700171-3723785996,190169-242137,272559-298768,275-365,7697-11193,61-78,75373-110112,425397-451337,9796507-9899607,991845-1013464,77531934-77616074"
//val input = "11-22,95-115,998-1012,1188511880-1188511890,222220-222224,1698522-1698528,446443-446449,38593856-38593862,565653-565659,824824821-824824827,2121212118-2121212124"

val ranges =
  input
    .split(",")
    .map { case s"$from-$to" => from.toLong to to.toLong }
    .toList

def isValid(number: Long) = {
  val str = number.toString

  if str.length % 2 == 1 then true
  else {
    val n = str.length / 2

    val (before, after) = str.splitAt(n)

    before != after
  }
}

def isValid2(number: Long) = {
  val str = number.toString

  val subsequenceSizes = (1 until str.length).filter(str.length % _ == 0)

  !subsequenceSizes.exists { subSize =>
    val fst = str.take(subSize)
    str.sliding(subSize, subSize).forall(_ == fst)
  }
}

println(ranges.view.flatMap(_.filterNot(isValid)).sum)
println(ranges.view.flatMap(_.filterNot(isValid2)).sum)
