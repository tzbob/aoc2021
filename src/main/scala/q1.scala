def q1 =
  val ints = Input.readLines("q1").map(_.toInt)

  def increments(slide: List[List[Int]]) =
    slide.collect { case Seq(a, b) if a < b => 1 }

  val p1 =
    val slide = ints.sliding(2).toList
    increments(slide).sum

  val p2 =
    val slide      = ints.sliding(3).toList
    val slidingSum = slide.map(_.sum)
    increments(slidingSum.sliding(2).toList).sum

  s"Part 1: $p1, Part 2: $p2"
