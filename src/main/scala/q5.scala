def q5 =
  val rawLines = Input.readLines("q5")

  case class Position(x: Int, y: Int)
  case class Segment(a: Position, b: Position):
    val vertical   = a.x == b.x
    val horizontal = a.y == b.y

    val straights = if vertical then
      val yRange = Math.min(a.y, b.y) to Math.max(a.y, b.y)
      yRange.map(Position(a.x, _))
    else if horizontal then
      val xRange = Math.min(a.x, b.x) to Math.max(a.x, b.x)
      xRange.map(Position(_, a.x))
    else Nil

    // diagonals are wrong
    val diagonals = if !vertical && !horizontal then
      val dx = (b.x - a.x).sign
      val dy = (b.y - a.y).sign

      val xs = LazyList.from(a.x, dx)
      val ys = LazyList.from(a.y, dy)

      xs.zip(ys).map { case (x, y) => Position(x, y) }.takeWhile(_ != b)
    else Nil

  val segments = rawLines.map { raw =>
    val Array(p1R, p2R) = raw.split("\\s+->\\s+")
    def pToPosition(p: String) =
      val Array(x, y) = p.split(",")
      Position(x.toInt, y.toInt)
    Segment(pToPosition(p1R), pToPosition(p2R))
  }

  val straights = segments.map(_.straights).flatten
  def moreThanTwoCount[A](xs: List[A]) =
    xs.groupBy(identity).values.map(_.size).count(_ >= 2)

  val p1 = moreThanTwoCount(straights)

  val p2 =
    val diagonals = segments.map(_.diagonals).flatten
    moreThanTwoCount(diagonals ++ straights)

  s"Part 1: $p1, Part 2: $p2"
