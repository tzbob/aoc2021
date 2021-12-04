def q3 =
  val lines = Input.readLines("q3")

  def columns(lines: List[String]) =
    val cells = lines.map(_.toCharArray.toList)
    cells.transpose

  def mostCommon(c: List[Char]) = c.count(_ == '1') >= c.count(_ == '0')

  val p1 =
    val gammaList   = columns(lines).map(mostCommon)
    val epsilonList = gammaList.map(!_)

    def bitListToInt(bl: List[Boolean]) =
      val bitString = bl.map { if _ then "1" else "0" }.mkString
      Integer.parseInt(bitString, 2)

    bitListToInt(gammaList) * bitListToInt(epsilonList)

  val p2 =
    def narrowLines(deriveCommon: List[Char] => Boolean) =
      def narrowLinesRec(idx: Int, currentLines: List[String]): String =
        val bit       = deriveCommon(columns(currentLines)(idx))
        val goodStart = if bit then '1' else '0'
        val nextLines = currentLines.filter(_(idx) == goodStart)
        if nextLines.size == 1 then nextLines.head
        else narrowLinesRec(idx + 1, nextLines)
      narrowLinesRec(0, lines)

    val oxyString = narrowLines(mostCommon)
    val oxy       = Integer.parseInt(oxyString, 2)

    val coString = narrowLines(c => !mostCommon(c))
    val co       = Integer.parseInt(coString, 2)

    oxy * co

  s"Part 1: $p1, Part 2: $p2"
