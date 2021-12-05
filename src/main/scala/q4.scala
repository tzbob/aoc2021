def q4 =
  val drawInput :: _ :: boardInput = Input.readLines("q4")
  val drawnNumbers                 = drawInput.split(",").toList.map(_.toInt)

  case class Board(rows: List[List[Int]]):
    val rowSets = rows.map(_.toSet)
    val colSets = rows.transpose.map(_.toSet)

  object Board:
    def won(board: Board, draw: List[Int]) =
      board.rowSets.exists(_.subsetOf(draw.toSet)) || board.colSets.exists(_.subsetOf(draw.toSet))

    def score(board: Board, draw: List[Int]) =
      val leftOvers = board.rows.flatten.toSet.diff(draw.toSet)
      leftOvers.sum * draw.last

    def fromLines(lines: List[String]) = Board(lines.map { x =>
      val list = x.trim.split("\\s+").toList
      list.map(_.toInt)
    })

  def makeBoards(input: List[String]): List[Board] = input match
    // remove whitespace lead
    case _ :: boardInput =>
      val (boardLines, rest) = boardInput.span(!_.isEmpty)
      Board.fromLines(boardLines) :: makeBoards(rest)
    case Nil => Nil

  val boards       = makeBoards(boardInput)
  val drawLazyList = drawnNumbers.inits.toSeq.reverseIterator.to(LazyList)
  val boardResults = drawLazyList.map(draw => draw -> boards.find(Board.won(_, draw)))

  val p1 =
    val Some((draw, Some(firstWin))) = boardResults.find(_._2.isDefined)
    Board.score(firstWin, draw)

  val p2 =
    val result = drawLazyList.scanLeft(List.empty[Int] -> List.empty[Board] -> boards) { case ((_, boards), draw) =>
      val (winners, leftovers) = boards.partition(Board.won(_, draw))
      draw -> winners -> leftovers
    }
    val Some((draw, List(lastWin)), _) = result.findLast { case ((draw, winners), _) => !winners.isEmpty }
    Board.score(lastWin, draw)

  s"Part 1: $p1, Part 2: $p2"
