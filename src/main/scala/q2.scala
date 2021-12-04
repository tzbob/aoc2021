case class Position(x: Int, depth: Int, aim: Int):
  val result = x * depth
object Position { def zero = Position(0, 0, 0) }

enum Command:
  case Forward(scalar: Int)
  case Down(scalar: Int)
  case Up(scalar: Int)

object Command:
  def fromLine(str: String) =
    val List(command, strValue) = str.split(" ").toList
    val scalar                  = strValue.toInt
    command match
      case "forward" => Forward(scalar)
      case "down"    => Down(scalar)
      case "up"      => Up(scalar)

  def p1(p: Position, command: Command) = command match
    case Forward(v) => p.copy(x = p.x + v)
    case Down(v)    => p.copy(depth = p.depth + v)
    case Up(v)      => p.copy(depth = p.depth - v)

  def p2(p: Position, command: Command) = command match
    case Down(v)    => p.copy(aim = p.aim + v)
    case Up(v)      => p.copy(aim = p.aim - v)
    case Forward(v) => p.copy(x = p.x + v, depth = p.depth + p.aim * v)

val q2 =
  val commands = Input.readLines("q2").map(Command.fromLine)
  val p1       = commands.foldLeft(Position.zero)(Command.p1)
  val p2       = commands.foldLeft(Position.zero)(Command.p2)
  s"Part 1: ${p1.result}, Part 2: ${p2.result}"
