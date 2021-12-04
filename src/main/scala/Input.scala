import scala.io.Source
import scala.util.{Try, Using}

object Input:
  def readLines(name: String): List[String] =
    Source.fromFile(s"src/main/resources/$name.txt").getLines().toList
//    Using(Source.fromFile(s"src/main/resources/$name.txt")) { source =>
//      source.getLines().toList
//    }
