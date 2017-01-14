import ClueType._

class Grid(size: Int, spaces: Set[Coords]) {

  def answerLength(clueType: ClueType.Value, x: Int, y: Int): Int = {
    val coords = clueType match {
      case ACROSS => for { col <- x until size } yield Coords(col, y)
      case DOWN => for { row <- y until size } yield Coords(x, row)
    }
    coords prefixLength spaces.contains
  }
}

object Grid {

  private final val SPACE = '-'

  def fromLines(lines: Seq[String]): Grid = {
    val size = lines.length
    val allCoords = for {
      x <- 0 until size
      y <- 0 until size
    } yield Coords(x, y)
    val spaces = allCoords filter (coords => lines(coords.y)(coords.x) == SPACE)
    new Grid(size, spaces.toSet)
  }
}
