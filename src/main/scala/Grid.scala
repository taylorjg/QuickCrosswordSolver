import ClueType._

class Grid(size: Int, spaces: Set[Coords]) {

  def answerLength(clueType: ClueType.Value, x: Int, y: Int): Int = {
    val coords = clueType match {
      case ACROSS => for { col <- x until size } yield Coords(col, y)
      case DOWN => for { row <- y until size } yield Coords(x, row)
    }
    coords prefixLength spaces.contains
  }

  private final val LastRow = size - 1
  private final val LastCol = size - 1

  private val clueNumbersToCoords = {
    val sortedSpaces = spaces.toList.sortBy(coords => coords.y * size + coords.x)
    val finalAcc = sortedSpaces.foldLeft((1, Map.empty[Int, Coords], Map.empty[Int, Coords])) {
      case ((nextNumber, acrossMap, downMap), coords) =>
        val x = coords.x
        val y = coords.y
        def isWall(x: Int, y: Int) = !spaces.contains(Coords(x, y))
        def leftIsWall = x == 0 || isWall(x - 1, y)
        def rightIsWall = x == LastCol || isWall(x + 1, y)
        def aboveIsWall = y == 0 || isWall(x, y - 1)
        def belowIsWall = y == LastRow || isWall(x, y + 1)
        val newAcrossClue = leftIsWall && !rightIsWall
        val newDownClue = aboveIsWall && !belowIsWall
        val newNextNumber = if (newAcrossClue || newDownClue) nextNumber + 1 else nextNumber
        val newAcrossMap = if (newAcrossClue) acrossMap + (nextNumber -> coords) else acrossMap
        val newDownMap = if (newDownClue) downMap + (nextNumber -> coords) else downMap
        (newNextNumber, newAcrossMap, newDownMap)
    }
    (finalAcc._2, finalAcc._3)
  }

  final val AcrossNumbersToCoords = clueNumbersToCoords._1
  final val DownNumbersToCoords = clueNumbersToCoords._2
}

object Grid {

  private final val SPACE = '-'

  def fromLines(lines: Seq[String]): Grid = {
    val size = lines.length
    val allCoords = for {
      y <- 0 until size
      x <- 0 until size
    } yield Coords(x, y)
    val spaces = allCoords filter (coords => lines(coords.y)(coords.x) == SPACE)
    new Grid(size, spaces.toSet)
  }
}
