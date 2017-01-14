import org.scalatest.FlatSpec
import ClueType._

class GridTests extends FlatSpec {

  private final val Input =
    """
      |x------x-----
      |-x-x-x-x-x-x-
      |---------x---
      |-x-x-xxx-x-x-
      |---x---------
      |-xxx-x-xxx-x-
      |---x-----x---
      |-x-xxx-x-xxx-
      |---------x---
      |-x-x-xxx-x-x-
      |---x---------
      |-x-x-x-x-x-x-
      |-----x------x
    """.stripMargin

  private final val Lines = Input split "\n" map (_.trim) filter (_.nonEmpty)

  private final val Clues = Seq(
    Clue(ACROSS, 1, "Small wheel", 6, Coords(1, 0), Set()),
    Clue(ACROSS, 5, "Be burdensome", 5, Coords(8, 0), Set()),
    Clue(ACROSS, 9, "Rules of behaviour", 9, Coords(0, 2), Set()),
    Clue(ACROSS, 10, "Common disease", 3, Coords(10, 2), Set()),
    Clue(ACROSS, 11, "Animal collection", 3, Coords(0, 4), Set()),
    Clue(ACROSS, 12, "Arouse passion in", 9, Coords(4, 4), Set()),
    Clue(ACROSS, 14, "Runner", 3, Coords(0, 6), Set()),
    Clue(ACROSS, 16, "Hot drink", 5, Coords(4, 6), Set()),
    Clue(ACROSS, 18, "Attention", 3, Coords(10, 6), Set()),
    Clue(ACROSS, 19, "Keep trying", 9, Coords(0, 8), Set()),
    Clue(ACROSS, 21, "Fluffy scarf", 3, Coords(10, 8), Set()),
    Clue(ACROSS, 22, "Eliminate", 3, Coords(0, 10), Set()),
    Clue(ACROSS, 23, "Try out (car)", 9, Coords(4, 10), Set()),
    Clue(ACROSS, 25, "Spiritualist's board", 5, Coords(0, 12), Set()),
    Clue(ACROSS, 26, "Allocate funds", 6, Coords(6, 12), Set()),
    Clue(DOWN, 2, "Comrade", 5, Coords(2, 0), Set()),
    Clue(DOWN, 3, "Holidaymaker", 7, Coords(4, 0), Set()),
    Clue(DOWN, 4, "Routine", 3, Coords(6, 0), Set()),
    Clue(DOWN, 5, "Seize forcibly", 5, Coords(8, 0), Set()),
    Clue(DOWN, 6, "Make bigger", 7, Coords(10, 0), Set()),
    Clue(DOWN, 7, "Burglar", 12, Coords(12, 0), Set()),
    Clue(DOWN, 8, "Female singing voice", 12, Coords(0, 1), Set()),
    Clue(DOWN, 13, "Avoid, sweet", 5, Coords(6, 4), Set()),
    Clue(DOWN, 15, "Tel Aviv native", 7, Coords(2, 6), Set()),
    Clue(DOWN, 17, "Gave up", 7, Coords(8, 6), Set()),
    Clue(DOWN, 20, "Minor actor", 5, Coords(4, 8), Set()),
    Clue(DOWN, 21, "Table covering material", 5, Coords(10, 8), Set()),
    Clue(DOWN, 24, "Cry", 3, Coords(6, 10), Set())
  )

  "Grid#fromLines" should "correctly create a grid with the correct across clue lengths" in {
    val grid = Grid.fromLines(Lines)
    for {
      clue <- Clues
      if clue.clueType == ACROSS
    } yield assert(grid.answerLength(ACROSS, clue.coords.x, clue.coords.y) == clue.length)
  }

  "Grid#fromLines" should "correctly create a grid with the correct down clue lengths" in {
    val grid = Grid.fromLines(Lines)
    for {
      clue <- Clues
      if clue.clueType == DOWN
    } yield assert(grid.answerLength(DOWN, clue.coords.x, clue.coords.y) == clue.length)
  }
}
