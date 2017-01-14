import org.scalatest.FlatSpec
import ClueType._

class GridTests extends FlatSpec {

  private final val Lines =
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
    """.stripMargin split "\n" map (_.trim) filter (_.nonEmpty)

  private final val ExampleGrid = Grid.fromLines(Lines)

  private final val ExampleClues = Seq(
    Clue(ACROSS, 1, "Small wheel", 6),
    Clue(ACROSS, 5, "Be burdensome", 5),
    Clue(ACROSS, 9, "Rules of behaviour", 9),
    Clue(ACROSS, 10, "Common disease", 3),
    Clue(ACROSS, 11, "Animal collection", 3),
    Clue(ACROSS, 12, "Arouse passion in", 9),
    Clue(ACROSS, 14, "Runner", 3),
    Clue(ACROSS, 16, "Hot drink", 5),
    Clue(ACROSS, 18, "Attention", 3),
    Clue(ACROSS, 19, "Keep trying", 9),
    Clue(ACROSS, 21, "Fluffy scarf", 3),
    Clue(ACROSS, 22, "Eliminate", 3),
    Clue(ACROSS, 23, "Try out (car)", 9),
    Clue(ACROSS, 25, "Spiritualist's board", 5),
    Clue(ACROSS, 26, "Allocate funds", 6),
    Clue(DOWN, 2, "Comrade", 5),
    Clue(DOWN, 3, "Holidaymaker", 7),
    Clue(DOWN, 4, "Routine", 3),
    Clue(DOWN, 5, "Seize forcibly", 5),
    Clue(DOWN, 6, "Make bigger", 7),
    Clue(DOWN, 7, "Burglar", 12),
    Clue(DOWN, 8, "Female singing voice", 12),
    Clue(DOWN, 13, "Avoid, sweet", 5),
    Clue(DOWN, 15, "Tel Aviv native", 7),
    Clue(DOWN, 17, "Gave up", 7),
    Clue(DOWN, 20, "Minor actor", 5),
    Clue(DOWN, 21, "Table covering material", 5),
    Clue(DOWN, 24, "Cry", 3)
  )

  "Grid#fromLines" should "create a grid with the correct across clue numbers" in {
    assert(ExampleGrid.AcrossNumbersToCoords.keys.toSet == ExampleClues.filter(_.clueType == ACROSS).map(_.number).toSet)
  }

  "Grid#fromLines" should "create a grid with the correct down clue numbers" in {
    assert(ExampleGrid.DownNumbersToCoords.keys.toSet == ExampleClues.filter(_.clueType == DOWN).map(_.number).toSet)
  }

  "Grid#fromLines" should "create a grid with the correct clue intersections for 1a" in {
    val actual = ExampleGrid.getIntersectionsForAcrossClue(1).sortBy(_.acrossIndex)
    val expected = Seq(
      Intersection(1, 2, 0),
      Intersection(3, 3, 0),
      Intersection(5, 4, 0))
    assert(actual == expected)
  }

  "Grid#fromLines" should "create a grid with the correct clue intersections for 12a" in {
    val actual = ExampleGrid.getIntersectionsForAcrossClue(12).sortBy(_.acrossIndex)
    val expected = Seq(
      Intersection(0, 3, 4),
      Intersection(2, 13, 0),
      Intersection(4, 5, 4),
      Intersection(6, 6, 4),
      Intersection(8, 7, 4))
    assert(actual == expected)
  }
}
