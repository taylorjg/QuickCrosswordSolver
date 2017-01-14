case class Clue(clueType: ClueType.Value,
                number: Int,
                text: String,
                length: Int,
                coords: Coords,
                possibleAnswers: Set[String])
