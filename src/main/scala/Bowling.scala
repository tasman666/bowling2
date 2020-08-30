import scala.util.Random

object Bowling {

  val maxPinValue = 10

  trait RollResult {
    def value(): Int
    def desc(): String

    override def toString: String = desc()
  }

  case class Normal(pins: Int) extends RollResult {
    override def value(): Int = pins
    override def desc(): String = s"$pins"
  }

  case class Spare(pins: Int) extends RollResult {
    override def value(): Int = pins
    override def desc(): String = "/"
  }

  case object Strike extends RollResult {
    override def value(): Int = maxPinValue
    override def desc(): String = "X"
  }

  class Player(val name: String) {

    val maxNameSize = 10

    def roll(nrOfFrame: Int, previousRoll: Option[RollResult], rollFunction: (Int, Int) => Int): RollResult = {
      println(s"   Player $name rolling...")

      val previousValue = previousRoll.map(_.value()).getOrElse(0)
      val rollResult = rollFunction(nrOfFrame, previousValue)

      val result = if (rollResult == maxPinValue && previousRoll.isEmpty) {
        Strike
      } else {
        val values = previousValue + rollResult
        if (values == maxPinValue) {
          Spare(rollResult)
        } else {
          Normal(rollResult)
        }
      }
      println(s"   Player $name throws $result")
      result
    }

    def getDisplayName(): String = {
      name match {
        case n if n.length < maxNameSize =>
          val emptySpaces = " " * (maxNameSize - n.length)
          name + emptySpaces
        case _ => name.substring(0, maxNameSize + 1)
      }
    }
  }

  class Frame(val nr: Int, rolls: Map[Player, List[RollResult]]) {

    def getPoints(player: Player, nextRolls: List[RollResult]): Points =  {
      val playerRolls = getPlayerRollResults(player)
      val pointsDesc = playerRolls.map(_.desc()).mkString("|")

      val allPointsFromFrame = playerRolls.map(_.value()).sum

      val allPoints: Int = playerRolls match {
        case rolls if rolls.contains(Strike) => nextRolls.map(_.value()).sum + allPointsFromFrame
        case rolls if rolls.exists(result => result.isInstanceOf[Spare]) =>
          nextRolls.headOption.map(_.value()).getOrElse(0) + allPointsFromFrame
        case _ => allPointsFromFrame
      }

      new Points(nr, allPoints, pointsDesc)
    }

    def getPlayerRollResults(player: Player): List[RollResult] = {
      rolls.getOrElse(player, List())
    }
  }

  class Points(nrOfFrame: Int, val value: Int, desc: String) {
    def desc(): String = {
      s"$desc ($value)"
    }
  }

  class BowlingResultPerPlayer(player: Player, points: List[Points]) {

    def display(): String =  {
      val pointsForFramesDesc = points.map(_.desc())
      val allPoints = points.map(_.value).sum
      s"${player.getDisplayName()}: ${pointsForFramesDesc.mkString(" || ")} = $allPoints"
    }

  }

  class BowlingResult(playerResults: List[BowlingResultPerPlayer]) {

    def display(): String = {
      playerResults.map(_.display()).mkString("\n")
    }

  }

  def play(nrOfFrames: Int, players: List[Player], rollFunction: (Int, Int) => Int = randomRoll): BowlingResult = {
    val frameNrs = List.range(1, nrOfFrames + 1)

    val frames = frameNrs.map(nrOfFrame => {
      println(s"Lets start frame $nrOfFrame")
      playFrame(nrOfFrame, players, rollFunction)
    })

    getResult(players, frames)

  }

  private def randomRoll(frameNr: Int, previousValue: Int) = {
    Random.between(0, maxPinValue + 1  - previousValue)
  }

  private def playFrame(nrOfFrame: Int, players: List[Player], rollFunction: (Int, Int) => Int): Frame = {
    val playerResults = players.map( player => {
      val resultFirstRoll = player.roll(nrOfFrame, Option.empty, rollFunction)
      resultFirstRoll match {
        case Strike =>  (player, List(resultFirstRoll))
        case _ =>
          val resultSecondRoll = player.roll(nrOfFrame, Option(resultFirstRoll), rollFunction)
          (player, List(resultFirstRoll, resultSecondRoll))
      }
    }).toMap
    new Frame(nrOfFrame, playerResults)
  }

  private def getResult(players: List[Player], frames: List[Frame]): BowlingResult = {
    val resultsPerPlayer = players.map(player => {
          val points = frames.map(frame => {
            frame.getPoints(player, nextTwoRolls(player, frame, frames))
          })
          new BowlingResultPerPlayer(player, points)
    })

    new BowlingResult(resultsPerPlayer)
  }

  private def nextTwoRolls(player: Player, frame: Frame, allFrames: List[Frame]): List[RollResult] = {
    allFrames.filter(f => f.nr > frame.nr).flatMap(_.getPlayerRollResults(player)).take(2)
  }

}