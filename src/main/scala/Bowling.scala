import com.sun.tools.corba.se.idl.StringGen

import scala.util.Random

object Bowling {

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
    override def value(): Int = 10
    override def desc(): String = "X"
  }

  class Player(val name: String) {

    val maxNameSize = 20

    def roll(previousRolls: List[RollResult]): RollResult = {
      println(s"   Player $name rolling...")
      Thread.sleep(2000)

      val previousValues = previousRolls.map(_.value()).sum
      val rollResult = Random.between(0, 11  - previousValues)

      val result = if (rollResult == 10 && previousRolls.isEmpty) {
        Strike
      } else {
        val values = previousValues + rollResult
        if (values == 10) {
          Spare(rollResult)
        } else {
          Normal(rollResult)
        }
      }
      println(s"   Player $name throws $result")
      result
    }

    def getDisplayName() = {
      name match {
        case n if n.length < maxNameSize =>
          val emptySpaces = " " * (maxNameSize - n.length)
          name + emptySpaces
        case _ => name.substring(0, maxNameSize + 1)
      }
    }
  }

  class Frame(val nr: Int, rolls: Map[Player, List[RollResult]]) {

    def getPoints(player: Player, nextFrame: Option[Frame]): Points =  {
      val playerRolls = getPlayerRollResults(player)
      val pointsDesc = playerRolls.map(_.desc()).mkString("|")

      val allPointsFromFrame = playerRolls.map(_.value()).sum

      val allPoints: Int = playerRolls match {
        case rolls if rolls.contains(Strike) => nextFrame.map(_.getPoints(player, None).value)
            .getOrElse(0) + allPointsFromFrame
        case rolls if rolls.contains(Spare) => nextFrame.map(_.getFirstRollResultValue(player))
            .getOrElse(0) + allPointsFromFrame
        case _ => allPointsFromFrame
      }

      new Points(nr, allPoints, pointsDesc)
    }

    private def getFirstRollResultValue(player: Player): Int = {
      val playerRolls = getPlayerRollResults(player)
      playerRolls.head.value()
    }

    private def getPlayerRollResults(player: Player): List[RollResult] = {
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

  def play(nrOfFrames: Int, players: List[Player]): BowlingResult = {
    println("Bowling game is starting")
    val frameNrs = List.range(1, nrOfFrames + 1)

    val frames = frameNrs.map(nrOfFrame => {
      println(s"Lets start frame $nrOfFrame")
      playFrame(nrOfFrame, players)
    })

    getResult(players, frames)

  }

  private def playFrame(nrOfFrame: Int, players: List[Player]): Frame = {
    val playerResults = players.map( player => {
      val resultFirstRoll = player.roll(List())
      resultFirstRoll match {
        case Strike =>  (player, List(resultFirstRoll))
        case _ =>
          val resultSecondRoll = player.roll(List(resultFirstRoll))
          (player, List(resultFirstRoll, resultSecondRoll))
      }
    }).toMap
    new Frame(nrOfFrame, playerResults)
  }

  private def getResult(players: List[Player], frames: List[Frame]): BowlingResult = {
    val resultsPerPlayer = players.map(player => {
          val points = frames.map(frame => {
            frame.getPoints(player, nextFrame(frame, frames))
          })
          new BowlingResultPerPlayer(player, points)
    })

    new BowlingResult(resultsPerPlayer)
  }

  private def nextFrame(frame: Frame, allFrames: List[Frame]): Option[Frame] = {
    allFrames.find(f => f.nr == frame.nr + 1)
  }

}
