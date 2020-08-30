import Bowling.Player
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class BowlingTest extends AnyFlatSpec with Matchers {

  val nrOfFrames = 2
  val players = List(new Player("Test"))

  "A Bowling" should "summarize points for normal rolls" in {
    val rollFunction: (Int, Int) => Int = (frameNr, previousValue) => (frameNr, previousValue) match {
      case (1,0) => 5
      case (1,_) => 3
      case (2,0) => 1
      case (2,_) => 4
    }

    val result = Bowling.play(nrOfFrames, players, rollFunction)

    result.display() shouldEqual  "Test      : 5|3 (8) || 1|4 (5) = 13"
  }

  it should "summarize points for spare" in {
    val rollFunction: (Int, Int) => Int = (frameNr, previousValue) => (frameNr, previousValue) match {
      case (1,0) => 5
      case (1,_) => 5
      case (2,0) => 1
      case (2,_) => 4
    }

    val result = Bowling.play(nrOfFrames, players, rollFunction)

    result.display() shouldEqual "Test      : 5|/ (11) || 1|4 (5) = 16"
  }

  it should "summarize points for strike" in {
    val rollFunction: (Int, Int) => Int = (frameNr, previousValue) => (frameNr, previousValue) match {
      case (1,0) => 10
      case (2,0) => 1
      case (2,_) => 4
    }

    val result = Bowling.play(nrOfFrames, players, rollFunction)

    result.display() shouldEqual "Test      : X (15) || 1|4 (5) = 20"
  }

  it should "summarize points for three strikes" in {
    val nrOfFrames = 4
    val rollFunction: (Int, Int) => Int = (frameNr, previousValue) => (frameNr, previousValue) match {
      case (1,0) => 10
      case (2,0) => 10
      case (3,0) => 10
      case (4,0) => 1
      case (4,_) => 3
    }

    val result = Bowling.play(nrOfFrames, players, rollFunction)

    result.display() shouldEqual "Test      : X (30) || X (21) || X (14) || 1|3 (4) = 69"
  }

  it should "summarize points for three spares" in {
    val nrOfFrames = 4
    val rollFunction: (Int, Int) => Int = (frameNr, previousValue) => (frameNr, previousValue) match {
      case (1,0) => 5
      case (1,_) => 5
      case (2,0) => 4
      case (2,_) => 6
      case (3,0) => 1
      case (3,_) => 9
      case (4,0) => 1
      case (4,_) => 3
    }

    val result = Bowling.play(nrOfFrames, players, rollFunction)

    result.display() shouldEqual "Test      : 5|/ (14) || 4|/ (11) || 1|/ (11) || 1|3 (4) = 40"
  }

  it should "summarize points for spare in the last frame" in {
    val rollFunction: (Int, Int) => Int = (frameNr, previousValue) => (frameNr, previousValue) match {
      case (1,0) => 5
      case (1,_) => 5
      case (2,0) => 4
      case (2,_) => 6
    }

    val result = Bowling.play(nrOfFrames, players, rollFunction)

    result.display() shouldEqual "Test      : 5|/ (14) || 4|/|4 (14) = 28"
  }

  it should "summarize points for strike in the last frame" in {
    val rollFunction: (Int, Int) => Int = (frameNr, previousValue) => (frameNr, previousValue) match {
      case (1,0) => 1
      case (1,_) => 4
      case (2,0) => 10
    }

    val result = Bowling.play(nrOfFrames, players, rollFunction)

    result.display() shouldEqual "Test      : 1|4 (5) || X|X|X (30) = 35"
  }

  it should "summarize points for strikes only" in {
    val nrOfFrames = 10
    val rollFunction: (Int, Int) => Int = (frameNr, previousValue) => (frameNr, previousValue) match {
      case (1,0) => 10
      case (2,0) => 10
      case (3,0) => 10
      case (4,0) => 10
      case (5,0) => 10
      case (6,0) => 10
      case (7,0) => 10
      case (8,0) => 10
      case (9,0) => 10
      case (10,0) => 10
    }

    val result = Bowling.play(nrOfFrames, players, rollFunction)

    result.display() shouldEqual "Test      : X (30) || X (30) || X (30) || X (30)" +
      " || X (30) || X (30) || X (30) || X (30) || X (30) || X|X|X (30) = 300"
  }

  it should "summarize points for two players" in {
    val players = List(new Player("Test1"), new Player("Test2"))
    val rollFunction: (Int, Int) => Int = (frameNr, previousValue) => (frameNr, previousValue) match {
      case (1,0) => 5
      case (1,_) => 3
      case (2,0) => 1
      case (2,_) => 4
    }

    val result = Bowling.play(nrOfFrames, players, rollFunction)

    result.display() shouldEqual  "Test1     : 5|3 (8) || 1|4 (5) = 13\nTest2     : 5|3 (8) || 1|4 (5) = 13"
  }

}
