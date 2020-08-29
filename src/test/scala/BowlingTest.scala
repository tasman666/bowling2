import Bowling.Player
import org.scalatest.flatspec.AnyFlatSpec

class BowlingTest extends AnyFlatSpec {

  "A Bowling" should "play for one player" in {
    val nrOfFrames = 2
    val players = List(new Player("Test"))

    val result = Bowling.play(nrOfFrames, players)
    result

  }

}
