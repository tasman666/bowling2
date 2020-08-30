import Bowling.Player

object BowlingApp {

  val nrOfFrames = 10

  def main(args: Array[String]): Unit = {

    val players = List(new Player("Sylwia"), new Player("Arek"))

    println("Bowling game is starting")
    val result = Bowling.play(nrOfFrames, players)

    println("-----RESULTS------")
    println(result.display())

  }

}
