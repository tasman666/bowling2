import Bowling.Player

object BowlingApp {

  val nrOfFrames = 2

  def main(args: Array[String]): Unit = {

    val players = List(new Player("Sylwia"), new Player("Arek"))

    val result = Bowling.play(nrOfFrames, players)

    println("-----RESULTS------")
    println(result.display())

  }

}
