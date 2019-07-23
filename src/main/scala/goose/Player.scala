package goose

import scala.util.Random

case class Player(name: String, inGameOrder: Int, currentCell: Int, canMove: Boolean = true) {

  import Game._

  private val random = Random

  private def singleRoll : Int = random.nextInt(6) + 1

  def doubleRoll : Int = singleRoll + singleRoll

  def makePlayerRound(round: Round = StandardRound): Player = {

    print(s"${this.name} from ${this.currentCell} ")

    val playerAfterMove = round match {
      case StandardRound => moveForward
      case PenaltyRound => moveBackward
    }

    print(s"to ${playerAfterMove.currentCell} ")

    playerAfterMove.currentCell match {
      case SKULL_CELL =>
        println("of type SKULL")
        playerAfterMove.goToStart
      case JAIL_CELL =>
        print("of type JAIL ")
        playerAfterMove.goToJail
      case cell if GOOSE_CELLS.contains(cell) =>
        println("of type GOOSE")
        playerAfterMove.makePlayerRound()
      case cell if PIT_CELLS.contains(cell) =>
        println("of type PIT")
        playerAfterMove.makePlayerRound(PenaltyRound)
      case _ =>
        println()
        playerAfterMove
    }

  }

  def moveForward: Player = {

    val roll = doubleRoll

    print(s"roll $roll ")

    val potentialPosition = currentCell + roll

    val newPosition =
      if (potentialPosition > END_CELL)
        END_CELL - (potentialPosition - END_CELL)
      else
        potentialPosition

    Player(name, inGameOrder, newPosition)

  }

  def moveBackward: Player = {

    val roll = singleRoll

    print(s"roll -$roll ")

    Player(name, inGameOrder, currentCell - roll)

  }

  def goToStart: Player = Player(name, inGameOrder, START_CELL)

  def goToJail: Player = {
    Game.updateJail(this)
    Player(name, inGameOrder, currentCell, canMove = false)
  }

  def hasWin: Boolean = this.currentCell == END_CELL

  def isInJail: Boolean = !this.canMove

}