package goose

import scala.util.Random

case class Player(name: String, inGameOrder: Int, currentCell: Int) {

  import Game._

  private val random = Random
  private def singleRoll : Int = random.nextInt(6) + 1
  private def doubleRoll : Int = singleRoll + singleRoll

  def makePlayerRound(round: Round = StandardRound): Player = {

    print(s"${this.name} from ${this.currentCell} ")

    val playerAfterMove = round match {
      case StandardRound => moveForward
      case PenaltyRound => moveBackward
    }

    val cellTypeAfterMove = cellType(playerAfterMove)

    println(s"to ${playerAfterMove.currentCell} of type $cellTypeAfterMove")

    cellTypeAfterMove match {
      case Goose => playerAfterMove.makePlayerRound()
      case Pit => playerAfterMove.makePlayerRound(PenaltyRound)
      case Skull => playerAfterMove.goToStart
      case _ => playerAfterMove
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

  def hasWin: Boolean = this.currentCell == END_CELL

}