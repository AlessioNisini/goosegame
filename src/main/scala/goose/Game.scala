package goose

import scala.annotation.tailrec

object Game {

  trait Round
  case object PenaltyRound extends Round
  case object StandardRound extends Round

  val REGEX = "^[a-zA-Z0-9]+$"

  val START_CELL = 0
  val END_CELL = 63
  val SKULL_CELL = 58
  val JAIL_CELL = 37
  val GOOSE_CELLS : Vector[Int] = Vector(5, 18, 27, 36, 45)
  val PIT_CELLS : Vector[Int] = Vector(19, 31, 42, 54)

  val players: scala.collection.mutable.Map[String, Player] = scala.collection.mutable.Map()

  def startGame(): Player = loop(firstPlayer())

  @tailrec
  def loop(currentPlayer: Player): Player = {
    println()
    if(currentPlayer.isInJail) {
      println(s"${currentPlayer.name} is in jail")
      loop(nextPlayer(currentPlayer))
    } else {
      val playerAfterHisRound = playOneRound(currentPlayer)
      if(playerAfterHisRound.hasWin) playerAfterHisRound
      else loop(nextPlayer(playerAfterHisRound))
    }
  }

  def playOneRound(player: Player) : Player = {
    val playerAfterHisRound = player.makePlayerRound()
    updatePlayers(playerAfterHisRound)
    playerAfterHisRound
  }

  def firstPlayer(): Player = {
    players.collectFirst {
      case (_, firstPlayer@Player(_, 0, _, _)) => firstPlayer
    }.get
  }

  def nextPlayer(player: Player): Player = {
    val currentIndex = players(player.name).inGameOrder
    val nextIndex = (currentIndex + 1) % players.size
    players.collectFirst {
      case (_, nextPlayer@Player(_, index, _, _)) if index == nextIndex => nextPlayer
    }.get
  }

  def updatePlayers(player: Player): Unit = {
    players += (player.name -> player)
  }

  def updateJail(player: Player): Unit = {
    anyoneInJail().foreach(freeThePlayer)
    putInJail(player)
    println()
  }

  def anyoneInJail(): Option[Player] = players.find(!_._2.canMove).map(_._2)

  private def freeThePlayer(player: Player): Unit = {
    print(s"now ${player.name} is free")
    updatePlayers(Player(player.name, player.inGameOrder, player.currentCell))
  }

  private def putInJail(player: Player): Unit = {
    updatePlayers(Player(player.name, player.inGameOrder, player.currentCell, canMove = false))
  }

}