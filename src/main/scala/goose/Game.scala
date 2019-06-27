package goose

object Game {

  trait Cell

  case object Plain extends Cell
  case object Goose extends Cell
  case object Pit extends Cell
  case object Jail extends Cell
  case object Skull extends Cell

  trait Round
  case object PenaltyRound extends Round
  case object StandardRound extends Round

  val START_CELL = 0
  val END_CELL = 63

  val board: Array[Cell] = Array.ofDim[Cell](END_CELL + 1)
  val players: scala.collection.mutable.Map[String, Player] = scala.collection.mutable.Map()

  def initializeBoard(): Unit = {
    for(i <- START_CELL to END_CELL) i match {
      case 58 => board(i) = Skull
      case 19 | 31 | 42 | 54 => board(i) = Pit
      case 5 | 18 | 27 | 36 | 45 => board(i) = Goose
      case _ => board(i) = Plain
    }
  }

  def addPlayers(names: String*): Unit = {
    for(name <- names)
      if(players.get(name).isEmpty)
        players += (name -> Player(name, players.size, START_CELL))
  }

  def startGame(): Player = loop(firstPlayer())

  def loop(currentPlayer: Player): Player = {
    val playerAfterHisRound = currentPlayer.makePlayerRound()
    println()
    updatePlayers(playerAfterHisRound)
    if(playerAfterHisRound.hasWin)
      playerAfterHisRound
    else
      loop(nextPlayer(playerAfterHisRound))
  }

  def cellType(player: Player): Cell = board(player.currentCell)

  def updatePlayers(player: Player): Unit = players += (player.name -> player)

  def firstPlayer(): Player = {
    players.collectFirst {
      case (_, firstPlayer@Player(_, 0, _)) => firstPlayer
    }.get
  }

  def nextPlayer(player: Player): Player = {
    val currentIndex = players(player.name).inGameOrder
    val nextIndex = (currentIndex + 1) % players.size
    players.collectFirst {
      case (_, nextPlayer@Player(_, index, _)) if index == nextIndex => nextPlayer
    }.get
  }

}