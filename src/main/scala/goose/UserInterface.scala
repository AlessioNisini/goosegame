package goose

import scala.annotation.tailrec
import scala.io.StdIn.readLine

object UserInterface {

  import Game._

  case class Command (key: String, msg: String)

  val ADD_PLAYER = Command("a", "Add player")
  val VIEW_PLAYERS = Command("v", "View players")
  val CLEAR_PLAYERS = Command("c", "Clear players")
  val START_GAME = Command("s", "Start game")
  val EXIT_GAME = Command("e", "Exit game")

  val AT_LEAST_TWO_ERROR = "Error: Add at least two player"
  val INVALID_NAME_ERROR = "Error: Invalid name"
  val DUPLICATE_NAME_ERROR = "Error: Player already exist"
  val INVALID_COMMAND_ERROR = "Error: Invalid command"

  val TITLE_MSG = "\nTHE GOOSE GAME!!!"
  val WAITING_INPUT_MSG = "=> "
  val INSERT_PLAYER_MSG = s"Enter the player's name\n$WAITING_INPUT_MSG"
  val OK_ADDED_MSG = "OK player added"
  val EMPTY_PLAYERS_MSG = "No player added yet"
  val CLEAR_PLAYERS_MSG = "OK players cleared"
  val WINNER_MSG = "\nThe winner is ... "
  val EXIT_MSG = "BYE BYE !!!"

  @tailrec
  def run(): Unit = {

    printWelcomeScreen()

    readLine() match {
      case ADD_PLAYER.key =>
        print(INSERT_PLAYER_MSG)
        addPlayer(readLine())
        run()
      case VIEW_PLAYERS.key =>
        viewPlayers()
        run()
      case CLEAR_PLAYERS.key =>
        clearPlayers()
        println(CLEAR_PLAYERS_MSG)
        run()
      case START_GAME.key if players.size < 2 =>
        println(AT_LEAST_TWO_ERROR)
        run()
      case START_GAME.key =>
        println(WINNER_MSG + startGame().name)
      case EXIT_GAME.key =>
        println(EXIT_MSG)
      case _ =>
        println(INVALID_COMMAND_ERROR)
        run()
    }

  }

  def addPlayer(name: String): Unit = name match {
    case _ if !name.matches(REGEX) => println(INVALID_NAME_ERROR)
    case _ if players.get(name).isDefined => println(DUPLICATE_NAME_ERROR)
    case _ =>
      updatePlayers(Player(name, players.size, START_CELL))
      println(OK_ADDED_MSG)
  }

  def viewPlayers(): Unit = {
    if (players.isEmpty) println(EMPTY_PLAYERS_MSG)
    else println(players.values.map(_.name).mkString(", "))
  }

  def clearPlayers(): Unit = players.clear()

  private def printWelcomeScreen() : Unit = {
    println(TITLE_MSG)
    printCommand(ADD_PLAYER)
    printCommand(VIEW_PLAYERS)
    printCommand(CLEAR_PLAYERS)
    printCommand(START_GAME)
    printCommand(EXIT_GAME)
    print(WAITING_INPUT_MSG)
  }

  private def printCommand(cmd: Command): Unit = {
    println(s"${cmd.key}: ${cmd.msg}")
  }

}
