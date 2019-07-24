package goose

import scala.io.StdIn.readLine

object Main extends App {

  import Game._

  println("THE GOOSE GAME!!!\n")

  userInterface

  def userInterface: Unit = {

    println("a: Add a player")
    println("v: View the players")
    println("s: Start the game")
    print("=> ")

    readLine() match {
      case "a" =>
        println("Enter the player's name")
        print("=> ")
        println(addPlayer(readLine()))
        userInterface
      case "v" =>
        println(s"${viewPlayers()}")
        userInterface
      case "s" if players.size < 2 =>
        println("Error: Add at least two player")
        userInterface
      case "s" =>
        println("\nThe winner is " + startGame().name)
      case _ =>
        println("Error: Invalid command")
        userInterface
    }

  }

}




