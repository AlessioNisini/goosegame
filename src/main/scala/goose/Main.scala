package goose

object Main extends App {

  Game.addPlayers("Alex", "Elena", "Luca", "Pietro")
  val winner = Game.startGame()
  println("The winner is " + winner.name)

}




