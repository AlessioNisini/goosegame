package goose

import org.scalatest.{Matchers, WordSpecLike}
class PlayerSpec extends WordSpecLike with Matchers {


  "move" should {
    "change the player position forward between 2 and 12" in {
      Game.addPlayers("Alex")
      Game.players("Alex").moveForward.currentCell should (be >= 2 and be <= 12)
    }
    "change the player position backward between -1 and -6" in {
      Game.addPlayers("Alex")
      Game.players("Alex").moveBackward.currentCell should (be >= -6 and be <= -1)
    }
    "change the player position backward if the final position is grater than End_Cell" in {
      Game.players += "Alex" -> Player("Alex", 0, 62)
      Game.players("Alex").moveForward.currentCell should (be >= 52 and be <= 62)
    }
  }

  "goToStart" should {
    "place a player to start again" in {
      Game.addPlayers("Alex")
      val movedPlayer = Game.players("Alex").moveForward
      movedPlayer.goToStart.currentCell shouldBe Game.START_CELL
    }
  }

  override def withFixture(test: NoArgTest) = {
    try super.withFixture(test)
    finally {
      Game.resetPlayers()
    }
  }

}
