package goose

import goose.Game.PenaltyRound
import org.scalatest.{Matchers, WordSpecLike}
import org.mockito.Mockito._

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
    "change the player position backward if the final position is grater than END_CELL" in {
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

  "goToJail" should {
    "set canMove to false for a player" in {
      Game.addPlayers("Alex")
      val inJailPlayer = Game.players("Alex").goToJail
      inJailPlayer.canMove shouldBe false
    }
  }

  "makePlayerRound" should {
    "put a player on START_CELL if he goes to SKULL_CELL" in {
      Game.addPlayers("Alex")
      val alexPlayer = Game.players("Alex")
      val alexOnSkull = Player(alexPlayer.name, alexPlayer.inGameOrder, Game.SKULL_CELL)
      val spyAlex = spy(alexPlayer)
      doReturn(alexOnSkull, Nil: _*).when(spyAlex).moveForward
      spyAlex.makePlayerRound().currentCell shouldBe Game.START_CELL
    }
    "put a player in jail if he goes to JAIL_CELL" in {
      Game.addPlayers("Alex")
      val alexPlayer = Game.players("Alex")
      val alexOnJail = Player(alexPlayer.name, alexPlayer.inGameOrder, Game.JAIL_CELL)
      val spyAlex = spy(alexPlayer)
      doReturn(alexOnJail, Nil: _*).when(spyAlex).moveForward
      spyAlex.makePlayerRound() shouldBe Player(spyAlex.name, spyAlex.inGameOrder, Game.JAIL_CELL, canMove = false)
    }
    "move a player forward again if he goes to a GOOSE_CELLS" in {
      Game.addPlayers("Alex")
      val alexPlayer = Game.players("Alex")
      val alexOnGoose = Player(alexPlayer.name, alexPlayer.inGameOrder, Game.GOOSE_CELLS(0))
      val spyAlex = spy(alexPlayer)
      doReturn(alexOnGoose, Nil: _*).when(spyAlex).moveForward
      spyAlex.makePlayerRound().currentCell should (be >= 7 and be <= 17)
    }
    "move a player backward again if he goes to a PIT_CELLS" in {
      Game.addPlayers("Alex")
      val alexPlayer = Game.players("Alex")
      val alexOnPit = Player(alexPlayer.name, alexPlayer.inGameOrder, Game.PIT_CELLS(3))
      val spyAlex = spy(alexPlayer)
      doReturn(alexOnPit, Nil: _*).when(spyAlex).moveForward
      spyAlex.makePlayerRound().currentCell should (be >= 48 and be <= 53)
    }
    "return a player to X position if he goes to a plain cell X" in {
      Game.addPlayers("Alex")
      val alexPlayer = Game.players("Alex")
      val alexOnPit = Player(alexPlayer.name, alexPlayer.inGameOrder, 4)
      val spyAlex = spy(alexPlayer)
      doReturn(alexOnPit, Nil: _*).when(spyAlex).moveForward
      spyAlex.makePlayerRound().currentCell shouldBe 4
    }
    "call moveBackward method if a player play a penalty round" in {
      Game.addPlayers("Alex")
      val alexPlayer = Game.players("Alex")
      val spyAlex = spy(alexPlayer)
      spyAlex.makePlayerRound(PenaltyRound)
      verify(spyAlex, times(1)).moveBackward
    }
  }

  override def withFixture(test: NoArgTest) = {
    try super.withFixture(test)
    finally {
      Game.resetPlayers()
    }
  }

}
