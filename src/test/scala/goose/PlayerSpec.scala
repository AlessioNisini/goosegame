package goose

import goose.Game.PenaltyRound
import org.scalatest.{Matchers, WordSpecLike}
import org.mockito.Mockito._

class PlayerSpec extends WordSpecLike with Matchers {

  "move" should {
    "change the player position forward between 2 and 12" in {
      Game.players("Alex").moveForward.currentCell should (be >= 2 and be <= 12)
    }
    "change the player position backward between -1 and -6" in {
      Game.players("Alex").moveBackward.currentCell should (be >= -6 and be <= -1)
    }
    "change the player position backward if the final position is grater than END_CELL" in {
      Game.players += "Alex" -> Player("Alex", 0, 62)
      Game.players("Alex").moveForward.currentCell should (be >= 52 and be <= 62)
    }
  }

  "goToStart" should {
    "place a player to start again" in {
      val movedPlayer = Game.players("Alex").moveForward
      movedPlayer.goToStart.currentCell shouldBe Game.START_CELL
    }
  }

  "goToJail" should {
    "set canMove to false for a player" in {
      val inJailPlayer = Game.players("Alex").goToJail
      inJailPlayer.canMove shouldBe false
    }
  }

  "makePlayerRound" should {
    "put a player on START_CELL if he goes to SKULL_CELL" in {
      val player = forceCellAfterMove(Game.SKULL_CELL)
      player.makePlayerRound().currentCell shouldBe Game.START_CELL
    }
    "put a player in jail if he goes to JAIL_CELL" in {
      val player = forceCellAfterMove(Game.JAIL_CELL)
      player.makePlayerRound() shouldBe Player(player.name, player.inGameOrder, Game.JAIL_CELL, canMove = false)
    }
    "move a player forward again if he goes to a GOOSE_CELLS" in {
      val player = forceCellAfterMove(Game.GOOSE_CELLS(0))
      player.makePlayerRound().currentCell should (be >= 7 and be <= 17)
    }
    "move a player backward again if he goes to a PIT_CELLS" in {
      val player = forceCellAfterMove(Game.PIT_CELLS(3))
      player.makePlayerRound().currentCell should (be >= 48 and be <= 53)
    }
    "return a player to X position if he goes to a plain cell X" in {
      val player = forceCellAfterMove(4)
      player.makePlayerRound().currentCell shouldBe 4
    }
    "call moveBackward method if a player play a penalty round" in {
      val alexPlayer = Game.players("Alex")
      val spyAlex = spy(alexPlayer)
      spyAlex.makePlayerRound(PenaltyRound)
      verify(spyAlex, times(1)).moveBackward
    }
  }

  def forceCellAfterMove(cell: Int): Player = {
    val alexPlayer = Game.players("Alex")
    val spyAlex = spy(alexPlayer)
    doReturn(Player(alexPlayer.name, alexPlayer.inGameOrder, cell), Nil: _*).when(spyAlex).moveForward
    spyAlex
  }

  override def withFixture(test: NoArgTest) = {
    try {
      Game.addPlayers("Alex")
      super.withFixture(test)
    }
    finally {
      Game.resetPlayers()
    }
  }

}
