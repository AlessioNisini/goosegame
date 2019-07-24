package goose

import org.scalatest.{Matchers, Outcome, WordSpecLike}
import org.mockito.Mockito._

class PlayerSpec extends WordSpecLike with Matchers with TestValues {

  import Game._

  "move" should {
    "change the player position forward between 2 and 12" in {
      players(P1).moveForward.currentCell should (be >= 2 and be <= 12)
    }
    "change the player position backward between -1 and -6" in {
      players(P1).moveBackward.currentCell should (be >= -6 and be <= -1)
    }
    "change the player position backward if the final position is grater than END_CELL" in {
      players += P1 -> Player(P1, P1order, 62)
      players(P1).moveForward.currentCell should (be >= 52 and be <= 62)
    }
  }

  "goToStart" should {
    "place a player to start again" in {
      val movedPlayer = players(P1).moveForward
      movedPlayer.goToStart.currentCell shouldBe START_CELL
    }
  }

  "goToJail" should {
    "set canMove to false for a player" in {
      val inJailPlayer = players(P1).goToJail
      inJailPlayer.canMove shouldBe false
    }
  }

  "makePlayerRound" should {
    "put a player on START_CELL if he goes to SKULL_CELL" in {
      val player = forceCellAfterMove(SKULL_CELL)
      player.makePlayerRound().currentCell shouldBe START_CELL
    }
    "put a player in jail if he goes to JAIL_CELL" in {
      val player = forceCellAfterMove(JAIL_CELL)
      player.makePlayerRound() shouldBe Player(player.name, player.inGameOrder, JAIL_CELL, canMove = false)
    }
    "move a player forward again if he goes to a GOOSE_CELLS" in {
      val player = forceCellAfterMove(GOOSE_CELLS(firstGooseCell))
      player.makePlayerRound().currentCell should (be >= 7 and be <= 17)
    }
    "move a player backward again if he goes to a PIT_CELLS" in {
      val player = forceCellAfterMove(PIT_CELLS(lastPitCell))
      player.makePlayerRound().currentCell should (be >= 48 and be <= 53)
    }
    "return a player to X position if he goes to a plain cell X" in {
      val player = forceCellAfterMove(aPlainCell)
      player.makePlayerRound().currentCell shouldBe aPlainCell
    }
    "call moveBackward method if a player play a penalty round" in {
      val player = players(P1)
      val spyPlayer = spy(player)
      spyPlayer.makePlayerRound(PenaltyRound)
      verify(spyPlayer, times(1)).moveBackward
    }
  }

  def forceCellAfterMove(cell: Int): Player = {
    val player = players(P1)
    val spyPlayer = spy(player)
    doReturn(Player(spyPlayer.name, spyPlayer.inGameOrder, cell), Nil: _*).when(spyPlayer).moveForward
    spyPlayer
  }

  override def withFixture(test: NoArgTest) : Outcome = {
    try {
      addPlayer(P1)
      super.withFixture(test)
    }
    finally {
      resetPlayers()
    }
  }

}
