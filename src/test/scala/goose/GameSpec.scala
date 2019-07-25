package goose

import org.scalatest.{Matchers, Outcome, WordSpecLike}

class GameSpec extends WordSpecLike with Matchers with TestValues {

  import Game._
  import UserInterface._

  "first player" should {
    "return the first player of the game" in {
      firstPlayer().name shouldBe P1
    }
  }

  "next player" should {
    "return the second player for the first player" in {
      val firstPlayer = players(P1)
      nextPlayer(firstPlayer).name shouldBe P2
    }
    "return the first player for the last player" in {
      val lastPlayer = players(P3)
      nextPlayer(lastPlayer).name shouldBe P1
    }
  }

  "anyone in jail" should {
    "return None if there is no one in jail" in {
      anyoneInJail() shouldBe None
    }
    "return Some(Player) if the jail is not empty" in {
      updateJail(players(P1))
      anyoneInJail() shouldBe Some(players(P1))
    }
  }

  "update jail" should {
    "if the jail is empty, put a player in jail" in {
      updateJail(players(P1))
      players(P1) shouldBe Player(P1, P1order, START_CELL, canMove = false)
    }
    "if there is someone in jail, put a player in jail and release the other" in {
      updateJail(players(P2))
      updateJail(players(P3))
      players(P2) shouldBe Player(P2, P2order, START_CELL)
      players(P3) shouldBe Player(P3, P3order, START_CELL, canMove = false)
    }
  }

  "play one round" should {
    "change the player position and update the players" in {
      playOneRound(players(P1))
      players(P1).currentCell shouldBe > (START_CELL)
    }
  }

  "start game" should {
    "return a winner" in {
      startGame shouldBe a [Player]
    }
  }

  override def withFixture(test: NoArgTest) : Outcome = {
    try {
      addPlayer(P1)
      addPlayer(P2)
      addPlayer(P3)
      super.withFixture(test)
    }
    finally {
      clearPlayers()
    }
  }

}
