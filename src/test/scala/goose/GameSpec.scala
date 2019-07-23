package goose

import org.scalatest.{Matchers, WordSpecLike}

class GameSpec extends WordSpecLike with Matchers {

  "add player" should {
    "add player correctly" in {
      Game.addPlayers("Alex", "Elena", "Pietro")
      Game.players.size shouldBe 3
      Game.players("Alex") shouldBe Player("Alex", 0, Game.START_CELL)
      Game.players("Elena") shouldBe Player("Elena", 1, Game.START_CELL)
      Game.players("Pietro") shouldBe Player("Pietro", 2, Game.START_CELL)
    }
    "do not add two player with the same name" in {
      Game.addPlayers("Alex", "Alex", "Pietro")
      Game.players.size shouldBe 2
      Game.players("Alex") shouldBe Player("Alex", 0, Game.START_CELL)
      Game.players("Pietro") shouldBe Player("Pietro", 1, Game.START_CELL)
    }
  }

  "first player" should {
    "return the first player of the game" in {
      Game.addPlayers("Alex", "Elena", "Pietro")
      Game.firstPlayer().name shouldBe "Alex"
    }
  }

  "next player" should {
    "return the second player for the first player" in {
      Game.addPlayers("Alex", "Elena", "Pietro")
      val firstPlayer = Game.players("Alex")
      Game.nextPlayer(firstPlayer).name shouldBe "Elena"
    }
    "return the first player for the last player" in {
      Game.addPlayers("Alex", "Elena", "Pietro")
      val lastPlayer = Game.players("Pietro")
      Game.nextPlayer(lastPlayer).name shouldBe "Alex"
    }
  }

  "anyone in jail" should {
    "return None if there is no one in jail" in {
      Game.addPlayers("Alex", "Elena", "Pietro")
      Game.anyoneInJail() shouldBe None
    }
    "return Some(Player) if the jail is not empty" in {
      Game.addPlayers("Alex", "Elena", "Pietro")
      Game.updateJail(Game.players("Elena"))
      Game.anyoneInJail() shouldBe Some(Game.players("Elena"))
    }
  }

  "update jail" should {
    "if the jail is empty, put a player in jail" in {
      Game.addPlayers("Alex", "Elena", "Pietro")
      Game.updateJail(Game.players("Elena"))
      Game.players("Elena") shouldBe Player("Elena", 1, Game.START_CELL, canMove = false)
    }
    "if there is someone in jail, put a player in jail and release the other" in {
      Game.addPlayers("Alex", "Elena", "Pietro")
      Game.updateJail(Game.players("Elena"))
      Game.updateJail(Game.players("Pietro"))
      Game.players("Elena") shouldBe Player("Elena", 1, Game.START_CELL)
      Game.players("Pietro") shouldBe Player("Pietro", 2, Game.START_CELL, canMove = false)
    }
  }

  "play one round" should {
    "change the player position and update the players" in {
      Game.addPlayers("Alex", "Elena", "Pietro")
      Game.playOneRound(Game.players("Alex"))
      Game.players("Alex").currentCell shouldBe > (Game.START_CELL)
    }
  }

  "start game" should {
    "return a winner" in {
      Game.addPlayers("Alex", "Elena", "Pietro")
      Game.startGame() shouldBe a [Player]
    }
  }

  override def withFixture(test: NoArgTest) = {
    try super.withFixture(test)
    finally {
      Game.resetPlayers()
    }
  }

}
