package goose

import org.scalatest.{Matchers, Outcome, WordSpecLike}
import java.io.ByteArrayOutputStream

class UserInterfaceSpec extends WordSpecLike with Matchers with TestValues {

  import Game._
  import UserInterface._

  "add player" should {
    "add player correctly" in {
      addPlayer(testPlayer)
      players.size shouldBe 1
      players(testPlayer) shouldBe Player(testPlayer, 0, START_CELL)
    }
    "do not add two player with the same name" in {
      addPlayer(testPlayer)
      addPlayer(testPlayer)
      players.size shouldBe 1
    }
    "do not add two player with incorrect name" in {
      addPlayer(invalidPlayer1)
      addPlayer(invalidPlayer2)
      addPlayer(invalidPlayer3)
      addPlayer(invalidPlayer4)
      players shouldBe empty
    }
  }

  "clear players" should {
    "empty the players map" in {
      addPlayer(testPlayer)
      clearPlayers()
      players shouldBe empty
    }
  }

  "view players" should {
    "print the empty message if the players map is empty" in {
      val stream = new ByteArrayOutputStream()
      Console.withOut(stream)(viewPlayers())
      stream.toString shouldBe EMPTY_PLAYERS_MSG + "\n"
    }
    "print the players list if the players map is not empty" in {
      addPlayer(P1)
      addPlayer(P2)
      val stream = new ByteArrayOutputStream()
      Console.withOut(stream)(viewPlayers())
      stream.toString shouldBe s"$P1, $P2\n"
    }
  }

  override def withFixture(test: NoArgTest) : Outcome = {
    try super.withFixture(test)
    finally {
      clearPlayers()
    }
  }

}
