package com.botequim.witchcraft.rules

import org.scalatest.{FunSuite, BeforeAndAfter}
import Form._


class WitchcraftGameSuite extends FunSuite with BeforeAndAfter {
  var game: WitchcraftGame = _

  before {
    game = WitchcraftGame.apply
  }

  test("Compose works nicelly with many points available.") {
    val prev = game
    var nu = getAndTestGame(game.compose(Circle, 0, 0))
    assert(nu.availableTurnPoints === prev.availableTurnPoints - 1)
    assert(nu.availableGamePoints(nu.player)
           === prev.availableGamePoints(prev.player) - 1)
    assert(nu.spells(nu.player).combination.length
           > prev.spells(prev.player).combination.length)

    nu = getAndTestGame(game.compose(Concave, 7, 7))
    assert(nu.availableTurnPoints === prev.availableTurnPoints - 7)
    assert(nu.availableGamePoints(nu.player)
           === prev.availableGamePoints(prev.player) - 7)
  }

  test("""Compose returns a None when the number of points for each spell have
been exceeded.""") {
    assert(game.compose(Convex, 11, 0) === None)
  }

  test("""Compose returns a None when the number of points for the player have
been exceeded.""") {
    game = new WitchcraftGame(Map(true -> 0, false -> 0),
                              Map(true -> Spell(), false -> Spell()),
                              true,
                              10.f,
                              Map(true -> 0.f, false -> 88.f))
    assert(game.compose(Circle, 0, 0) === None)
  }

  test("""The getAftermath causes the correct damage taking into
account the reflection shield.""") {
    game = new WitchcraftGame(Map(true -> 0, false -> 0),
                              Map(true -> ((Circle, 0, 0) :: Spell(1)),
                                  false -> ((Concave, 8, 8) :: Spell())),
                              false,
                              10.f,
                              Map(true -> 88.f, false -> 88.f))
    game = game.getAftermath
    assert(game.availableGamePoints(game.player) === 84.f)
  }

  test("""The getAftermath resets the initial points automatically.""") {
    game = new WitchcraftGame(Map(true -> 0, false -> 0),
                              Map(true ->  Spell(),
                                  false ->  Spell()),
                              false,
                              10.f,
                              Map(true -> 0.f, false -> 100.f))
    game = game.getAftermath
    assert(game.availableGamePoints(game.player) === 100.f)
  }
  def getAndTestGame(o: Option[WitchcraftGame]) =
    o match {
      case Some(g) => g
      case None => {
        assert(false, "WitchcraftGame produced a None on compose.")
        null
      }
    }
}
