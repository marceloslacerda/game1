/*
 * Copyright 2013 Marcelo de Sena Lacerda
 *
 * This file is part of Witchcraft.
 * 
 * Witchcraft is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 * 
 * Witchcraft is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 * 
 * You should have received a copy of the GNU General Public License
 * along with Witchcraft.  If not, see <http://www.gnu.org/licenses/>.
 */

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
    assert(nu.spells(nu.player).combination.length
           > prev.spells(prev.player).combination.length)

    nu = getAndTestGame(game.compose(Concave, 7, 7))
    assert(nu.availableTurnPoints === prev.availableTurnPoints - 7)
  }

  test("""Compose returns a None when the number of points for each spell have
been exceeded.""") {
    assert(game.compose(Convex, 11, 0) === None)
  }

  test("""Compose returns a None when the number of points for the player have
been exceeded.""") {
    game = new WitchcraftGame(Map(true -> Spell(), false -> Spell()),
                              true,
                              0.f,
                              Map(true -> 10.f, false -> 88.f))
    assert(game.compose(Circle, 0, 0) === None)
  }

  test("""Game points never reaches below zero.""") {
    game = new WitchcraftGame(Map(true -> ((Circle, 0, 0) :: Spell(1)),
                                  false -> ((Concave, 8, 8) :: Spell())),
                              false,
                              10.f,
                              Map(true -> 4.f, false -> 88.f))
    game = game.getAftermath
    assert(game.availableGamePoints(!game.player) === 0.f)
  }
  test("""The getAftermath causes the correct damage taking into
account the reflection shield.""") {
    game = new WitchcraftGame(Map(true -> ((Circle, 0, 0) :: Spell(1)),
                                  false -> ((Concave, 8, 8) :: Spell())),
                              false,
                              10.f,
                              Map(true -> 88.f, false -> 88.f))
    game = game.getAftermath
    assert(game.availableGamePoints(!game.player) === 74.f)
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
