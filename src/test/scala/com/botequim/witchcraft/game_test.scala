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
    var nu = getAndTestGame(game.compose(Circle, 0, 0, true))
    assert(nu.availableTurnPoints === prev.availableTurnPoints - 1)
    assert(nu.spells(true).combination.length
           > prev.spells(true).combination.length)

    nu = getAndTestGame(game.compose(Concave, 7, 7, true))
    assert(nu.availableTurnPoints === prev.availableTurnPoints - 7)
  }

  test("""Compose returns a None when the number of points for each spell have
been exceeded.""") {
    assert(game.compose(Convex, 11, 0, true) === None)
  }

  test("""Compose returns a None when the number of points for the player have
been exceeded.""") {
    game = new WitchcraftGame(Map(true -> Spell(), false -> Spell()),
                              0.f,
                              Map(true -> false, false -> false),
                              Map(true -> 10.f, false -> 88.f))
    assert(game.compose(Circle, 0, 0, true) === None)
  }

  test("""Game points never reaches below zero.""") {
    game = new WitchcraftGame(Map(true -> ((Circle, 0, 0) :: Spell(1)),
                                  false -> ((Concave, 8, 8) :: Spell())),
                              10.f,
                              Map(true -> true, false -> true),
                              Map(true -> 4.f, false -> 88.f))
    game = game.getAftermath.get
    assert(game.availableGamePoints(true) === 0.f)
  }

  test("""Game returns none if one player don't commit.""") {
    val prms = (Map(true -> ((Circle, 0, 0) :: Spell(1)),
                                  false -> ((Concave, 8, 8) :: Spell())),
                              10.f,
                              Map(true -> 4.f, false -> 88.f))
    val g1 = new WitchcraftGame(prms._1, prms._2,
      Map(true -> false, false -> false), prms._3)
    val g2 = new WitchcraftGame(prms._1, prms._2,
      Map(true -> true, false -> false), prms._3)
    val g3 = new WitchcraftGame(prms._1, prms._2,
      Map(true -> false, false -> true), prms._3)
    val g4 = new WitchcraftGame(prms._1, prms._2,
      Map(true -> true, false -> true), prms._3)
    assert(g1.getAftermath === None)
    assert(g2.getAftermath === None)
    assert(g3.getAftermath === None)
    assert(g4.getAftermath.isInstanceOf[Some[_]])
  }

  test("""The getAftermath causes the correct damage taking into
account the reflection shield.""") {
    game = new WitchcraftGame(Map(true -> ((Circle, 0, 0) :: Spell(1)),
                                  false -> ((Concave, 8, 8) :: Spell())),
                              10.f,
                              Map(true -> true, false -> true),
                              Map(true -> 88.f, false -> 88.f))
    game = game.getAftermath.get
    assert(game.availableGamePoints(true) === 74.f)
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
