/*
 * Copyright 2013 Marcelo de Sena Lacerda
 *
 * This file is part of Botequim's Game 1.
 *
 * Game 1 is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * Game 1 is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with Game 1.  If not, see <http://www.gnu.org/licenses/>.
 */

package org.botequim.game1.rules

import org.scalatest.{FunSuite, BeforeAndAfter}
import Form._


class GameSuite extends FunSuite with BeforeAndAfter {
  var game: Game = _

  before {
    game = Game.apply
  }

  test("Compose works nicelly with many points available.") {
    val prev = game
    var nu = getAndTestCompose(game.compose(Circle, 0, 0, true))
    assert(nu.turnPoints(true) === prev.turnPoints(true) - 1)
    assert(nu.spells(true).combination.length
           > prev.spells(true).combination.length)

    nu = getAndTestCompose(game.compose(Concave, 7, 7, true))
    assert(nu.turnPoints(true) === prev.turnPoints(true) - 7)
  }

  test("""Compose returns a None when the number of points for each spell have
been exceeded.""") {
    assert(game.compose(Convex, 11, 0, true) === None)
  }

  test("""Compose returns a None when the number of points for the player have
been exceeded.""") {
    game = new Game(Map(true -> Spell(), false -> Spell()),
                              Map(true -> 0d, false -> 10d),
                              Map(true -> false, false -> false),
                              Map(true -> 10d, false -> 88d))
    assert(game.compose(Circle, 0, 0, true) === None)
  }

  test("""Game points never reaches below zero.""") {
    game = new Game(Map(true -> ((Circle, 0, 0) :: Spell(1)),
                                  false -> ((Concave, 8, 8) :: Spell())),
                              Map(true -> 10d, false -> 10d),
                              Map(true -> true, false -> true),
                              Map(true -> 4d, false -> 88d))
    game = getAndTestAftermath(game.getAftermath)
    assert(game.gamePoints(true) === 0d)
  }

  test("""Game points is decreased by turn points.""") {
    game = new Game(Map(true -> ((Circle, 0, 0) :: Spell(1)),
                                  false -> ((Concave, 8, 8) :: Spell())),
                              Map(true -> 10d, false -> 10d),
                              Map(true -> true, false -> true),
                              Map(true -> 4d, false -> 88d))
    game = getAndTestAftermath(game.getAftermath)
    assert(game.gamePoints(false) === 78d)
  }

  test("""Game returns none if one player don't commit.""") {
    val prms = (Map(true -> ((Circle, 0, 0) :: Spell(1)),
                                  false -> ((Concave, 8, 8) :: Spell())),
                              Map(true -> 10d, false -> 10d),
                              Map(true -> 4d, false -> 88d))
    val g1 = new Game(prms._1, prms._2,
      Map(true -> false, false -> false), prms._3)
    val g2 = new Game(prms._1, prms._2,
      Map(true -> true, false -> false), prms._3)
    val g3 = new Game(prms._1, prms._2,
      Map(true -> false, false -> true), prms._3)
    val g4 = new Game(prms._1, prms._2,
      Map(true -> true, false -> true), prms._3)
    assert(g1.getAftermath === None)
    assert(g2.getAftermath === None)
    assert(g3.getAftermath === None)
    assert(g4.getAftermath.isInstanceOf[Some[_]])
  }

  test("""Turn points are restored in the aftermath.""") {
    game = new Game(Map(true -> ((Circle, 0, 0) :: Spell(1)),
                                  false -> ((Circle, 0, 0) :: Spell())),
                              Map(true -> 0d, false -> 0d),
                              Map(true -> true, false -> true),
                              Map(true -> 14d, false -> 88d))
    game = getAndTestAftermath(game.getAftermath)
    assert(game.turnPoints(true) === 4d)
    assert(game.turnPoints(false) === 10d)
  }


  test("""The getAftermath causes the correct damage taking into
account the reflection shield.""") {
    game = new Game(Map(true -> ((Circle, 0, 0) :: Spell(1)),
                                  false -> ((Concave, 8, 8) :: Spell())),
                              Map(true -> 10d, false -> 10d),
                              Map(true -> true, false -> true),
                              Map(true -> 88d, false -> 88d))
    game = game.getAftermath.get
    assert(game.gamePoints(true) === 74d)
  }

  test("""Entire game session works as expected""") {
    val first = game
    def doTurn(g: (Option[Game], Int)): Stream[(Option[Game], Int)] ={
      val nu = g._1 flatMap {
        _.compose(Concave, 4, 1, true) } flatMap {
        _.commit(true).commit(false).getAftermath
      }
      (nu, g._2) #:: doTurn((nu, g._2 + 1))
    }

    val sx = doTurn(Option(game), 1) takeWhile { i: (Option[Game], Int) =>
      i._1 match {
        case Some(g) => {
          assert(g.gamePoints(false) >=
            first.gamePoints(false) - i._2 * 15, "Inert player")
          assert(g.gamePoints(true) >=
            first.gamePoints(true) - i._2 * 10, "Active player")
          g.gamePoints(false) > 0 && i._2 < 10
        }
        case None => {
          assert(false, "Composition failed")
          false
        }
      }
    }
    assert(sx.last._1.get.gamePoints(false) >= 10d, "Overall inert player")
    assert(sx.last._1.get.gamePoints(true) > 10d, "Overall active player")
  }

  def getAndTestCompose(o: Option[Game]) ={
    getAndTestGame(o, "compose")
  }
  def getAndTestAftermath(o: Option[Game]) ={
    getAndTestGame(o, "aftermath")
  }
  def getAndTestGame(o: Option[Game], m: String) ={
    o match {
      case Some(g) => g
      case None => {
        assert(false, "Game produced a None on " + m)
        null
      }
    }
  }
}
