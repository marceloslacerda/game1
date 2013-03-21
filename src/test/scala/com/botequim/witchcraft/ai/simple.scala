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

package org.botequim.game1.ai

import org.scalatest.{FunSuite, BeforeAndAfter}
import org.botequim.game1.rules.{WitchcraftGame, Effect, Form, Spell}
import Effect._

class FortuneAISuite extends FunSuite with BeforeAndAfter {
  var game: WitchcraftGame = _

  before {
    game = WitchcraftGame.apply
  }

  def assertEqualTurnResult(reflect: Int, mCharge: Int, attack: Int, defense: Int, charge: Int)
                            (eReflect: Int, eAttack: Int, eDefense: Int, eCharge: Int) {
    val comp = FortuneAI.spellComposition(reflect + mCharge, attack, defense, charge)
    assert(FortuneAI.toSpell(comp, false, game).result ===
      Map(Attack -> eAttack, Defense -> eDefense, Charge -> eCharge,
               Reflect -> eReflect))
  }

  test("Empty child.") {
    assertEqualTurnResult(1, 0, 0, 0, 0)(1, 0, 0, 0)
  }

  test("Test attack") {
    assertEqualTurnResult(0, 0, 4, 0, 0)(0, 5, 0, 0)
    assertEqualTurnResult(0, 0, 5, 0, 0)(0, 10, 0, 0)
  }

  test("Test defense") {
    assertEqualTurnResult(0, 0, 0, 3, 0)(0, 0, 3, 0)
  }

  test("Test reflect") {
    assertEqualTurnResult(1, 0, 0, 0, 0)(1, 0, 0, 0)
  }

  test("Test middle charge") {
    /*needs reflect in order to middle charge*/
    assertEqualTurnResult(1, 5, 4, 0, 0)(1, 30, 0, 0)
  }

  test("Test charge") {
    /*needs another spell in order to charge*/
    assertEqualTurnResult(1, 0, 0, 0, 1)(1, 0, 0, 1)
  }

  test("Test all spells") {
    assertEqualTurnResult(1, 0, 5, 3, 1)(1, 10, 3, 1)
  }

  test("Test combinations") {
    val combs = FortuneAI.combinations
    assert(combs.contains((1, 0, 0, 9)))
    assert(combs.contains((1, 4, 4, 1)))
    assert(combs.contains((1, 5, 3, 1)))
    assert(combs.contains((1, 9, 0, 0)))
    assert(combs.distinct.size === combs.size, "There is no combination repetition")

  }

/*  test("Test spell composition") {
    import Form._
    val comp = FortuneAI.spellComposition(0, 5, 5, 0, 0)
    assert(comp.reverse === (Circle, 0, 0) ::
      (Circle, 0, 0) ::
      (Circle, 0, 0) ::
      (Circle, 0, 0) ::
      (Circle, 0, 0) ::
      (Concave, 5, 5) ::
      (Convex, 0, 0) :: Nil)
  }*/

  test("Test children") {
    val prev = System.currentTimeMillis
    val cx = FortuneAI.children(game, false)
    val current = System.currentTimeMillis
    println("Total = " + (current-prev)/1000d)
    val sx = cx.force map {
      _.spells(false).combination
    }
    println("Children size: " + sx.size)
    assert(sx.size === sx.distinct.size, "There is no spell repetition")
    assert(cx.exists {_.spells(false).result == Map(Reflect -> 1, Charge -> 0, Attack -> 0, Defense -> 21)}, "Missing 1")
  }

  test("Test minimal move") {
    val minimalVal = FortuneAI.getMove(Seq(game), false).getAftermathCalculus(true)("pFinal")
    assert(minimalVal === 40)
  }
}
