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

package com.botequim.witchcraft.ai

import org.scalatest.{FunSuite, BeforeAndAfter}
import com.botequim.witchcraft.rules.{WitchcraftGame, Effect}
import Effect._


class FortuneAISuite extends FunSuite with BeforeAndAfter {
  var game: WitchcraftGame = _

  before {
    game = WitchcraftGame.apply
  }

  def assertEqualTurnResult(reflect: Int, mCharge: Int, attack: Int, defense: Int, charge: Int)
                            (eReflect: Int, eAttack: Int, eDefense: Int, eCharge: Int) {
    val child = FortuneAI.child(game, reflect, mCharge, attack, defense, charge, false)
    assert(child.spells(false).getTurnResult === 
        Map(Attack -> eAttack, Defense -> eDefense, Charge -> eCharge,
            Reflect -> eReflect))
  }

  test("Empty child.") {
    assertEqualTurnResult(0, 0, 0, 0, 0)(0, 0, 0, 0)
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
    assert(FortuneAI.combinations.contains((0, 0, 0, 0, 10)))
    assert(FortuneAI.combinations.contains((1, 0, 4, 4, 1)))
    assert(FortuneAI.combinations.contains((1, 0, 5, 3, 1)))
    assert(FortuneAI.combinations.contains((1, 0, 9, 0, 0)))
  }

  test("Test children") {
    val child = FortuneAI.child(game, 0, 6, 4, 0, 0, false)
    val eFinal = child.getAftermathCalculus(true)("pFinal")
    val attackFinal = child.getAftermathCalculus(true)("eAtk")
    assert(attackFinal === 30)    
    assert(eFinal === 60)
  }

  test("Test minimal move") {
    val minimalVal = FortuneAI.getMove(Seq(game), false).getAftermathCalculus(true)("pFinal")
    assert(minimalVal === 40)
  }
}

class MinimaxAISuite extends FunSuite with BeforeAndAfter {
  var game: WitchcraftGame = _

  before {
    game = WitchcraftGame.apply
  }

  test("Test branching size") {
    val before = Runtime.getRuntime().freeMemory
    val c = MinimaxAI.children(game, false)
    val used = Runtime.getRuntime().freeMemory - before
    println("%s MB used".format(used/1000/1000))
    assert(c.length === 26896)
    println("Ok done")
  }

/*  test("Eventual attack") {
    println("Yep ok")
    val movement = MinimaxAI.getMove(game :: Nil, false)
    assert(movement.spells(false).isEmpty === true)
  }*/
}
