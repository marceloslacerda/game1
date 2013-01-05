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


class DumbAISuite extends FunSuite with BeforeAndAfter {
  var game: WitchcraftGame = _

  before {
    game = WitchcraftGame.apply
  }

  def assertEqualTurnResult(reflect: Int, mCharge: Int, attack: Int, defense: Int, charge: Int)
                            (eReflect: Int, eAttack: Int, eDefense: Int, eCharge: Int) {
    val child = DumbAI.child(game, reflect, mCharge, attack, defense, charge)
    assert(child.spells(true).getTurnResult === 
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
    assert(DumbAI.combinations.contains((0, 0, 0, 0, 10)))
    assert(DumbAI.combinations.contains((1, 0, 4, 4, 1)))
    assert(DumbAI.combinations.contains((1, 0, 5, 3, 1)))
    assert(DumbAI.combinations.contains((1, 0, 9, 0, 0)))
  }

  test("Test children") {
    val child = DumbAI.child(game, 0, 6, 4, 0, 0)
    val eFinal = child.getAftermathCalculus(false)("pFinal")
    val attackFinal = child.getAftermathCalculus(false)("eAtk")
    assert(attackFinal === 30)    
    assert(eFinal === 60)
  }

  test("Test minimal move") {
    val minimalVal = DumbAI.getMove(game).getAftermathCalculus(false)("pFinal")
//    println(minimalVal)
    assert(minimalVal === 40)
  }
}
