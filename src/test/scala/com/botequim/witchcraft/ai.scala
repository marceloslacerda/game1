package com.botequim.witchcraft.ai

import org.scalatest.{FunSuite, BeforeAndAfter}
import com.botequim.witchcraft.rules.{WitchcraftGame, Effect}
import Effect._


class DumbAISuite extends FunSuite with BeforeAndAfter {
  var game: WitchcraftGame = _

  before {
    game = WitchcraftGame.apply
  }

  def assertEqualTurnResult(reflect: Int, attack: Int, defense: Int, charge: Int)
                            (eReflect: Int, eAttack: Int, eDefense: Int, eCharge: Int) {
    val child = DumbAI.child(game, reflect, attack, defense, charge)
    assert(child.spells(true).getTurnResult === 
        Map(Attack -> eAttack, Defense -> eDefense, Charge -> eCharge,
            Reflect -> eReflect))
  }

  test("Empty child.") {
    assertEqualTurnResult(0, 0, 0, 0)(0, 0, 0, 0)
  }

  test("Test attack") {
    assertEqualTurnResult(0, 4, 0, 0)(0, 8, 0, 0)
  }

  test("Test defense") {
    assertEqualTurnResult(0, 0, 3, 0)(0, 0, 3, 0)
  }

  test("Test reflect") {
    assertEqualTurnResult(1, 0, 0, 0)(1, 0, 0, 0)
  }

  test("Test charge") {
    /*needs another spell in order to charge*/
    assertEqualTurnResult(0, 4, 0, 1)(0, 8, 0, 1)
  }

  test("Test all spells") {
    assertEqualTurnResult(1, 5, 3, 1)(1, 10, 3, 1)
  }

  test("Test combinations") {
    assert(DumbAI.combinations.contains((0, 0, 0, 10)))
    assert(DumbAI.combinations.contains((1, 4, 4, 1)))
    assert(DumbAI.combinations.contains((1, 5, 3, 1)))
    assert(DumbAI.combinations.contains((1, 9, 0, 0)))
  }

  test("Test children") {
    val child = DumbAI.child(game, 0, 4, 0, 0)
    val eFinal = child.getAftermathCalculus(false)("pFinal")
    assert(eFinal === 82)
  }

  test("Test minimal move") {
    val minimalVal = DumbAI.getMove(game).getAftermathCalculus(false)("pFinal")
    println(minimalVal)
    assert(minimalVal === 70)
  }
}
