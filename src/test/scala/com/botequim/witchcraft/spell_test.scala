package com.botequim.witchcraft.rules

import org.scalatest.FunSuite
import Form._
import Effect._

class SpellSuite extends FunSuite {
 
  test("Spells are composed nicelly.") {
    val spell = (Circle, 0, 0) :: (Convex, 4, 0) :: (Concave, 5, 5) :: (Circle, 0, 0) :: Spell()
    assert(List((Reflect, 1), (Attack, 10), (Defense, 4),
                (Charge, 1)) === spell.combination.reverse)
  }

  test("Turn results for simple spells are calculated nicelly.") {
    val spell = (Circle, 0, 0) :: (Convex, 4, 0) :: (Concave, 5, 5) :: (Circle, 0, 0) :: Spell()
    assert(spell.getTurnResult ===
      Map(Reflect -> 1, Attack -> 10, Defense -> 4,
          Charge -> 1))
  }

  test("Turn results for multiple charges work.") {
    var spell = (Circle, 0, 0) :: (Circle, 0, 0) :: (Concave, 5, 5) :: (Circle, 0, 0) :: Spell()
    assert(spell.getTurnResult ===
      Map(Reflect -> 1, Attack -> 10, Defense -> 0,
          Charge -> 2))
    spell = (Circle, 0, 0) :: (Circle, 0, 0) :: (Circle, 0, 0) :: (Concave, 5, 5) :: (Circle, 0, 0) :: Spell()
    assert(spell.getTurnResult ===
      Map(Reflect -> 1, Attack -> 10, Defense -> 0,
          Charge -> 3))
  }

  test("Multiple attacks and defenses are added up.") {
    var spell = (Circle, 0, 0) :: (Concave, 4, 1) :: (Convex, 4, 0) :: (Concave, 5, 5) :: (Convex, 5, 0) :: (Circle, 0, 0) :: Spell()
    assert(spell.getTurnResult ===
      Map(Reflect -> 1, Attack -> 15, Defense -> 9,
          Charge -> 1))
    spell = (Circle, 0, 0) :: (Concave, 4, 1) :: (Convex, 4, 0) :: (Concave, 5, 5) :: (Convex, 5, 0) :: (Concave, 7,7) :: (Convex, 7, 0) :: (Circle, 0, 0) :: Spell()
    assert(spell.getTurnResult ===
      Map(Reflect -> 1, Attack -> 29, Defense -> 16,
          Charge -> 1))
  }

  test("Fast charges work.") {
    var spell = (Convex, 4, 0) :: (Circle, 0, 0) :: (Concave, 5, 5) :: (Circle, 0, 0) :: Spell()
    assert(spell.getTurnResult ===
      Map(Reflect -> 1, Attack -> 10, Defense -> 8,
          Charge -> 0))

    spell = (Convex, 4, 0) :: (Concave, 4, 1) :: (Circle, 0, 0) :: (Circle, 0, 0) :: Spell()
    assert(spell.getTurnResult ===
      Map(Reflect -> 1, Attack -> 10, Defense -> 8,
          Charge -> 0))

    spell = (Concave, 4, 1) :: (Convex, 4, 0) :: (Concave, 4, 1) :: (Circle, 0, 0) :: (Circle, 0, 0) :: Spell()
    assert(spell.getTurnResult ===
      Map(Reflect -> 1, Attack -> 20, Defense -> 8,
          Charge -> 0))
  }

  test("Fast multiple charges in sequence work.") {
    var spell = (Convex, 4, 0) :: (Circle, 0, 0) :: (Circle, 0, 0) :: (Concave, 5, 5) :: (Circle, 0, 0) :: Spell()
    assert(spell.getTurnResult ===
      Map(Reflect -> 1, Attack -> 10, Defense -> 16,
          Charge -> 0))

    spell = (Concave, 4, 1) :: (Convex, 4, 0) :: (Concave, 4, 1) :: (Circle, 0, 0) :: (Circle, 0, 0) :: (Circle, 0, 0) :: Spell()
    assert(spell.getTurnResult ===
      Map(Reflect -> 1, Attack -> 40, Defense -> 16,
          Charge -> 0))
  }

  test("Fast multiple charges not in sequence work.") {
    var spell = (Convex, 4, 0) :: (Circle, 0, 0) :: (Concave, 5, 5) :: (Circle, 0, 0) :: (Circle, 0, 0) :: Spell()
    assert(spell.getTurnResult ===
      Map(Reflect -> 1, Attack -> 20, Defense -> 16,
          Charge -> 0))

    spell = (Concave, 4, 1) :: (Circle, 0, 0) :: (Convex, 4, 0) :: (Concave, 4, 1) :: (Circle, 0, 0) :: (Circle, 0, 0) :: Spell()
    assert(spell.getTurnResult ===
      Map(Reflect -> 1, Attack -> 30, Defense -> 8,
          Charge -> 0))
  }

  test("Multiplier level is applied correctly.") {
    var spell = (Circle, 0, 0) :: (Convex, 4, 0) :: (Concave, 5, 5) :: (Circle, 0, 0) :: Spell(1)
    assert(spell.getTurnResult ===
      Map(Reflect -> 2, Attack -> 20, Defense -> 8,
          Charge -> 1))
    spell = (Circle, 0, 0) :: (Convex, 4, 0) :: (Concave, 5, 5) :: (Circle, 0, 0) :: Spell(2)
    assert(spell.getTurnResult ===
      Map(Reflect -> 4, Attack -> 40, Defense -> 16,
          Charge -> 1))
  }

  test("All things work together.") {
    val spell = (Circle, 0, 0) :: (Circle, 0, 0) :: (Concave, 4, 1) :: (Circle, 0, 0) :: (Convex, 4, 0) :: (Concave, 4, 1) :: (Circle, 0, 0) :: (Circle, 0, 0) :: Spell(2)
    assert(spell.getTurnResult ===
      Map(Reflect -> 4, Attack -> 120, Defense -> 32,
          Charge -> 2))
  }

  test("Reflect power works.") {
    var comb = ((Circle, 0, 0) :: Spell(0)).getTurnResult
    assert(.5f === Spell.getReflectPower(comb))
    comb = ((Circle, 0, 0) :: Spell(1)).getTurnResult 
    assert(.25f === Spell.getReflectPower(comb))
  }
}
