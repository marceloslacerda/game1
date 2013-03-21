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
    assert(spell.result ===
      Map(Reflect -> 1, Attack -> 10, Defense -> 4,
          Charge -> 1))
  }

  test("Turn results for multiple charges work.") {
    var spell = (Circle, 0, 0) :: (Circle, 0, 0) :: (Concave, 5, 5) :: (Circle, 0, 0) :: Spell()
    assert(spell.result ===
      Map(Reflect -> 1, Attack -> 10, Defense -> 0,
          Charge -> 2))
    spell = (Circle, 0, 0) :: (Circle, 0, 0) :: (Circle, 0, 0) :: (Concave, 5, 5) :: (Circle, 0, 0) :: Spell()
    assert(spell.result ===
      Map(Reflect -> 1, Attack -> 10, Defense -> 0,
          Charge -> 3))
  }

  test("Multiple attacks and defenses are added up.") {
    var spell = (Circle, 0, 0) :: (Concave, 4, 1) :: (Convex, 4, 0) :: (Concave, 5, 5) :: (Convex, 5, 0) :: (Circle, 0, 0) :: Spell()
    assert(spell.result ===
      Map(Reflect -> 1, Attack -> 15, Defense -> 9,
          Charge -> 1))
    spell = (Circle, 0, 0) :: (Concave, 4, 1) :: (Convex, 4, 0) :: (Concave, 5, 5) :: (Convex, 5, 0) :: (Concave, 7,7) :: (Convex, 7, 0) :: (Circle, 0, 0) :: Spell()
    assert(spell.result ===
      Map(Reflect -> 1, Attack -> 29, Defense -> 16,
          Charge -> 1))
  }

  test("Only charges is not possible.") {
    val spell = (Convex, 3, 0) :: (Circle, 0, 0) :: (Circle, 0, 0) :: (Circle, 0, 0) ::
      (Circle, 0, 0) :: (Circle, 0, 0) :: (Circle, 0, 0) :: (Circle, 0, 0) :: Spell()
    assert(spell.result === Map(Reflect -> 1, Attack-> 0, Charge -> 0, Defense -> 21))
  }

  test("Fast charges work.") {
    var spell = (Convex, 4, 0) :: (Circle, 0, 0) :: (Concave, 5, 5) :: (Circle, 0, 0) :: Spell()
    assert(spell.result ===
      Map(Reflect -> 1, Attack -> 10, Defense -> 8,
          Charge -> 0))

    spell = (Convex, 4, 0) :: (Concave, 4, 1) :: (Circle, 0, 0) :: (Circle, 0, 0) :: Spell()
    assert(spell.result ===
      Map(Reflect -> 1, Attack -> 10, Defense -> 8, Charge -> 0))

    spell = (Concave, 4, 1) :: (Convex, 4, 0) :: (Concave, 4, 1) :: (Circle, 0, 0) :: (Circle, 0, 0) :: Spell()
    assert(spell.result ===
      Map(Reflect -> 1, Attack -> 20, Defense -> 8, Charge -> 0))
  }

  test("Fast multiple charges in sequence work.") {
    var spell = (Convex, 4, 0) :: (Circle, 0, 0) :: (Circle, 0, 0) :: (Concave, 5, 5) :: (Circle, 0, 0) :: Spell()
    assert(spell.result ===
      Map(Reflect -> 1, Attack -> 10, Defense -> 12, Charge -> 0))
    spell = (Concave, 4, 1) :: (Convex, 4, 0) :: (Circle, 0, 0) :: (Circle, 0, 0) :: (Circle, 0, 0) :: Spell()

    assert(spell.result ===
      Map(Reflect -> 1, Attack -> 15, Defense -> 12, Charge -> 0))
  }

  test("Fast multiple charges not in sequence work.") {
    var spell = (Convex, 4, 0) :: (Circle, 0, 0) :: (Concave, 5, 5) :: (Circle, 0, 0) :: (Circle, 0, 0) :: Spell()
    assert(spell.result ===
      Map(Reflect -> 1, Attack -> 20, Defense -> 12,
          Charge -> 0))

    spell = (Concave, 4, 1) :: (Circle, 0, 0) :: (Convex, 4, 0) :: (Concave, 4, 1) :: (Circle, 0, 0) :: (Circle, 0, 0) :: Spell()
    assert(spell.result ===
      Map(Reflect -> 1, Attack -> 25, Defense -> 8,
          Charge -> 0))
  }

  test("Multiplier level is applied correctly.") {
    var spell = (Circle, 0, 0) :: (Convex, 4, 0) :: (Concave, 5, 5) :: (Circle, 0, 0) :: Spell(1)
    assert(spell.result ===
      Map(Reflect -> 2, Attack -> 20, Defense -> 8, Charge -> 1))
    spell = (Circle, 0, 0) :: (Convex, 4, 0) :: (Concave, 5, 5) :: (Circle, 0, 0) :: Spell(2)
    assert(spell.result ===
      Map(Reflect -> 3, Attack -> 30, Defense -> 12, Charge -> 1))
  }

  test("All things work together.") {
    val spell = (Circle, 0, 0) :: (Circle, 0, 0) :: (Concave, 4, 1) :: (Circle, 0, 0) :: (Convex, 4, 0) :: (Concave, 4, 1) :: (Circle, 0, 0) :: (Circle, 0, 0) :: Spell(2)
    assert(spell.result ===
      Map(Reflect -> 3, Attack -> 45, Defense -> 16, Charge -> 2))
  }

  test("Reflect power works.") {
    var comb = ((Circle, 0, 0) :: Spell(0)).result
    assert(.5f === Spell.getReflectPower(comb))
    comb = ((Circle, 0, 0) :: Spell(1)).result
    assert(.25f === Spell.getReflectPower(comb))
  }
}
