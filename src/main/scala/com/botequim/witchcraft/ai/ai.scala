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

import com.botequim.witchcraft.rules._
import com.botequim.ai.AI
import Form._

trait WitchcraftNodeGenerator {
  type Node = WitchcraftGame

  val combinations: Seq[(Int, Int, Int, Int)] = {
    val pointsLimit = WitchcraftGame.pointsPTurnLimit.toInt
    for {
      mCharge <- 0 to pointsLimit
      attack <- 0 +: (4 to (pointsLimit - mCharge))
      defense <- 0 +: (3 to (pointsLimit - (attack + mCharge)))
      charge = pointsLimit - (attack + defense + mCharge)
    } yield (mCharge, attack, defense, charge)
  }

    import Effect._
  def spellComposition(mCharge: Int, attack: Int, defense: Int,
    charge: Int): List[(Effect, Int)] = {

    var comp: List[(Effect, Int)] = Nil
    if(mCharge > 0) {
      comp ::= (Reflect, 1)
      if(mCharge > 1) comp ::= (Charge, mCharge - 1)
    }
    if(attack > 0) comp ::= (Attack, if(attack == 4) 5 else 2 * attack)
    if(defense > 0) comp ::= (Defense, defense)
    for(i <- 0 until charge) comp ::= (Charge, 1)
    comp
  }

  val preparedCompositions = combinations map { i =>
      spellComposition(i._1, i._2, i._3, i._4)
  }

  def toSpell(sx: List[(Effect, Int)], player: Boolean, node: Node): Spell =
    new Spell(node.spells(player).level, sx)

  def child(spell: Spell, player: Boolean, node: Node): Node =
    new WitchcraftGame(node.spells.updated(player, spell),
      node.turnPoints,
      node.commits,
      node.gamePoints)

  def children(node: Node, player: Boolean): Seq[Node] = {
    preparedCompositions map {i =>
      child(toSpell(i, player, node), player, node).commit(player)
    }
  }

}

trait WitchcraftAI extends AI with WitchcraftNodeGenerator
