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

import com.botequim.witchcraft.rules.{WitchcraftGame, Form}
import com.botequim.ai.AI
import Form._

trait WitchcraftNodeGenerator {
  type Node = WitchcraftGame

  def combinations: Seq[(Int, Int, Int, Int, Int)] = {
    val pointsLimit = WitchcraftGame.pointsPTurnLimit.toInt
    for{
      reflect <- Seq(0,1)
      mCharge <- 0 to (pointsLimit - reflect)
      attack <- 0 +: (4 to (pointsLimit - (reflect + mCharge)))
      defense <- 0 +: (3 to (pointsLimit - (reflect + attack + mCharge)))
      charge = pointsLimit - (reflect + attack + defense + mCharge)
    } yield (reflect, mCharge, attack, defense, charge)
  }

  def spellComposition(reflect: Int, mCharge: Int, attack: Int, defense: Int,
    charge: Int): Seq[(Form, Int, Int)] = {
    val fCircles = 0 until (reflect + mCharge) map { i =>
      (Circle, 0, 0)
    }
    val concave = if(attack > 0){
      if(attack == 4) (Concave, 4, 1)
      else (Concave, attack, attack)
    } else (Concave, 0, 0)
    val convex = if(defense > 0) (Convex, defense, 0)
    else (Convex, 0, 0)
    val lCircles = 0 until (charge) map { i =>
      (Circle, 0, 0)
    }
    fCircles.toList ::: concave :: convex :: lCircles.toList
  }

  def children_(node: Node, player: Boolean): Seq[Node] = {
    val compositions = combinations map {i =>
      spellComposition(i._1, i._2, i._3, i._4, i._5)
    } distinct

    compositions map { comp =>

      child(Option(node), comp, player) map { _.commit(player) }
    } filterNot { _ == None } map { _.get }
  }

  def children(node: Node, player: Boolean): Seq[Node] = {
    def distinct(sx: List[Node]): List[Node] =
      if(sx == Nil) sx
      else {
        if(sx.tail exists {x =>
          sx.head.spells(player).combination == x.spells(player).combination
        }) distinct(sx.tail)
        else sx.head :: distinct(sx.tail)
      }

    distinct(children_(node, player).toList)
  }

  def child(node: Option[Node], spell: Seq[(Form, Int, Int)],
              player: Boolean): Option[Node] = {
    if(spell == Nil) node
    else {
      val s = spell.head
      if(s == (Concave, 0, 0) || s == (Convex, 0, 0)) {
        child(node, spell.tail, player)
      } else
        child(node flatMap {
          _.compose(s._1, s._2, s._3, player)
        }, spell.tail, player)
    }
  }
}

trait WitchcraftAI extends AI with WitchcraftNodeGenerator
