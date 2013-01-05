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

object DumbAI extends AI{
  def children(n: Node): Seq[Node] = {
    val pointsLimit = WitchcraftGame.pointsPTurnLimit.toInt
    for(i <- combinations)
      yield child(n, i._1, i._2, i._3, i._4, i._5)
  }

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

  def child(node: Node, reflect: Int, mCharge: Int, attack: Int, defense: Int, charge: Int): Node = {
    import Form._
    var result = node
    def compose(f: Form, i: Int, j: Int){
      result = result.compose(f, i, j).getOrElse(result)
    }
    if(reflect == 1) compose(Circle, 0 , 0)
    for(i <- 0 until mCharge) compose(Circle, 0, 0)
    if(attack > 0){
      if(attack == 4) compose(Concave, 4, 1)
      else compose(Concave, attack, attack)
    }
    if(defense > 0) compose(Convex, defense, 0)
    for(i <- 0 until charge) compose(Circle, 0, 0)
    result.commit
  }

  def getMove(node: Node): Node = {
    val grouped = children(node).groupBy[Double] { n => n.getAftermathCalculus(!n.player)("pFinal") }
    val maximal = grouped(grouped.keys.max)
    maximal minBy { n => n.getAftermathCalculus(n.player)("pFinal")}
  }
}
