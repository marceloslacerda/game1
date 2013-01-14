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
import com.botequim.ai.AlphaBetaPrunning
import com.botequim.ai.AI

object MinimaxAI extends WitchcraftAI with AlphaBetaPrunning{
  type Player = Boolean
  val maxPlayer = false
  val minPlayer = false
  def isTerminal(node: Node): Boolean =
    node.availableGamePoints(true) == 0.f ||
    node.availableGamePoints(false) == 0.f
  def children(node: Node, player: Player): Seq[Node] =
    endGameChildren(node)
  def fae(node: Node, player: Player): Int =
    node.availableGamePoints(player).toInt
  override def not(player: Player) = !player

  def endGameChildren(n: Node) = {
    children(n) flatMap { i =>
      combinations map { j =>
        child(i, j._1, j._2, j._3, j._4, j._5)
      }
    }
  }

  def getMove(sx: Seq[Node]): Node = {
    children(sx.head) map {
      i => (apply(i, 1, false), i)
    } minBy {_._1} _2
  }
}
