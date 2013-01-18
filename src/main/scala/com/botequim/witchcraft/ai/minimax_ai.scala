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
import com.botequim.ai.Minimax

object MinimaxAI extends WitchcraftAI with Minimax {
  type Player = Boolean
  val maxPlayer = false
  val minPlayer = false
  def isTerminal(node: Node): Boolean =
    node.gamePoints(true) == 0.f ||
    node.gamePoints(false) == 0.f
  override def children(n: Node, player: Player): Seq[Node] =
    super.children(n, player) flatMap { i =>
      combinations map { j =>
        child(i, j._1, j._2, j._3, j._4, j._5, !player)
      } map { i => i.getAftermath.get }
    }
  def fae(node: Node, player: Player): Int =
    node.gamePoints(player).toInt
  override def not(player: Player) = !player

  def getMove(sx: Seq[Node], player: Boolean): Node = {
    children(sx.head, player) map {
      i => (apply(i, 1, player), i)
    } minBy {_._1} _2
  }
}
