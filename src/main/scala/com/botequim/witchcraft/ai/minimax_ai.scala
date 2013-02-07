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

trait MinimaxAlgorithm {
  type Node
  def isTerminal(node: Node): Boolean
  def victory(n: Node, player: Boolean): Boolean
  def eval(n: Node,  player: Boolean): Double
  def children(n: Node, player: Boolean): Seq[Node]
  def max(sx: Seq[Node], player: Boolean, depth: Int): Node = {
    if(depth == 0)
      throw new UnsupportedOperationException("Cannot calc max with depth 0")
    sx maxBy { x =>
      val sc = children(x, !player) 
      eval(min(sc, player, depth -1), player)
    }
  }
  def min(sx: Seq[Node], player: Boolean, depth: Int): Node = {
    sx minBy { x =>
      if(depth == 0) eval(x, player)
      else if(isTerminal(x)) {
        if(victory(x, player)) Double.MaxValue
        else Double.MinValue
      }
      else eval(max(children(x, player), player, depth), player)
    }
  }
}


class MinimaxAI extends WitchcraftAI with MinimaxAlgorithm {
  def isTerminal(node: Node): Boolean =
    node.gamePoints(true) == 0d ||
    node.gamePoints(false) == 0d
  def victory(n: Node, player: Boolean) =
    n.gamePoints(player) > 0d && n.gamePoints(!player) == 0d

  def eval(n: Node, player: Boolean): Double =
    n.gamePoints(!player) - n.gamePoints(player)

  override def children(n: Node, player: Boolean) =
    super.children(n, player) map { c => c.getAftermath.getOrElse(c) }

  def getMove(sx: Seq[Node], player: Boolean, depth: Int): Node =
    max(children(sx.head, player), player, depth)

  def getMove(sx: Seq[Node], player: Boolean): Node = null
}
