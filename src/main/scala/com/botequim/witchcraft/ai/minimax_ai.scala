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

class MinimaxAI extends MinimaxAlgorithm[WitchcraftGame] with WitchcraftAI {
  def isTerminal(node: Node): Boolean =
    node.gamePoints(true) == 0d ||
    node.gamePoints(false) == 0d
  def victoryOrDraw(n: Node, player: Boolean) =
    n.gamePoints(!player) == 0d

  def eval(n: Node, player: Boolean): Double =
    n.gamePoints(player) - n.gamePoints(!player) + n.spells(player).level * 0.01

  override def children(n: Node, player: Boolean) =
    super.children(n, player) map { c => c.getAftermath.getOrElse(c) }

  def getMove(sx: Seq[Node], player: Boolean, depth: Int): Node =
    max(children(sx.head, player), player, depth)

  def getMove(sx: Seq[Node], player: Boolean): Node = null
}

class MinimaxAlphaBetaAI extends AlphaBetaMinimax[WitchcraftGame] with WitchcraftAI {
  def isTerminal(node: Node): Boolean =
    node.gamePoints(true) == 0d ||
    node.gamePoints(false) == 0d
  def victoryOrDraw(n: Node, player: Boolean) =
    n.gamePoints(!player) == 0d

  def eval(n: Node, player: Boolean): Double =
    n.gamePoints(player) - n.gamePoints(!player) + n.spells(player).level * 0.01

  override def children(n: Node, player: Boolean) =
    super.children(n, player) map { c => c.getAftermath.getOrElse(c) }

  def getMove(sx: Seq[Node], player: Boolean, depth: Int): Node =
    max(children(sx.head, player), player, depth, Double.NegativeInfinity, Double.PositiveInfinity)

  def getMove(sx: Seq[Node], player: Boolean): Node = null
}
