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

package org.botequim.game1.ai
import org.botequim._
import ai._
import game1.rules.{Game, Form}

class MinimaxAI extends MinimaxAlgorithm[Game] with AI {
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
    children(sx.head, player) maxBy { n => max(n, player, depth) }

  def getMove(sx: Seq[Node], player: Boolean): Node = null
}

class MinimaxAlphaBetaAI extends AlphaBetaMinimax[Game] with AI {
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
    children(sx.head, player) minBy { n => min(n, !player, depth, Double.NegativeInfinity, Double.PositiveInfinity) }

  def getMove(sx: Seq[Node], player: Boolean): Node = getMove(sx, player, 1)
}