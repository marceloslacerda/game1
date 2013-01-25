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

  def combinations: List[(Int, Int, Int, Int, Int)] = {
    val pointsLimit = WitchcraftGame.pointsPTurnLimit.toInt
    for{
      reflect <- List(0,1)
      mCharge <- 0 to (pointsLimit - reflect)
      attack <- 0 +: (4 to (pointsLimit - (reflect + mCharge)))
      defense <- 0 +: (3 to (pointsLimit - (reflect + attack + mCharge)))
      charge = pointsLimit - (reflect + attack + defense + mCharge)
    } yield (reflect, mCharge, attack, defense, charge)
  }

  def spellComposition(reflect: Int, mCharge: Int, attack: Int, defense: Int,
    charge: Int): List[(Form, Int, Int)] = {
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
       lCircles.toList ::: convex :: concave :: fCircles.toList
  }

  def children_(node: Node, player: Boolean): List[Node] = {
    val compositions = combinations map {i =>
      spellComposition(i._1, i._2, i._3, i._4, i._5)
    }

    compositions map { comp =>

      child_iter(Option(node), comp, player) map { _.commit(player) }
    } filterNot { _ == None } map { _.get }
  }

  def distinct_iter(sx: List[Node], player: Boolean): List[Node] = {
    var dist: List[Node] = Nil
    for(x <- sx) {
      if(dist forall { _.spells(player).combination != x.spells(player).combination })
        dist = x :: dist
    }
    dist
  }

  def distinct(sx: List[Node], player: Boolean): List[Node] = sx match {
    case Nil => Nil
    case y :: sy =>
      if(sy exists {x =>
        y.spells(player).combination == x.spells(player).combination
      }) distinct(sy, player)
      else y :: distinct(sy, player)
  }

  def children(node: Node, player: Boolean): Seq[Node] = {
//    var prev = System.currentTimeMillis
    val sc_ = children_(node, player)
//    var current = System.currentTimeMillis
//    println("Ch = " + (current-prev)/1000.)
//    prev = System.currentTimeMillis
    val sc = distinct_iter(sc_, player)
//    current = System.currentTimeMillis
//    println("Di = " + (current-prev)/1000.)
    sc
  }

  def child_iter(node: Option[Node], sx: List[(Form, Int, Int)],
              player: Boolean): Option[Node] = {
    var res = node
    for(x <- sx) {
      if(x != (Concave, 0, 0) && x != (Convex, 0, 0))
        res = res flatMap { _.compose(x._1, x._2, x._3, player) }
    }
    res
  }

  def child(node: Option[Node], spell: List[(Form, Int, Int)],
              player: Boolean): Option[Node] = spell match {
    case Nil => node
    case x :: sx =>
      if(x == (Concave, 0, 0) || x == (Convex, 0, 0)) {
        child(node, sx, player)
      } else
        child(node flatMap {
          _.compose(x._1, x._2, x._3, player)
        }, sx, player)
  }
}

trait WitchcraftAI extends AI with WitchcraftNodeGenerator
