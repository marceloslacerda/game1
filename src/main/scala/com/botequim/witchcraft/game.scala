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

package com.botequim.witchcraft.rules

object Effect extends Enumeration {
  type Effect = Value
  val Attack, Defense, Reflect, Charge = Value
}

object Form extends Enumeration {
  type Form = Value
  val Circle, Convex, Concave = Value
}

import Effect._
import Form._

 case class WitchcraftGame (spells: Map[Boolean, Spell],
                      turnPoints: Map[Boolean, Double],
                      commits: Map[Boolean, Boolean],
                      gamePoints: Map[Boolean, Double]) {
  import WitchcraftGame._

  def compose(f: Form, points: Int, intersec: Int, player: Boolean): Option[WitchcraftGame] ={
    if(points < 3 && f == Convex ||
       points < 4 && f == Concave ||
       points == 4 && f == Concave && intersec > 1 ||
       f == Concave && points < intersec ||
       commits(player))
      return None
    val pointsWasted = f match {
      case Circle => 1
      case _ => points
    }
    val newTurnPoints = turnPoints(player) - pointsWasted
    val newSpells = spells.updated(player, ((f, points, intersec) :: spells(player)))
    if(newTurnPoints < 0)
      return None
    else
      return Some(new WitchcraftGame(newSpells,
                         turnPoints.updated(player, newTurnPoints),
                         commits,
                         gamePoints))
  }

  def commit(player: Boolean): WitchcraftGame ={
    val nppt = pointsPTurnLimit.min(gamePoints(player))
    new WitchcraftGame(spells,
                   turnPoints.updated(player, nppt),
                   commits.updated(player, true),
                   gamePoints)
  }

  def aftermathCalculus(player: Boolean): (Double, Int, Double, Double, Double, Double, Double, Int) ={
    import Spell._
    val rA = spells(player).result
    val rB = spells(!player).result
    val aPoints = gamePoints(player) - pointsPTurnLimit
    val bAtk = getAttackPower(rB)
    val aRefl = getReflectPower(rA)
    val partialB = (bAtk * aRefl)
    val aDef = getDefensePower(rA)
    val finalB = partialB - aDef
    val finalA = (aPoints - finalB.max(0d)).max(0d)
    (aPoints, bAtk, aRefl, partialB, aDef, finalB, finalA, getChargeLevel(rA))
  }
  def getAftermathCalculus(player: Boolean): Map[String, Double] ={
    val ac = aftermathCalculus(player)
    Map("pPoints" -> ac._1,
        "eAtk" -> ac._2,
        "pRefl" -> ac._3,
        "ePartial" -> ac._4,
        "pDef" -> ac._5,
        "eFinal" -> ac._6,
        "pFinal" -> ac._7,
        "pCharge" -> ac._8)
  }

  def getAftermath: Option[WitchcraftGame] ={
    if(commits exists {!_._2}) return None
    val resultA = aftermathCalculus(true)
    val resultB = aftermathCalculus(false)
    val newGamePoints = Map(
      true -> resultA._7,
      false -> resultB._7)
    val newSpells = Map(
      true -> Spell(resultA._8),
      false -> Spell(resultB._8))
    Option(new WitchcraftGame(newSpells,
                       turnPoints,
                       initialCommitMap,
                       newGamePoints))
  }
}

object WitchcraftGame {
  val pointsPTurnLimit = 10d
  val initialPoints = 100d
  val initialCommitMap = Map(true -> false, false -> false)
  def apply() = new WitchcraftGame(
      Map(true -> Spell(), false -> Spell()),
      Map(true -> pointsPTurnLimit, false -> pointsPTurnLimit),
      initialCommitMap,
      Map(true -> initialPoints, false -> initialPoints))
}

object Spell {
  def apply(m: Int = 0) = new Spell(m, Nil)
  def getAttackPower(comb: Map[Effect, Int]) =
    comb(Attack)
  def getDefensePower(comb: Map[Effect, Int]) =
    comb(Defense)
  def getReflectPower(comb: Map[Effect, Int]) =
    1d/math.pow(2d, comb(Reflect))
  def getChargeLevel(comb: Map[Effect, Int]) =
    comb(Charge)
}

class Spell(lvl: Int, comb: List[(Effect, Int)]) {
  def combination = comb
  def isEmpty = comb.isEmpty
  def level = lvl
  def ::(elem: (Effect, Int)) =
    new Spell(lvl, elem :: combination)

  def ::(tup: (Form, Int, Int)): Spell ={
    val (elem, points, intersects) = tup
    elem match {
      case Circle => {
        if(combination.isEmpty) (Reflect, 1) :: this
        else (Charge, 1) :: this
      }
      case Convex => (Defense, points) :: this
      case Concave =>
        (Attack, points + intersects) :: this
    }
  }

  lazy val result: Map[Effect, Int] = {
    val comb = combination.toArray
    val mult = lvl + 1
    var lcharge = 0
    var defense = 0
    var attack = 0
    var reflect = 0
    if(comb.size > 0) {
      var index = 0
      while(index < comb.size && comb(index)._1 == Charge) {
        lcharge += comb(index)._2
        index += 1
      }
      var rIndex = comb.size -1
      if(comb(rIndex)._1 == Reflect) {
        reflect = comb(rIndex)._2 * mult
        rIndex -= 1
      }
      var partialCharge = mult
      while(index <= rIndex) {
        if(comb(rIndex)._1 == Attack)
          attack += comb(rIndex)._2 * partialCharge
        else if(comb(rIndex)._1 == Defense)
          defense += comb(rIndex)._2 * partialCharge
        else if(comb(rIndex)._1 == Charge) {
          partialCharge += comb(rIndex)._2
        }
        rIndex -= 1
      }
    }
    Map(Reflect -> reflect,
      Attack -> attack,
      Defense -> defense,
      Charge -> lcharge)
  }

  override def toString(): String =
    toStringList.mkString("\n")

  def toStringList: List[String] =
    comb.reverse map {i => i._1 + ": " + i._2}
}
