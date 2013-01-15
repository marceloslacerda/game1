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
                      availableTurnPoints: Float,
                      commits: Map[Boolean, Boolean],
                      availableGamePoints: Map[Boolean, Float]) {
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
    val newTurnPoints = availableTurnPoints - pointsWasted
    val newSpells = spells.updated(player, ((f, points, intersec) :: spells(player)))
    if(newTurnPoints < 0)
      return None
    else
      return Some(new WitchcraftGame(newSpells,
                         newTurnPoints,
                         commits,
                         availableGamePoints))
  }

  def commit(player: Boolean): WitchcraftGame =
    new WitchcraftGame(spells,
                   pointsPTurnLimit.min(availableGamePoints(player)),
                   commits.updated(player, true),
                   availableGamePoints)

  def getAftermathCalculus(player: Boolean): Map[String, Double] ={
    val rA = spells(player).getTurnResult
    val rB = spells(!player).getTurnResult
    val aPoints = availableGamePoints(player) - pointsPTurnLimit
    import Spell._
    val bAtk = getAttackPower(rB)
    val aRefl = getReflectPower(rA)
    val partialB = (bAtk * aRefl)
    val aDef = getDefensePower(rA)
    val finalB = partialB - aDef
    val finalA = (aPoints - finalB.max(0.)).max(0.)
    Map("pPoints" -> aPoints,
        "eAtk" -> bAtk,
        "pRefl" -> aRefl,
        "ePartial" -> partialB,
        "pDef" -> aDef,
        "eFinal" -> finalB,
        "pFinal" -> finalA,
        "pCharge" -> getChargeLevel(rA))
  }

  def getAftermath: Option[WitchcraftGame] ={
    if(commits exists {!_._2}) return None
    val resultA = getAftermathCalculus(true)
    val resultB = getAftermathCalculus(false)
    val newGamePoints = Map(
      true -> resultA("pFinal").toFloat,
      false -> resultB("pFinal").toFloat)
    val newSpells = Map(
      true -> Spell(resultA("pCharge").toInt),
      false -> Spell(resultB("pCharge").toInt))
    val newPPT = pointsPTurnLimit.min(newGamePoints(true))
    Option(new WitchcraftGame(newSpells,
                       newPPT,
                       initialCommitMap,
                       newGamePoints))
  }
}

object WitchcraftGame {
  val pointsPTurnLimit = 10.f
  val initialPoints = 100.f
  val initialCommitMap = Map(true -> false, false -> false)
  def apply() = new WitchcraftGame(
      Map(true -> Spell(), false -> Spell()),
      pointsPTurnLimit,
      initialCommitMap,
      Map(true -> initialPoints, false -> initialPoints))
}

object Spell {
  def apply(m: Int = 0) = new Spell(m, Nil)
  def getAttackPower(comb: Map[Effect, Int]) =
    comb(Attack).toFloat
  def getDefensePower(comb: Map[Effect, Int]) =
    comb(Defense).toFloat
  def getReflectPower(comb: Map[Effect, Int]) =
    (1. - (1 to comb(Reflect)).map({i =>
      1./math.pow(2., i)
    }).sum)
  def getChargeLevel(comb: Map[Effect, Int]) =
    comb(Charge)
}

class Spell(mult: Int, comb: List[(Effect, Int)]) {
  val combination = comb
  val multLvl = mult

  def ::(elem: (Effect, Int)) =
    new Spell(multLvl, elem :: combination)

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

  def getTurnResult: Map[Effect, Int] = {
    val combination = comb.reverse
    var result: Map[Effect, Int] = Map()
    val multiplier = multLvl + 1
    //First off, reflect
    result += Reflect ->
               ((
                 if(combination.headOption == Some(Reflect, 1))
                   1
                 else
                   0
               ) * multiplier)

    def calcAttackDefense(comb: List[(Effect, Int)],
                          m: Int,
                          prev: Map[Effect, Int]): Map[Effect, Int] = {
      var r: Map[Effect, Int] = Map()
      r ++= prev.filterKeys(k=>{k != Attack && k != Defense})
      //Next is attack
      r += Attack -> (prev.get(Attack).getOrElse(0) +
                 comb.takeWhile(_._1 != Charge)
                     .filter(_._1 == Attack)
                     .map(_._2 * m).sum)
      //Then defense
      r += Defense -> (prev.get(Defense).getOrElse(0) +
                 comb.takeWhile(_._1 != Charge)
                     .filter(_._1 == Defense)
                     .map(_._2 * m).sum)
      r
    }

    result = calcAttackDefense(combination, multiplier,
                               result)
    //Now charge
    result += Charge -> (combination.reverse.takeWhile(
                 _._1 == Charge).length)
    /** Multiplier left out to avoid
     * GEBOD(Gigant Energy Ball of Death)
     */

    //Finally fast charges

    def getFChargedSection(list: List[(Effect, Int)]) =
      list.dropWhile(
        _._1 != Charge)

    var charged = getFChargedSection(combination)
    var m = multiplier
    while(!charged.isEmpty) {
      m += charged.takeWhile(_._1 == Charge).map(i => 1).sum
      charged = charged.dropWhile(_._1 == Charge)
      result = calcAttackDefense(charged, m, result)
      charged = getFChargedSection(charged)
    }
    result
  }

  override def toString(): String =
    comb.head._1 + ": " + comb.head._2

  def toStringList(): List[String] =
    comb.reverse map {i => i._1 + ": " + i._2}
}
