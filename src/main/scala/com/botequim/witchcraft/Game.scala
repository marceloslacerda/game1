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

class WitchcraftGame (s: Map[Boolean, Int],
                      spe: Map[Boolean, Spell],
                      play: Boolean,
                      availT: Float,
                      availG: Map[Boolean, Float]){
  import WitchcraftGame.{pointsPTurnLimit, initialPoints}
  val score = s
  val player = play
  val spells = spe
  val availableTurnPoints = availT
  val availableGamePoints = availG
  def compose(f: Form, points: Int, intersec: Int): Option[WitchcraftGame] ={
    if(points < 3 && f == Convex)
      return None
    else if(points < 4 && f == Concave)
      return None
    val pointsWasted = f match {
      case Circle => 1
      case _ => points
    }
    val newTurnPoints = availableTurnPoints - pointsWasted
    val newGamePoints = availableGamePoints(player) - pointsWasted
    val newSpells = Map(!player -> spells(!player),
                        player -> ((f, points, intersec) :: spells(player)))
    if(newTurnPoints < 0 || newGamePoints < 0)
      return None
    else
      return Some(new WitchcraftGame(score,
                         newSpells,
                         player,
                         newTurnPoints,
                         Map(!player ->
                             availableGamePoints(!player),
                             player -> newGamePoints)))
  }
  def commit: WitchcraftGame ={
    if(player) 
      new WitchcraftGame(score,
                   spells,
                   !player,
                   pointsPTurnLimit,
                   availableGamePoints)
    else
      getAftermath
  }

  def getAftermath: WitchcraftGame ={
    import Spell._
    val rA = spells(true).getTurnResult
    val rB = spells(false).getTurnResult
    
    val damageTakenA = (
      getAttackPower(rB)
      * (1.0f - getReflectPower(rA))
      - getDefensePower(rA))
    val damageTakenB = (
      getAttackPower(rA)
      * (1.0f-getReflectPower(rB))
      - getDefensePower(rB))
    var newGamePoints = Map(
      true -> (availableGamePoints(true)
      - damageTakenA),
      false -> (availableGamePoints(true)
      - damageTakenB))
    val newSpells = Map(
      true -> Spell(getChargeLevel(rA)),
      false -> Spell(getChargeLevel(rB)))
    val (addA, addB) =
      (newGamePoints(true), newGamePoints(false)) match {
        case (0.f, 0.f) => (0, 0)
        case (0.f, _) => (0, 1)
        case (_, 0.f) => (1, 0)
        case (_, _) => (0, 0)
      }

    if(!newGamePoints.keys.forall(newGamePoints(_) > 0))
      newGamePoints = Map(true -> initialPoints,
                          false -> initialPoints)

    val newScore = Map(true -> (score(true) + addA),
                       false -> (score(false) + addB))
    new WitchcraftGame(newScore,
                       newSpells,
                       !player,
                       pointsPTurnLimit,
                       newGamePoints)
  }
}

object WitchcraftGame {
  val combinationMaxSize = 4
  val pointsPTurnLimit = 10
  val initialPoints = 88
  def apply() = new WitchcraftGame(
      Map(true -> 0, false -> 0),
      Map(true -> Spell(), false -> Spell()),
      true,
      pointsPTurnLimit,
      Map(true -> initialPoints, false -> initialPoints))
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
    val multiplier = math.pow(2, multLvl).toInt
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
                 comb.takeWhile(
                   _._1 != Charge).filter(
                   _._1 == Attack).map(_._2 * m).sum)
      //Then defense
      r += Defense -> (prev.get(Defense).getOrElse(0) +
                 comb.takeWhile(
                   _._1 != Charge).filter(
                   _._1 == Defense).map(_._2 * m).sum)
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
      m *= math.pow(2, charged.takeWhile(_._1 == Charge).map(_._2).sum).toInt
      charged = charged.dropWhile(_._1 == Charge)
      result = calcAttackDefense(charged, m, result)
      charged = getFChargedSection(charged)
    }
    result
  }
}

object Spell {
  def apply(m: Int = 0) = new Spell(m, Nil)
  def getAttackPower(comb: Map[Effect, Int]) =
    comb(Attack).toFloat
  def getDefensePower(comb: Map[Effect, Int]) =
    comb(Defense).toFloat
  def getReflectPower(comb: Map[Effect, Int]) =
    (1 to comb(Reflect)).map(i => {
      1./math.pow(2., i)
    }).sum.toFloat
  def getChargeLevel(comb: Map[Effect, Int]) =
    comb(Charge)
}
