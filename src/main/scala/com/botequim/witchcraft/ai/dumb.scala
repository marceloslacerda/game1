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
