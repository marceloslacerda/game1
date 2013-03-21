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

import org.scalatest.{FunSuite, BeforeAndAfter}
import org.botequim._
import game1.rules.{Game, Effect, Form, Spell}
import ai._
import Effect._

class MinimaxAISuite extends FunSuite with BeforeAndAfter {
  var game: Game = _
  val ai = new MinimaxAI()

  before {
    game = Game.apply
  }

  object TestMinimax extends MinimaxAlgorithm[Int] {
    type Node = Int
    def isTerminal(n: Node) = n == 0 || n == 10
    def victoryOrDraw(n: Node, p: Boolean) = eval(n, p) == 10.0
    def eval(n: Node, p: Boolean) = n.toDouble
    def children(n: Node, player: Boolean): Seq[Node] = {
      n match {
        case 0 => Seq(1, 1, 1)
        case 1 => Seq(1, 2, 3)
        case 2 => Seq(7, 0, 1)
        case 3 => Seq(5, 4, 10)
        case 4 => Seq(3, 3, 2)
        case 5 => Seq(5, 0, 1)
        case 7 => Seq(0, 2, 3)
        case 10 => Seq(1, 2, 3)

      }
    }

    def getMove(n: Node, player: Boolean, depth: Int): Node =
      Seq(1, 2, 3) maxBy { n =>
        val mx = max(n, player, depth)
        println(mx, "max")
        mx}
  }

  test("Test minimax search") {
    assert(TestMinimax.getMove(1 , true, 1) === 1)
  }

  test("Depth 0") {
    intercept[UnsupportedOperationException] {
      ai.getMove(game :: Nil, false, 0)
    }
  }

/*  test("Depth 1") {
    val movement = timed {() => ai.getMove(game :: Nil, false, 1)}
    val res = Map(Reflect -> 1, Charge -> 0, Attack -> 50, Defense -> 0)
    assert(movement.spells(false).result === res)
  }*/

  test("Depth 1 both") {
    println("Both")
    timed {() => ai.getMove(game :: Nil, false, 1)}
    timed {() => (new MinimaxAlphaBetaAI()).getMove(game :: Nil, false, 1)}
  }
/*  test("Depth 2") {
    val ai = new MinimaxAlphaBetaAI()
    val movement = timed {() => ai.getMove(game :: Nil, false, 2)}
    val res = Map(Reflect -> 1, Charge -> 0, Attack -> 50, Defense -> 0)
    assert(movement.spells(false).result === res)
  }

  test("Depth 2 vs") {
    val ai = new MinimaxAlphaBetaAI()
    var g = game
    var player = true
    var count = 0
    while(!(g.gamePoints.values exists { _ == 0 }) && count < 20) {
      g = ai.getMove(g :: Nil, player, 2)
      println("Player: " + player + " score: " + g.gamePoints(player) + " move: " + g.spells(player).result)
      if(player){
        g = g.commit(player)
      }
      else {
        println(g.gamePoints.values)
      }
      player = !player
      count += 1
    }
  }

  test("Vs player cheating") {
    val ai = new MinimaxAlphaBetaAI()
    var g = game
    var player = true
    var count = 0
    import Form._
    val spell = (Circle, 0, 0) :: (Concave, 4, 1) :: (Circle, 0, 0) :: (Circle, 0, 0) :: (Circle, 0, 0) :: (Circle, 0, 0) :: (Circle, 0, 0) :: Nil
    while(!(g.gamePoints.values exists { _ == 0 }) && count < 20) {
      if(player){
        for(i <- spell) g = g.compose(i._1, i._2, i._3, player).get
        g = g.commit(player)
      }
      else {
        g = ai.getMove(g :: Nil, player, 1)
        println(g.gamePoints.values)
      }
      println("Player: " + player + " score: " + g.gamePoints(player) + " move: " + g.spells(player).result)
      player = !player
      count += 1
    }
  }

  test("Vs player no cheat") {
    val ai = new MinimaxAlphaBetaAI()
    var g = game
    var gprev = g
    var player = true
    var count = 0
    import Form._
    val spell = (Circle, 0, 0) :: (Concave, 4, 1) :: (Circle, 0, 0) :: (Circle, 0, 0) :: (Circle, 0, 0) :: (Circle, 0, 0) :: (Circle, 0, 0) :: Nil
    while(!(g.gamePoints.values exists { _ == 0 }) && count < 20) {
      if(player){
        println("Turn " + count / 2)
        gprev = g
        for(i <- spell) {
          g.compose(i._1, i._2, i._3, player) match {
            case Some(x) => g = x
            case None => println("Skipped: " + g.turnPoints(player))
          }
        }
        g = g.commit(player)
        println("Player: " + player + " score: " + g.gamePoints(player) + " move: " + g.spells(player).result)
      }
      else {
        val gai = ai.getMove(gprev :: Nil, player, 3)
        println("Player: " + player + " score: " + gai.gamePoints(player) + " move: " + gai.spells(player).result)
        g = fuse(g, gai)
//        println(g)
        g = g.getAftermath.get
        println(g.gamePoints.values)
      }
      player = !player
      count += 1
    }
  }
 */

  test("Vs random no cheat") {
    val ai = new MinimaxAlphaBetaAI()
    var g = game
    var gprev = g
    var player = true
    var count = 0
    while(!(g.gamePoints.values exists { _ == 0 }) && count < 20) {
      if(player){
        println("Turn " + count / 2)
        gprev = g
        g = RandomAI.getMove(g :: Nil, player)
        g = g.commit(player)
        println("Player: " + player + " score: " + g.gamePoints(player) + " move: " + g.spells(player).result)
      }
      else {
        val gai = ai.getMove(gprev :: Nil, player, 1)
        println("Player: " + player + " score: " + gai.gamePoints(player) + " move: " + gai.spells(player).result)
        g = fuse(g, gai)
        g = g.getAftermath.get
        println(g.gamePoints.values)
      }
      player = !player
      count += 1
    }
  }

  def fuse(g1: Game, g2: Game): Game =
    new Game(g1.spells.updated(false, g2.spells(false)),
      Map(true -> 0.0, false -> 0.0),
      Map(true -> true, false -> true),
      g1.gamePoints)

  def timed(f: () => Game): Game = {
    val prev = System.currentTimeMillis
    val ret = f()
    val current = System.currentTimeMillis
    println("Time = " + (current-prev)/1000d)
    ret
  }
}
