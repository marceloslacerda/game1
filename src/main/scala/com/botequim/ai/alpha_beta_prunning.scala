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

package com.botequim.ai

trait AlphaBetaPrunning {
  type Player
  type Node
  val maxPlayer : Player
  val minPlayer : Player
  def isTerminal(node: Node): Boolean
  def children(node: Node, player: Player): Seq[Node]
  def fae(node: Node, player: Player): Int
  def not(player: Player) : Player = if(player == maxPlayer)minPlayer else maxPlayer
  def println(stream: String) {
    if(false) print(stream+"\n")
  }
  def apply(node: Node, depth: Int, player: Player): Int = {
    def alphaBeta(node: Node, depth: Int, alpha: Int, beta: Int, player: Player): Int = {
      var currentBeta = beta
      var currentAlpha = alpha
      if(depth == 0 || isTerminal(node)){
        val result = fae(node, player)
        println("FAE: "+result)
        return result
      }
      else if(player == maxPlayer){
        for(child <- children(node, player)){
          val childMinimax = alphaBeta(child, depth - 1, currentAlpha, beta, not(player))
          println("Current Alpha: " + currentAlpha)
          println("Child minimax: " + childMinimax)
          currentAlpha = currentAlpha.max(childMinimax)
          println("New Alpha: " + currentAlpha)
          if(beta <= currentAlpha){
            println("#Cutoff alpha\n" + node)
            println("Current Alpha: " + currentAlpha)
            println("Beta: "+beta+"#")
            return currentAlpha
          }
        }
        return currentAlpha
      }
          else {
            for(child <- children(node, player)){
              val childMinimax = alphaBeta(child, depth - 1, alpha, currentBeta, not(player))
              println("Current Beta: " + currentBeta)
              println("Child minimax: " + childMinimax)
              currentBeta = currentBeta.min(childMinimax)
              println("New beta: " + currentBeta)
              if(currentBeta <= alpha){
                println("#Cutoff beta\n" + node)
                println("Current beta: "+currentBeta)
                println("Alfa: "+alpha+"#")
                return currentBeta
              }
            }
            return currentBeta
          }
    }
    val minimax = alphaBeta(node, depth, Int.MinValue, Int.MaxValue, player)
    println("Minimax: "+minimax)
    return minimax
  }
}
