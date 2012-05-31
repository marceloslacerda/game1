package com.botequim.witchcraft

import rules.Form._
import rules.WitchcraftGame
object ConsoleWitchcraft extends App {
  var game = WitchcraftGame.apply
  val playerName = Map(true -> "A", false -> "B")
  println("Witchcraft")
  var playing = true
  while(playing) {
    println("Scoreboard:")
    println("A -> " + game.score(true) + " B -> " + game.score(false))
    println("Game points:")
    println("A -> " + game.availableGamePoints(true) + " B -> " + game.availableGamePoints(false))
    println("Player " + playerName(game.player) + " movement(Circle, cOnvex, concAve, commIt, Exit game):")
    val c = Console.readChar()
    game = c match {
      case 'I' => game.commit
      case 'E' => {
        playing = false
        game

      }
      case _ => {
        (c match {
          case 'C' => game.compose(Circle, 0, 0)
          case 'O' => {
            println("Number of points used:")
            game.compose(Convex, Console.readInt, 0)
          }
          case 'A' => {
            println("Number of points used:")
            val points = Console.readInt
            println("Number of intersections:")
            val inter = Console.readInt
            game.compose(Concave, points, inter)
          }
          case _ => {
            println("Undefined option: " + c)
            Some(game)
          }
        }) match {
          case Some(g) => g
          case None => game
        }
      }
    }
    println("Points left: " + game.availableTurnPoints)
    println("Combination: " + game.spells(game.player).combination)
  }
}

  
