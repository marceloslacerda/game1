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

package com.botequim.witchcraft.swing

import scala.swing.Panel
import collection.immutable.Stack
import com.botequim.witchcraft.rules.{WitchcraftGame, Spell, Form}
import Form._

trait WitchcraftBoard {
  var gameStates = Stack(WitchcraftGame())
  var ai: Option[com.botequim.witchcraft.ai.AI] = None
  lazy val canvas = new Canvas()
  val spellPanel = new SpellPanel()
  def changeTPoints(tp: String): Unit
  canvas.mouseReleasedListener = Option({() => 
    changeTPoints(availTPoints.toString)
    if(spellPanel.aftermath) spellPanel.clearSpells()
  })
  canvas.undoListener = Option({() =>
    if(gameStates.length > 1) {
      gameStates = gameStates.pop
      spellPanel.popSpell(gameStates.head.player)
      changeTPoints(availTPoints.toString)
    }
  })

  def formType: Form ={
    if(canvas.circle) Circle
    else if(canvas.intersectionTotal > 0) Concave
    else Convex
  }

  def addSpellToList(s: Spell) {
    spellPanel.addSpell(s.toString, gameStates.head.player)
  }
  def clearSpellList() {
    spellPanel.clearSpells()
  }

  def availTPoints =
    gameStates.head.availableTurnPoints -
      (if(canvas.circle) 1 else canvas.points.size)

  def doCommit() {
    addForm()
    gameStates = Stack(gameStates.head.commit)

    if(!ai.isEmpty) {
      aiMove()
    }

    if(gameStates.head.player) {
      val current = gameStates.head
      spellPanel.aftermath = true
      spellPanel.updateScore(current)
      gameStates = Stack(current.getAftermath)      
    }
  }

  def aiMove() {
    gameStates = Stack(ai.get.getMove(gameStates.head))
    val aiBol = !gameStates.head.player
    gameStates.head.spells(aiBol)
      .toStringList foreach {i => spellPanel.addSpell(i, aiBol)}
  }

  def addForm() {
    val current = gameStates.head
    val newOptionalState = formType match {
      case Circle => {
        current.compose(Circle, 0, 0)
      }
      case Convex => {
        current.compose(Convex, canvas.points.size, 0)
      }
      case Concave => {
        current.compose(Concave,
                        canvas.points.size,
                        canvas.intersectionTotal)
      }
    }
    if(newOptionalState != None) {
      gameStates = gameStates push newOptionalState.get
      addSpellToList(gameStates.head.currentSpell)
    }
    canvas.clearCanvas()
  }
}
