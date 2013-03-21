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

package org.botequim.game1.swing

import scala.swing.Panel
import collection.mutable.Stack
import org.botequim.game1._
import rules.{WitchcraftGame, Spell, Form}
import ai.WitchcraftAI
import Form._

trait WitchcraftBoard {
  val gameStates = Stack(WitchcraftGame())
  val gameHistory = Stack(gameStates.head)
  var ai: Option[WitchcraftAI] = None
  lazy val canvas = new Canvas()
  val spellPanel = new SpellPanel()
  var player = true
  def changeTPoints(tp: String): Unit
  canvas.mouseReleasedListener = Option({() => 
    changeTPoints(availTPoints.toString)
    if(spellPanel.aftermath) spellPanel.clearSpells()
  })
  canvas.undoListener = Option({() =>
    if(gameStates.length > 1) {
      gameStates.pop
      spellPanel.popSpell(player)
      changeTPoints(availTPoints.toString)
    }
  })

  def formType: Form ={
    if(canvas.circle) Circle
    else if(canvas.intersectionTotal > 0) Concave
    else Convex
  }

  def addSpellToList(s: Spell) {
    spellPanel.addSpell(s.headString, player)
  }
  def clearSpellList() {
    spellPanel.clearSpells()
  }

  def availTPoints =
    gameStates.head.turnPoints(player) -
      (if(canvas.circle) 1 else canvas.points.size)

  def clearStatesWith(g: WitchcraftGame) {
    gameStates.elems = g :: Nil
  }    

  def doCommit() {
    addForm()
    clearStatesWith(gameStates.head.commit(player))
    player = !player
    gameHistory push gameStates.head

    ai foreach { a => aiMove(a) }

    if(player) {
      val current = gameStates.head
      spellPanel.aftermath = true
      spellPanel.updateScore(current)
      clearStatesWith(current.getAftermath.get)
    }
  }

  def aiMove(ai: WitchcraftAI) {
    clearStatesWith(ai.getMove(gameHistory.tail, player))
    gameHistory push gameStates.head
    val aiBol = !player
    gameStates.head.spells(aiBol)
      .toStringList foreach {i => spellPanel.addSpell(i, aiBol)}
  }

  def addForm() {
    val current = gameStates.head
    val newOptionalState = formType match {
      case Circle => {
        current.compose(Circle, 0, 0, player)
      }
      case Convex => {
        current.compose(Convex, canvas.points.size, 0, player)
      }
      case Concave => {
        current.compose(Concave,
                        canvas.points.size,
                        canvas.intersectionTotal, player)
      }
    }
    if(newOptionalState != None) {
      gameStates push newOptionalState.get
      addSpellToList(gameStates.head.spells(player))
    }
    canvas.clearCanvas()
  }
}
