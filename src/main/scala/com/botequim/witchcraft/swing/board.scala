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
    gameStates = gameStates push ai.get.getMove(gameStates.head)
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
