package com.botequim.witchcraft.swing

import scala.swing.Panel
import collection.immutable.Stack
import com.botequim.witchcraft.rules.{WitchcraftGame, Spell, Form}
import Form._

trait WitchcraftBoard {
  var gameStates = Stack(WitchcraftGame())
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
    spellPanel.addSpell(s, gameStates.head.player)
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
    val current = gameStates.head

    gameStates = Stack(current)
    
    if(gameStates.head.player) {
      spellPanel.aftermath = true
      spellPanel.updateScore(current)
      gameStates = Stack(current.getAftermath)      
    }
  }

  def addForm() {
    val current = gameStates.head
    val newOptionalState = formType match {
      case Circle => {
        println(formType)
        current.compose(Circle, 0, 0)
      }
      case Convex => {
        println(formType, canvas.points.size, 0)
        current.compose(Convex, canvas.points.size, 0)
      }
      case Concave => {
        println(formType, canvas.points.size, canvas.intersectionTotal)
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
