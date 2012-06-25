package com.botequim.witchcraft

import scala.swing.Panel
import collection.immutable.Stack
import rules.{WitchcraftGame, Spell}
import rules.Form._

trait WitchcraftBoard {
  var gameStates = Stack(WitchcraftGame())
  val canvas = new Canvas()
  def changeScore(sa: String, sb: String): Unit
  def changeTPoints(tp: String): Unit
  def addSpellToList(s: Spell): Unit
  def clearSpellList(): Unit
  canvas.mouseReleasedListener = Option({() => changeTPoints(
    availTPoints.toString)})

  def formType: Form ={
    if(canvas.circle) Circle
    else if(canvas.intersectionTotal > 0) Concave
    else Convex
  }

  def availTPoints =
    gameStates.head.availableTurnPoints -
      (if(canvas.circle) 1 else canvas.points.size)

  def doCommit() {
    addForm()
    gameStates = Stack(gameStates.head.commit)
    val current = gameStates.head

    gameStates = Stack(current)
    
    changeScore(current.availableGamePoints(true).toString,
                current.availableGamePoints(false).toString)
    changeTPoints(current.availableTurnPoints.toString)
    clearSpellList()
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
