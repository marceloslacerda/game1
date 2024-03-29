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

import scala.swing._
import Swing._
import javax.swing.JPanel
import com.botequim.witchcraft.rules.{Form, Spell, WitchcraftGame}
import java.awt.{Color}

object WitchcraftApp extends SimpleSwingApplication {
  val windowSize = new Dimension(700, 700)
  val playerNames = Map(true -> "A", false -> "B")
  val victoryLabel = new Label()
  val nextForm = new Button(Action("Next Figure")(board.addForm))
  val commitButton = new Button(Action("Commit")(board.doCommit))
  val availTPointsLabel = new Label("")
  val yourSpell = new Label()
  val enemySpell = new Label()
  def eBMaker = new Button(Action("Exit")(onExit))
  val exitButton = eBMaker
  val cSize = (windowSize.height * 0.70) toInt
  val board: WitchcraftBoard = new WitchcraftBoard {
    override lazy val canvas = new Canvas(cSize)
    def changeTPoints(tp: String) {
      availTPointsLabel.text = tp
    }  
  }

  def formReady() {
    nextForm.enabled = true
  }

  def boardCleared() {
    nextForm.enabled = false    
  }

  def onExit() {
    top.close()
    System.exit(0);
  }

  def actionPanel = {
    val jpanel = new JPanel()
    jpanel.add(board.canvas.panel)
    new BoxPanel(Orientation.Vertical) {
      contents += Component.wrap(jpanel)
      contents += board.spellPanel
    }

  }
  
  def playTwo(){
    top.contents = gamePanel
  }

  def playSingle(){
    top.contents = gamePanel
    board.ai = Option(com.botequim.witchcraft.ai.DumbAI)
  }

  def gamePanel = new BorderPanel {
      import BorderPanel.Position._
      add(controlsPanel, East)
      add(actionPanel, Center)
  }

  def menu = new Menu(List(new Button(Action("Single player")(playSingle)), new Button(Action("Two Player")(playTwo)), eBMaker))

  val top = new MainFrame {
    title = "Witchcraft"
    maximumSize = windowSize
    minimumSize = windowSize
    preferredSize = windowSize
    resizable = false
    nextForm.enabled = false
    commitButton.enabled = true
    board.canvas.formReadyListener = Option(formReady)
    board.canvas.boardClearedListener = Option(boardCleared)
    availTPointsLabel.text = WitchcraftGame.pointsPTurnLimit.toString

    contents = menu.component
  }


  def controlsPanel = new BoxPanel(Orientation.Vertical) {
    val h = commitButton.preferredSize.height
    val w = 130
    val apPanel = availPointsPanel

    nextForm.preferredSize = (w, h)
    commitButton.preferredSize = (w, h)
    exitButton.preferredSize = (w, h)
    apPanel.preferredSize = (w, apPanel.preferredSize.height)
    nextForm.maximumSize = (w, h)
    commitButton.maximumSize = (w, h)
    exitButton.maximumSize = (w, h)
    apPanel.maximumSize = (w, apPanel.maximumSize.height)

    nextForm.peer.setAlignmentX(java.awt.Component.RIGHT_ALIGNMENT)
    commitButton.peer.setAlignmentX(java.awt.Component.RIGHT_ALIGNMENT)
    apPanel.peer.setAlignmentX(java.awt.Component.RIGHT_ALIGNMENT)
    exitButton.peer.setAlignmentX(java.awt.Component.RIGHT_ALIGNMENT)
    
    contents ++ List(
      RigidBox((0,h+11)),
      nextForm,
      RigidBox((0,h+11)),
      commitButton,
      RigidBox((0,h+11)),
      apPanel,
      VGlue,
      exitButton,
      RigidBox((0,h))
    )
  }

  def availPointsPanel = new BoxPanel(Orientation.Vertical) {
    availTPointsLabel.peer.setAlignmentX(java.awt.Component.CENTER_ALIGNMENT)
    border = Swing.TitledBorder(Swing.LineBorder(java.awt.Color.black), "Available points")
    contents ++ List(
      availTPointsLabel
    )
  }
}
