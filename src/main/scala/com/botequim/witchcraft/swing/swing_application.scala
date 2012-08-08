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
  val exitButton = new Button(Action("Exit")(onExit))
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
    contents = new BorderPanel {
      import BorderPanel.Position._
      add(controlsPanel, East)
      add(actionPanel, Center)
    }
  }

  def controlsPanel = new BoxPanel(Orientation.Vertical) {
    val h = commitButton.preferredSize.height
    val w = 130
    nextForm.preferredSize = (w, h)
    commitButton.preferredSize = (w, h)
    exitButton.preferredSize = (w, h)
    nextForm.maximumSize = (w, h)
    commitButton.maximumSize = (w, h)
    exitButton.maximumSize = (w, h)
    
    contents ++ List(
      RigidBox((0,h+11)),
      nextForm,
      commitButton,
      VGlue,
      exitButton,
      RigidBox((0,h))
    )
  }


  def availPointsPanel = new BoxPanel(Orientation.Horizontal) {
    contents ++ List(
      new Label("Available points:"),
      availTPointsLabel
    )
  }
}
