package com.botequim.witchcraft

import scala.swing._
import java.awt.Dimension
import javax.swing.{JPanel, BorderFactory}
import rules.{Form, Spell, WitchcraftGame}
import java.awt.{Color}
import com.botequim.ZebraJList

object WitchcraftApp extends SimpleSwingApplication with WitchcraftBoard {
  val windowSize = new Dimension(640, 450)
  val playerNames = Map(true -> "A", false -> "B")
  val victoryLabel = new Label()
  val scores = List(
    (new Label("Player " + playerNames(true) + ":"), new Label("")),
    (new Label("Player " + playerNames(false) + ":"), new Label("")))
  val nextForm = new Button(Action("Next Figure")(addForm))
  val commitButton = new Button(Action("Commit")(doCommit))
  val availTPointsLabel = new Label("")
  val spellList = ListView.wrap[Spell](new ZebraJList())
  val yourSpell = new Label()
  val enemySpell = new Label()
  val exitButton = new Button(Action("Exit")(onExit))

  def formReady() {
    nextForm.enabled = true
  }

  def boardCleared() {
    nextForm.enabled = false    
  }

  def changeScore(sa: String, sb: String) {
    scores(0)._2.text = sa
    scores(1)._2.text = sb
  }

  def changeTPoints(tp: String) {
    availTPointsLabel.text = tp
  }

  def addSpellToList(s: Spell) {
    spellList.listData +:= s
  }

  def clearSpellList() {
    spellList.listData = Seq()
  }

  def onExit() {
    top.close()
    System.exit(0);
  }

  def actionPanel = {
    val jpanel = new JPanel()
    jpanel.add(canvas.panel)
    new BoxPanel(Orientation.Vertical) {
      contents += Component.wrap(jpanel)
    }

  }

  def top = new MainFrame {
    title = "Witchcraft"
//    maximumSize = windowSize
    minimumSize = windowSize
    nextForm.enabled = false
    commitButton.enabled = true
    canvas.formReadyListener = Option(formReady)
    canvas.boardClearedListener = Option(boardCleared)
    scores(0)._2.text = WitchcraftGame.initialPoints.toString
    scores(1)._2.text = WitchcraftGame.initialPoints.toString
    availTPointsLabel.text = WitchcraftGame.pointsPTurnLimit.toString
    contents = new BorderPanel {
      import BorderPanel.Position._
      add(scorePanel, North)
      add(controlsPanel, East)
      add(actionPanel, Center)
      add(spellListPanel, West)
      add(exitButton, South)
    }
  }

  def controlsPanel = new BoxPanel(Orientation.Vertical) {
    val h = commitButton.preferredSize.height
    val w = 130
    nextForm.preferredSize = new Dimension(w, h)
    commitButton.preferredSize = new Dimension(w, h)
    nextForm.maximumSize = new Dimension(w, h)
    commitButton.maximumSize = new Dimension(w, h)
    contents ++ List(
      nextForm,
      commitButton
    )
  }


  def availPointsPanel = new BoxPanel(Orientation.Horizontal) {
    contents ++ List(
      new Label("Available points:"),
      availTPointsLabel
    )
  }

  def spellListPanel = new BoxPanel(Orientation.Vertical) {
    spellList.fixedCellHeight = 20;
    contents += new ScrollPane { contents = spellList }
    contents += yourSpell
    contents += enemySpell
  }

  def scorePanel: BoxPanel = {
    scores(0)._1 border = BorderFactory.createEmptyBorder(0, 0, 0, 10)
    scores(1)._1 border = BorderFactory.createEmptyBorder(0, 0, 0, 10)
    scores(0)._2.border = BorderFactory.createEmptyBorder(0, 0, 0, 20)
    scores(0)._2.foreground = Color.blue
    scores(1)._2.foreground = Color.green


    new BoxPanel(Orientation.Vertical) {
      contents += new BoxPanel(Orientation.Horizontal) {
        scores.foreach(t => {
          contents += t._1
          contents += t._2
        })
      }
      contents += victoryLabel
      contents += availPointsPanel
      border = Swing.EmptyBorder(0, 30, 0, 30)
    }
  }
}
