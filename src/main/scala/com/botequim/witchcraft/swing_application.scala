package com.botequim.witchcraft

import scala.swing._
import java.awt.Dimension
import javax.swing.{JPanel, BorderFactory, Box, JComponent}
import rules.{Form, Spell, WitchcraftGame}
import java.awt.{Color}
import com.botequim.ZebraJList

object WitchcraftApp extends SimpleSwingApplication {
  val windowSize = new Dimension(700, 700)
  val playerNames = Map(true -> "A", false -> "B")
  val victoryLabel = new Label()
  val scores = List(
    (new Label("Player " + playerNames(true) + ":"), new Label("")),
    (new Label("Player " + playerNames(false) + ":"), new Label("")))
  val nextForm = new Button(Action("Next Figure")(board.addForm))
  val commitButton = new Button(Action("Commit")(board.doCommit))
  val availTPointsLabel = new Label("")
  val spellList = ListView.wrap[Spell](new ZebraJList())
  val yourSpell = new Label()
  val enemySpell = new Label()
  val exitButton = new Button(Action("Exit")(onExit))
  val cSize = (windowSize.height * 0.7) toInt
  val board: WitchcraftBoard = new WitchcraftBoard {
    override lazy val canvas = new Canvas(cSize)
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
      contents += scorePanel
      contents += Component.wrap(jpanel)
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
    scores(0)._2.text = WitchcraftGame.initialPoints.toString
    scores(1)._2.text = WitchcraftGame.initialPoints.toString
    availTPointsLabel.text = WitchcraftGame.pointsPTurnLimit.toString
    contents = new BorderPanel {
      import BorderPanel.Position._
      add(controlsPanel, East)
      add(actionPanel, Center)
      add(spellListPanel, South)
    }
  }

  def controlsPanel = new BoxPanel(Orientation.Vertical) {
    val h = commitButton.preferredSize.height
    val w = 130
    nextForm.preferredSize = new Dimension(w, h)
    commitButton.preferredSize = new Dimension(w, h)
    exitButton.preferredSize = new Dimension(w, h)
    nextForm.maximumSize = new Dimension(w, h)
    commitButton.maximumSize = new Dimension(w, h)
    exitButton.maximumSize = new Dimension(w, h)
    val box = Box.createRigidArea(new Dimension(0,100))
    contents ++ List(
      nextForm,
      commitButton,
      Component.wrap(box.asInstanceOf[JComponent]),
      exitButton
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
