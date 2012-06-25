package com.botequim.witchcraft

import scala.swing._
import javax.swing.{JPanel, BorderFactory}
import rules.{Form, Spell, WitchcraftGame}
import java.awt.{Color}

object WitchcraftApp extends SimpleSwingApplication with WitchcraftBoard {
  val windowSize = new Dimension(250, 200)
  val playerNames = Map(true -> "A", false -> "B")
  val victoryLabel = new Label()
  val scores = List(
    (new Label("Player " + playerNames(true) + ":"), new Label("")),
    (new Label("Player " + playerNames(false) + ":"), new Label("")))
  val nextForm = new Button(Action("Next Figure")(addForm))
  val commitButton = new Button(Action("Commit")(doCommit))
  val availTPointsLabel = new Label("")
  val spellList = new ListView[Spell]()
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
    maximumSize = windowSize
    minimumSize = windowSize
    nextForm.enabled = false
    commitButton.enabled = true
    canvas.formReadyListener = Option(formReady)
    canvas.boardClearedListener = Option(boardCleared)
    scores(0)._2.text = WitchcraftGame.initialPoints.toString
    scores(1)._2.text = WitchcraftGame.initialPoints.toString
    availTPointsLabel.text = WitchcraftGame.pointsPTurnLimit.toString
    contents = new BoxPanel(Orientation.Vertical) {
      contents ++
        List(scorePanel,
             nextForm,
             commitButton,
             actionPanel,
             availPointsPanel,
             spellList,
             exitButton
           )
    }
  }

  def availPointsPanel = new BoxPanel(Orientation.Horizontal) {
    contents ++ List(
      new Label("Available points:"),
      availTPointsLabel
    )
  }

/*
  def updateScore {
    val result = game.endGame
    if(result == 'DRAW) {
      victoryLabel.text = winningMessages(1)
      victoryLabel.visible = true
    }
    else if (result == 'VICTORY) {
      val winner = !game.currentPlayer
      victoryLabel.text = winningMessages(0).format(
        playerNames(winner))
      victoryLabel.visible = true
      val scoreBoard = (if(winner) scores(0)
                        else scores(1))
        scoreBoard._2.text = game.score(winner).toString
    }
  }*/

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
      border = Swing.EmptyBorder(0, 30, 0, 30)
    }
  }
/*
  def controlPanel: BoxPanel = {
    new BoxPanel(Orientation.Horizontal) {
      contents += new Button(Action("Restart") {
        game = game.cleanBoard.asInstanceOf[T]
        setBoard
        victoryLabel.visible = false
      })
      contents += depthField
      border = Swing.EmptyBorder(0, 30, 0, 30)
    }
  }

  def setBoard {
    for(i <- 0 until game.height;
        j <- 0 until game.width){
      buttons(i)(j).text = game(i)(j).toString
    }
  }*/
}
