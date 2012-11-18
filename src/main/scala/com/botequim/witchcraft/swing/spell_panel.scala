package com.botequim.witchcraft.swing

import scala.swing._
import javax.swing.JComponent
import com.botequim.witchcraft.rules.{Spell, WitchcraftGame}
import com.botequim.ZebraJList
import net.miginfocom.swing.MigLayout

class SpellPanel extends BorderPanel {
  var aftermath = false
  var pGame: WitchcraftGame = WitchcraftGame()
  val spellA = ListView.wrap[String](new ZebraJList())
  val aFields = (new Label("B attack"),    new Label("10")) ::
                (new Label("A relfect"),   new Label("50%")) ::
                (new Label("B parcial"),   new Label("5")) ::
                (new Label("A defense"),   new Label("4")) ::
                (new Label("B final"),     new Label("1")) ::
                (new Label("A remainder"), new Label("99")) ::
                (new Label("Charge"),      new Label("1")) :: Nil
  val spellB = ListView.wrap[String](new ZebraJList())
  val bFields = (new Label("A attack"),    new Label("10")) ::
                (new Label("B relfect"),   new Label("50%")) ::
                (new Label("A parcial"),   new Label("5")) ::
                (new Label("B defense"),   new Label("4")) ::
                (new Label("A final"),     new Label("1")) ::
                (new Label("B remainder"), new Label("99")) ::
                (new Label("Charge"),      new Label("1")) :: Nil

  val aPoints = (new Label("A points"),    new Label("100"))
  val bPoints = (new Label("B points"),    new Label("100"))

  def separators = new Label("-") ::
                   new Label(" ") ::
                   new Label("-") ::
                   new Label(" ") ::
                   new Label("-") ::
                   new Label(" ") ::
                   new Label("+") :: Nil

  def scorePanel(fieldList: List[(Label, Label)],
                 spellPanel: ListView[String])
   = new BoxPanel(Orientation.Horizontal) {
    val lPanel = new BoxPanel(Orientation.Vertical)
    fieldList foreach { lPanel.contents += _._1 }
    contents += lPanel
    val box = Swing.HGlue
    contents += box
    val sPanel = new BoxPanel(Orientation.Vertical)
    separators foreach { sPanel.contents += _ }
    contents += sPanel
    val vPanel = new BoxPanel(Orientation.Vertical)
    fieldList foreach { x: (Label, Label) =>
      x._2.peer.setAlignmentX(java.awt.Component.BOTTOM_ALIGNMENT)
      vPanel.contents += x._2
    }
    contents += vPanel
  }
  
  val panelA = scorePanel(aFields, spellA)  
  val panelB = scorePanel(bFields, spellB)
  val wrappedA = new BoxPanel(Orientation.Vertical) {
    contents += new ScrollPane { contents = spellA}
    contents += new BoxPanel(Orientation.Horizontal) {
      contents += aPoints._1
      contents += Swing.HGlue
      contents += aPoints._2
    }
    contents += panelA
  }
  val wrappedB = new BoxPanel(Orientation.Vertical) {
    contents += new ScrollPane { contents = spellB}
    contents += new BoxPanel(Orientation.Horizontal) {
      contents += bPoints._1
      contents += Swing.HGlue
      contents += bPoints._2
    }
    contents += panelB
  }

  import BorderPanel.Position._
  panelA.visible = false
  panelB.visible = false
  val dividedPanel = new javax.swing.JPanel(new MigLayout())
  dividedPanel.add(wrappedA.peer, "w 50%")
  dividedPanel.add(wrappedB.peer, "w 50%")

  add(Component.wrap(dividedPanel), Center)

  def addSpell(s: String, p: Boolean) {
    if(p) {
      spellA.listData :+= s
    }
    else {
      spellB.listData :+= s
    }
  }

  def clearSpells() {
    spellA.listData = Seq()
    spellB.listData = Seq()
    panelA.visible = false
    panelB.visible = false
    aftermath = false
    aPoints._2.text = pGame.getAftermathCalculus(true)("pFinal").toString
    bPoints._2.text = pGame.getAftermathCalculus(false)("pFinal").toString
  }

  def updateScore(game: WitchcraftGame) {
    implicit def doubleToStr(a: Double) = a.toString
    
    def setScoreFields(pPoints: (Label, Label),
                       sfList: List[(Label, Label)],
                       player: Boolean) {
      val result = game.getAftermathCalculus(player)
      pPoints._2.text = result("pPoints")
      sfList(0)._2.text = result("eAtk")
      sfList(1)._2.text = (1. - result("pRefl")) * 100 + "%"
      sfList(2)._2.text = result("ePartial")
      sfList(3)._2.text = result("pDef")
      sfList(4)._2.text = result("eFinal")
      sfList(5)._2.text = result("pFinal")
      sfList(6)._2.text = result("pCharge")
    }
    setScoreFields(aPoints, aFields, true)
    setScoreFields(bPoints, bFields, false)
    pGame = game
    panelA.visible = true
    panelB.visible = true
  }
}
