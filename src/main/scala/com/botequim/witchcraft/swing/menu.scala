package com.botequim.witchcraft.swing

import scala.swing._
import Swing._
import com.botequim.witchcraft.rules.{Form, Spell, WitchcraftGame}
import java.awt.{Color}
import javax.swing.{JPanel, JLabel}
import net.miginfocom.swing.MigLayout

class Menu(buttons: List[Button]) {
  val panel = new JPanel(new MigLayout("fillx"))
  val buttonsPanel = new JPanel(new MigLayout("filly"))
  buttonsPanel.add(new JLabel(""), "growy,wrap")
  buttons.foreach { i: Button =>
    buttonsPanel.add(i.peer, "wrap 5, aligny bottom, align center, growx")
  }
  panel.add(new JLabel(""), "h 70%, wrap,align center")
  panel.add(buttonsPanel, "h 30%, align center")
  val component = Component.wrap(panel)
}
