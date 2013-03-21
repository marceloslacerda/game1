/*
 * Copyright 2013 Marcelo de Sena Lacerda
 *
 * This file is part of Botequim's Game 1.
 *
 * Game 1 is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * Game 1 is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with Game 1.  If not, see <http://www.gnu.org/licenses/>.
 */

package org.botequim.swing

import scala.swing._
import Swing._
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
