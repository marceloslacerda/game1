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

package org.botequim.game1.ai

import org.botequim.game1.rules.{WitchcraftGame, Form}
import scala.util.Random

object FortuneAI extends WitchcraftAI {
  def getMove(sx: Seq[Node], player: Boolean): Node = {
    val grouped = children(sx.head, player).groupBy[Double] { n => n.getAftermathCalculus(true)("pFinal") }
    val minimal = grouped(grouped.keys.min)
    minimal maxBy { n => n.getAftermathCalculus(true)("pFinal")}
  }
}

object RandomAI extends WitchcraftAI {
  def getMove(sx: Seq[Node], player: Boolean): Node = {
    children(sx.head, player)(Random.nextInt(60))
  }
}
