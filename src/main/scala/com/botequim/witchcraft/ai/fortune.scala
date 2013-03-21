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

package com.botequim.witchcraft.ai

import com.botequim.witchcraft.rules.{WitchcraftGame, Form}
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
