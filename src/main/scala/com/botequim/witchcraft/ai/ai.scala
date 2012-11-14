package com.botequim.witchcraft.ai

import com.botequim.witchcraft.rules.WitchcraftGame

trait AI {
  type Node = WitchcraftGame
  def getMove(node: Node): Node
}
