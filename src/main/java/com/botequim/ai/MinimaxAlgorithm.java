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

package org.botequim.ai;

import scala.collection.Seq;
import scala.collection.JavaConversions;

public abstract class MinimaxAlgorithm<Node> {
    abstract public boolean isTerminal(Node n);
    abstract public boolean victoryOrDraw(Node n, boolean player);
    abstract public double eval(Node n,  boolean player);
    abstract public Seq<Node> children(Node n, boolean player);
    public double max(Node node, boolean player, int depth) {
        if(depth <= 0)
            throw new UnsupportedOperationException("Cannot calc max with depth <= 0");
        //                System.out.println("Depth: " + depth);
        double maxEval = Double.NEGATIVE_INFINITY;
        for(Node c : JavaConversions.asJavaIterable(children(node, !player))) {
            //            System.out.println("Max Node: " + c);
            double newEval = min(c, player, depth -1);
            //              System.out.println("Eval: " + newEval);
            if(newEval > maxEval) {
                maxEval = newEval;
            }
        }
        return maxEval;
    }
    public double min(Node node, boolean player, int depth) {
        double minEval = Double.POSITIVE_INFINITY;
        double newEval;
        for(Node c : JavaConversions.asJavaIterable(children(node, player))) {
            //            System.out.println("Min Node: " + c);
            if(isTerminal(c)) {
                if(victoryOrDraw(c, player)) continue; // Max infinity so, never picked
                else return eval(c, player); // Defeat is the minimum possible value
            }
            if(depth == 0) newEval = eval(c, player);
            else newEval = max(c, player, depth);
            //            System.out.println("Eval: " + newEval);
            if(newEval < minEval) {
                minEval = newEval;
            }
        }
        return minEval;
    }
}
