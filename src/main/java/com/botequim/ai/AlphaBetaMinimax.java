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

public abstract class AlphaBetaMinimax<Node> {
    abstract public boolean isTerminal(Node n);
    abstract public boolean victoryOrDraw(Node n, boolean player);
    abstract public double eval(Node n,  boolean player);
    abstract public Seq<Node> children(Node n, boolean player);
    public double max(Node node, boolean player, int depth, double alpha, double beta) {
        double a = alpha;
        double b = beta;
        //        System.out.println("Depth: " + depth);
        double maxEval = Double.NEGATIVE_INFINITY;
        double newEval;
        for(Node c : JavaConversions.asJavaIterable(children(node, !player))) {
            if(isTerminal(c)) {
                if(victoryOrDraw(c, player)) return eval(c, player); // Best case
                else continue; // worst case
            }
            if(depth == 0) newEval = eval(c, player);
            else newEval = min(c, player, depth - 1, a, b);
            if(newEval > maxEval) {
                maxEval = newEval;
            }
            if(newEval >= b) return newEval;
            if(newEval > a) a = newEval;
        }
        return maxEval;
    }
    public double min(Node node, boolean player, int depth, double alpha, double beta) {
        double a = alpha;
        double b = beta;
        double minEval = Double.POSITIVE_INFINITY;
        double newEval;
        for(Node c : JavaConversions.asJavaIterable(children(node, player))) {
            if(isTerminal(c)) {
                if(victoryOrDraw(c, player)) continue; // Infinity so, never picked
                else return eval(c, player); // Defeat is the minimum possible value
            }
            if(depth == 0) newEval = eval(c, player);
            else newEval = max(c, player, depth - 1, a, b);
            if(newEval < minEval) {
                minEval = newEval;
            }
            if(newEval <= a) return newEval;
            if(newEval <= b) b = newEval;
        }
        return minEval;
    }
}
