package com.botequim.witchcraft.ai;

import scala.collection.Seq;
import scala.collection.JavaConversions;

abstract class MinimaxAlgorithm<Node> {
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

abstract class AlphaBetaMinimax<Node> {
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
