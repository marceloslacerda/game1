package com.botequim.witchcraft.ai;

import scala.collection.Seq;
import scala.collection.JavaConversions;

abstract class MinimaxAlgorithm<Node> {
    abstract public boolean isTerminal(Node n);
    abstract public boolean victoryOrDraw(Node n, boolean player);
    abstract public double eval(Node n,  boolean player);
    abstract public Seq<Node> children(Node n, boolean player);
    public Node max(Seq<Node> sx, boolean player, int depth) {
        if(depth == 0)
            throw new UnsupportedOperationException("Cannot calc max with depth 0");
        //        System.out.println("Depth: " + depth);
        Node maxNode = sx.head();
        double maxEval = Double.NEGATIVE_INFINITY;
        for(Node n : JavaConversions.asJavaIterable(sx)) {
            double newEval = eval(min(children(n, !player), player, depth -1), player);
            //  System.out.println("Node: " + n + " Eval: " + newEval);
            if(newEval > maxEval) {
                maxEval = newEval;
                maxNode = n;
            }
        }
        return maxNode;
    }
    public Node min(Seq<Node> sx, boolean player, int depth) {
        Node minNode = sx.head();
        double minEval = Double.POSITIVE_INFINITY;
        double newEval;
        for(Node n : JavaConversions.asJavaIterable(sx)) {
            if(isTerminal(n)) {
                if(victoryOrDraw(n, player)) continue; // Max infinity so, never picked
                else return n; // Defeat is the minimum possible value
            }
            if(depth == 0) newEval = eval(n, player);
            else newEval = eval(max(children(n, player), player, depth), player);
            if(newEval < minEval) {
                minEval = newEval;
                minNode = n;
            }
        }
        return minNode;
    }
}
