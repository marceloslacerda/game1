package com.botequim.witchcraft

import java.awt.{Graphics2D, Color, Rectangle}
import java.awt.geom.Line2D
import java.awt.event.{MouseListener, MouseEvent, MouseMotionListener}

class Canvas {
  type Point = (Int, Int)
  type Line = (Int, Int, Int, Int)
  val panel = new java.awt.Panel
  var lines: List[Line] = Nil
  var timeStamp = -1L
  var circle = false
  val circleSize = 30
  var points: List[Point] = Nil
  var colorsMap: Map[Point, Color] = Map()
  var formReadyListener: Option[() => Unit] = None
  var boardClearedListener: Option[() => Unit] = None
  var mouseReleasedListener: Option[() => Unit] = None
  var bufferedGraphics : Option[Graphics2D] = None
  setupPanel()

  def clearCanvas() {
    lines.foreach(eraseLine _)
    points.foreach(eraseCircle _)
    eraseBigCircle()

    lines = Nil
    points = Nil
    circle = false
    boardClearedListener foreach {_.apply()}
  }

  def intersectionTotal: Int ={
    var inter = 0
    val seqlines = lines.toSeq
    for{i <- 0 until seqlines.size
        j <- 0 until seqlines.size
        if(i < j)
        val (x1,y1,x2,y2) = seqlines(i)
        val (x3,y3,x4,y4) = seqlines(j)
        if Line2D.linesIntersect(x1, y1, x2, y2, x3, y3, x4, y4)
    } inter += 1
    inter - lines.size
  }

  def drawAnyCircle(x: Int, y: Int, w: Int, h: Int, c: Color) {
    drawOnCanvas { g =>
      g.setPaint(Color.white)
      g.fillOval(x, y, w, h)
      g.setPaint(c)
      g.drawOval(x, y, w, h)
    }
  }

  def drawCircleWithColor(p: Point, c: Color) {
    val (x, y) = p
    drawAnyCircle(x - circleSize / 2,
                  y - circleSize / 2,
                  circleSize,
                  circleSize, c)
  }

  def drawCircle(p: Point) {
    drawCircleWithColor(p, colorsMap(p))
  }

  def eraseCircle(p: Point) {
    drawCircleWithColor(p, Color.white)
  }

  def drawBigCircleWithColor(c: Color) {
    drawAnyCircle(circleSize,
                  circleSize,
                  circleSize * 8,
                  circleSize * 8, c)
  }

  def drawBigCircle() {
    drawBigCircleWithColor(Color.black)
  }

  def eraseBigCircle() {
    drawBigCircleWithColor(Color.white)
  }

  def drawCircles() {
    points foreach { p => drawCircle(p) }
  }

  def drawLine(l: Line) {
    val (x1, y1, x2, y2) = l
    drawOnCanvas { g =>
      g.setPaint(Color.black)
      g.drawLine(x1, y1, x2, y2)
    }
    drawCircle(x1, y1)
    drawCircle(x2, y2)
  }

  def drawLines() {
    lines.toSeq foreach {drawLine(_)}
  }

  def drawOnCanvas(f: (Graphics2D) => Unit) {
    bufferedGraphics match {
      case None =>
        val graphics: Graphics2D = panel.getGraphics().asInstanceOf[Graphics2D]
        f(graphics)
        panel.paint(graphics)
        panel.setVisible(true)
      case Some(g) =>
        f(g)
    }
  }

  def bufferedDraw(f: () => Unit) {
    bufferedGraphics = Option(panel.getGraphics().asInstanceOf[Graphics2D])
    f()
    panel.paint(bufferedGraphics.get)
    panel.setVisible(true)
    bufferedGraphics = None
  }

  def linesWithPoint(p: Point): Seq[Line] =
    for{
      l <- (lines.toSeq)
      val (x1, y1, x2, y2) = l
      if(x1 == p._1 && y1 == p._2
         || x2 == p._1 && y2 == p._2)
    } yield l

  def eraseLine(l : Line) {
    val (x1, y1, x2, y2) = l
    drawOnCanvas { g =>
      g.setPaint(Color.white)
      g.drawLine(x1, y1, x2, y2)
    }
  }

  def addPoint(x: Int, y: Int) {
    points ::= (x, y)
    colorsMap += (points.head -> Color.red)
    drawCircle(points.head)
  }

  def addLine(p1: Point, p2: Point) {
    lines ::= ((p1._1, p1._2, p2._1, p2._2))
    drawLine(lines.head)
  }

  def deletePoint(p: Point) {
    bufferedDraw { () =>
      linesWithPoint(points.head).foreach({ l =>
        eraseLine(l)
        lines = lines filterNot (_ == l)
      })
      eraseCircle(points.head)
      points = points filterNot (_ == p)
      drawLines()
      drawCircles()
    }
  }

  def clickEmptyArea(x: Int, y: Int) {
    val follow = points.headOption
    addPoint(x, y)
    follow foreach { addLine(_, points.head) }
  }

  def clickCircle(p: Point) {
    points.headOption foreach { f =>
      if(p != f
         && not2Connected(p)) {
           addLine(f, p)
           formReadyListener foreach {_.apply()} 
      }
    }
  }

  def setupPanel() {
    panel.setPreferredSize(new java.awt.Dimension(300, 300))
    panel.setBackground(Color.white)
    panel.addMouseListener(new MouseListener {
      def mouseClicked(e: MouseEvent) {
        e.getButton match {
          case MouseEvent.BUTTON1 => {
            if(circle || closesCycle) return
            points.find(containsPoint(_, e)) match {
              case None => clickEmptyArea(e.getX, e.getY)
              case Some(p) => clickCircle(p)
            }
          }
          case MouseEvent.BUTTON3 => {
            if(!points.isEmpty) {
              deletePoint(points.head)
            }
            else if(circle) {
              eraseBigCircle()
              circle = false
            }
            if(points.isEmpty)
              boardClearedListener foreach {_.apply()}
          }
          case _ =>
        }
      }
      def mouseEntered(e: MouseEvent) {}
      def mouseExited(e: MouseEvent) {}
      def mousePressed(e: MouseEvent) {
        if(circle || !points.isEmpty) return
        if(timeStamp < 0) { timeStamp = System.currentTimeMillis }
      }
      def mouseReleased(e: MouseEvent) {
        if(!(circle || !points.isEmpty)
           && System.currentTimeMillis - timeStamp >= 1000) {
          drawBigCircle()
          circle = true
          formReadyListener foreach {_.apply()} 
        }
        timeStamp  = -1
        mouseReleasedListener foreach {_.apply()}
      }
    })

  }
  
  def containsPoint(p: Point, e: MouseEvent): Boolean = {
    val (x, y) = p
    new Rectangle(x - circleSize / 2,
                  y - circleSize / 2,
                  circleSize, circleSize).contains(
      e.getX, e.getY)
  }

  /**
   * By the very assembly of this program
   * all we need to check in order do state whether
   * a set of points and lines form a closed poligon
   * written in the body of this function.
   */
  def closesCycle = (lines.size == points.length
                     && lines.size >= 3)

  /**
   * Returns true if the number of lines that
   * uses the given point is less than 2.
   * Points that take part in a poligonal figure can have
   * at most 2 lines.
   */
  def not2Connected(p: Point) = 
    linesWithPoint(p).length < 2
}
