package com.botequim.witchcraft

import scala.swing._
import javax.swing.{JPanel, BorderFactory}
import java.awt.{Canvas, Graphics2D, Color, Rectangle}
import java.awt.geom.Line2D
import java.awt.event.{MouseListener, MouseEvent, MouseMotionListener}
import rules.{WitchcraftGame, Form}
import rules.Form._
import collection.mutable.LinkedList
import collection.immutable.Stack

object WitchcraftApp extends SimpleSwingApplication {
  val windowSize = new Dimension(250, 200)
  val canvas = new java.awt.Panel
  val playerNames = Map(true -> "A", false -> "B")
  val circleSize = 30
  val victoryLabel = new Label()
  val scores = List(
    (new Label("Player " + playerNames(true) + ":"), new Label("")),
    (new Label("Player " + playerNames(false) + ":"), new Label("")))
  var points: LinkedList[(Int, Int, Color)] = LinkedList()
  var lines: Set[(Int, Int, Int, Int)] = Set()
  var follow : Option[(Int, Int, Color)] = None
  var timeStamp = -1L
  var circle = false
  val nextForm = new Button(Action("Next Figure") { addForm() })
  val commitButton = new Button(Action("Commit") { doCommit() })
  val availTPointsLabel = new Label("")
  var gameStates = Stack(WitchcraftGame())

  def top = new MainFrame {
    title = "Witchcraft"
    maximumSize = windowSize
    minimumSize = windowSize
    nextForm.enabled = false
    commitButton.enabled = true
    scores(0)._2.text = WitchcraftGame.initialPoints.toString
    scores(1)._2.text = WitchcraftGame.initialPoints.toString
    availTPointsLabel.text = WitchcraftGame.pointsPTurnLimit.toString
    contents = new BoxPanel(Orientation.Vertical) {
      contents ++
        List(scorePanel,
             nextForm,
             commitButton,
             actionPanel,
             availPointsPanel,
             new Button(Action("Exit") {
               top.close()
               System.exit(0);
             })
           )
    }
  }

  def availPointsPanel = new BoxPanel(Orientation.Vertical) {
    contents ++ List(
      new Label("Available points:"),
      availTPointsLabel
    )
  }

  def formType: Form ={
    if(circle) Circle
    else if(intersectionTotal > 0) Concave
    else Convex
  }

  def clearBoard() {
    lines.foreach(eraseLine _)
    points.foreach(eraseCircle _)
    eraseBigCircle()

    lines = Set()
    points = LinkedList()
    follow = None
    circle = false
  }

  def intersectionTotal: Int ={
    var inter = 0
    val seqlines = lines.toSeq
    for{i <- 0 until seqlines.size
        j <- 0 until seqlines.size
        if(i<j)
        val (x1,y1,x2,y2) = seqlines(i)
        val (x3,y3,x4,y4) = seqlines(j)
        if Line2D.linesIntersect(x1, y1, x2, y2, x3, y3, x4, y4)
    } inter += 1
    inter - lines.size
  }

  def addForm() {
    val current = gameStates.head
    val newOptionalState = formType match {
      case Circle => {
        println(formType)
        current.compose(Circle, 0, 0)
      }
      case Convex => {
        println(formType, points.size, intersectionTotal)
        current.compose(Convex,
                        points.size,
                        intersectionTotal)
      }
      case Concave => {
        println(formType, points.size, intersectionTotal)
        current.compose(Concave,
                        points.size,
                        0)
      }
    }
    if(newOptionalState != None)
      gameStates = gameStates push newOptionalState.get
    clearBoard()
    nextForm.enabled = false
  }

  def doCommit() {
    addForm()
    gameStates = Stack(gameStates.head.commit)
    val current = gameStates.head
    scores(0)._2.text = current.availableGamePoints(true).toString
    scores(1)._2.text = current.availableGamePoints(false).toString
    availTPointsLabel.text = current.availableTurnPoints.toString
    clearBoard()
  }

  def drawCircle(x: Int, y: Int, c: Color) {
    drawOnCanvas { g =>
      g.setPaint(Color.white)
      g.fillOval(x - circleSize / 2,
                 y - circleSize / 2,
                 circleSize, circleSize)
      g.setPaint(c)
      g.drawOval(x - circleSize / 2,
                 y - circleSize / 2,
                 circleSize, circleSize)
    }
  }

  def drawBigCircle() {
    drawOnCanvas { g =>
      g.setPaint(Color.white)
      g.fillOval(circleSize,
                 circleSize,
                 circleSize * 8,
                 circleSize * 8)
      g.setPaint(Color.black)
      g.drawOval(circleSize,
                 circleSize,
                 circleSize * 8,
                 circleSize * 8)
    }
  }

  def eraseBigCircle() {
    drawOnCanvas { g =>
      g.setPaint(Color.white)
      g.drawOval(circleSize,
                 circleSize,
                 circleSize * 8,
                 circleSize * 8)
    }
  }

  def drawCircles() {
    drawOnCanvas { g =>
      points.foreach { p =>
        g.setPaint(Color.white)
        g.fillOval(p._1 - circleSize / 2,
                   p._2 - circleSize / 2,
                   circleSize, circleSize)
        g.setPaint(p._3)
        g.drawOval(p._1 - circleSize / 2,
                   p._2 - circleSize / 2,
                   circleSize, circleSize)
      }
    }
  }

  def drawLine(x1: Int, y1: Int, x2: Int, y2: Int) {
    drawOnCanvas { g =>
      g.setPaint(Color.black)
      g.drawLine(x1, y1, x2, y2)
    }
  }

  def drawOnCanvas(f: (Graphics2D) => Unit) {
    val graphics: Graphics2D = canvas.getGraphics().asInstanceOf[Graphics2D]
    f(graphics)
    canvas.paint(graphics)
    canvas.setVisible(true)
  }

  def eraseCircle() {
    if(points.isEmpty) return
    eraseCircle(points.head)
  }

  def eraseCircle(p: (Int, Int, Color)) {
    drawOnCanvas { g =>
      val (x, y, c) = p
      g.setPaint(Color.white)
      g.drawOval(x - circleSize / 2,
                 y - circleSize / 2,
                 circleSize, circleSize)
    }
  }

  def linesWithPoint(p: (Int, Int, Color)): Seq[(Int, Int, Int, Int)] =
    for{
      l <- (lines.toSeq)
      val (x1, y1, x2, y2) = l
      if(x1 == p._1 && y1 == p._2
         || x2 == p._1 && y2 == p._2)
    } yield l

  def eraseLine(l : (Int, Int, Int, Int)) {
    drawOnCanvas { g =>
      val (x1, y1, x2, y2) = l
      g.setPaint(Color.white)
      g.drawLine(x1, y1, x2, y2)
    }
  }

  def actionPanel: Panel = {
    val jpanel = new JPanel()
    canvas.setPreferredSize(new java.awt.Dimension(300, 300))
    canvas.setBackground(Color.white)
    canvas.addMouseListener(new MouseListener {
      def mouseClicked(e: MouseEvent) {
        e.getButton match {
          case MouseEvent.BUTTON1 => {
            if(circle || closesCycle) return
            points.find(containsPoint(_, e)) match {
              case None => {
                points +:= (e.getX, e.getY, Color.red)
                drawCircle(e.getX, e.getY, Color.red)
                if(!follow.isEmpty
                   && linesWithPoint(follow.get).length < 2
                   && linesWithPoint(points.head).length < 2) {
                  val f = follow.get
                  val p = points.head
                  lines += ((p._1, p._2, f._1, f._2))
                  drawLine(p._1, p._2, f._1, f._2)
                  drawCircles()
                  nextForm.enabled = closesCycle
                }
                follow = Option(points.head)
              }
              case Some(p) => {
                follow match {
                  case None => {
                    follow = Option(p)
                    drawCircle(p._1, p._2, Color.red)
                    points(points.indexOf(p)) = (p._1, p._2, Color.red)
                  }
                  case Some(f) => {
                    if(isNotFollow(p._1, p._2)) {
                      if(isPoligonal(follow.get)
                         && isPoligonal(p)) {
                        lines += ((p._1, p._2, f._1, f._2))
                        drawLine(p._1, p._2, f._1, f._2)
                        drawCircles()
                        nextForm.enabled = closesCycle
                      }
                      follow = Option(p)
                    }
                  }
                }
              }
            }    
          }
          case MouseEvent.BUTTON3 => {
            nextForm.enabled = false
            if(!points.isEmpty) {
              linesWithPoint(points.head).foreach(l => {
                eraseLine(l)
                lines -= l
              })
              eraseCircle()
              points = points.tail
              follow = points.headOption
            }
            else if(circle) {
              eraseBigCircle()
              circle = false
            }
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
        updateTPoints()
        if(circle || !points.isEmpty) return
        if(timeStamp - System.currentTimeMillis + 1000 < 0) {
          drawBigCircle()
          circle = true
          nextForm.enabled = true
        }
        timeStamp  = -1
      }
    })

    def updateTPoints() {
      availTPointsLabel.text = availTPoints.toString
    }

    def availTPoints =
      gameStates.head.availableTurnPoints - (if(circle) 1
        else points.size)

    canvas.addMouseMotionListener(new MouseMotionListener() {
      def mouseDragged(e: MouseEvent) {
        points.find(containsPoint(_, e)) match {
          case Some(p) => {
            eraseCircle(p)
            drawCircle(e.getX, e.getY, p._3)
            points(points.indexOf(p)) = (e.getX, e.getY, p._3)
          }
          case None =>
        }
      }
      def mouseMoved(e: MouseEvent) {
        points.foreach(p => {
          val (x,y,c) = p
          if(containsPoint(p, e)) {
            if(isNotFollow(x, y)) {
              if(c != Color.blue){
                drawCircle(x, y, Color.blue)
                points(points.indexOf(p)) = (x, y, Color.blue)
              }
            }
            else if(c != Color.red) {
              drawCircle(x, y, Color.red)
              points(points.indexOf(p)) = (x, y, Color.red)
            }
          }
          else if(c != Color.pink && isNotFollow(x, y)) {
            drawCircle(x, y, Color.pink)
            points(points.indexOf(p)) = (x, y, Color.pink)
          }
        })
      }
    })

    jpanel.add(canvas)
    new BoxPanel(Orientation.Vertical) {
      contents += Component.wrap(jpanel)
    }
  }

  def isNotFollow(x: Int, y: Int):Boolean = follow match {
    case Some((x1,y1,_)) => x1 != x || y1 != y
    case None => false
  }

  def containsPoint(p: (Int, Int, Color), e: MouseEvent): Boolean = {
    val (x, y, c) = p
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
  def isPoligonal(p: (Int, Int, Color)) = 
    linesWithPoint(p).length < 2

/*
  def updateScore {
    val result = game.endGame
    if(result == 'DRAW) {
      victoryLabel.text = winningMessages(1)
      victoryLabel.visible = true
    }
    else if (result == 'VICTORY) {
      val winner = !game.currentPlayer
      victoryLabel.text = winningMessages(0).format(
        playerNames(winner))
      victoryLabel.visible = true
      val scoreBoard = (if(winner) scores(0)
                        else scores(1))
        scoreBoard._2.text = game.score(winner).toString
    }
  }*/

  def scorePanel: BoxPanel = {
    scores(0)._1 border = BorderFactory.createEmptyBorder(0, 0, 0, 10)
    scores(1)._1 border = BorderFactory.createEmptyBorder(0, 0, 0, 10)
    scores(0)._2.border = BorderFactory.createEmptyBorder(0, 0, 0, 20)
    scores(0)._2.foreground = Color.blue
    scores(1)._2.foreground = Color.green


    new BoxPanel(Orientation.Vertical) {
      contents += new BoxPanel(Orientation.Horizontal) {
        scores.foreach(t => {
          contents += t._1
          contents += t._2
        })
      }
      contents += victoryLabel
      border = Swing.EmptyBorder(0, 30, 0, 30)
    }
  }
/*
  def controlPanel: BoxPanel = {
    new BoxPanel(Orientation.Horizontal) {
      contents += new Button(Action("Restart") {
        game = game.cleanBoard.asInstanceOf[T]
        setBoard
        victoryLabel.visible = false
      })
      contents += depthField
      border = Swing.EmptyBorder(0, 30, 0, 30)
    }
  }

  def setBoard {
    for(i <- 0 until game.height;
        j <- 0 until game.width){
      buttons(i)(j).text = game(i)(j).toString
    }
  }*/
}
