package ebarrientos.draw.shape
import java.awt.Graphics2D
import java.awt.Color

import scala.math.{min, max}

import scala.language.implicitConversions

/** Common behaviour for shapes */
trait Shape {
  def controlPoint: Point
  
  // Have the shape draw itself
  protected def draw(g: Graphics2D): Unit
  def paint(g: Graphics2D): Unit = draw(g)
  
  // Determine wether a shape is in a certain point
  def isAt(p: Point): Boolean
  final def isAt(x: Double, y: Double): Boolean = isAt(Point(x, y))
  
  // Create a shape that is the same as this one, but in a different location
  def at(p: Point): Shape
  def at(x: Double, y: Double): Shape = at(Point(x,y))
  
  // Create shape equivalent to this, moved by an amount in the x and y axis
  def movedBy(x: Double, y: Double) = at(controlPoint.x + x, controlPoint.y + y)
}


/********************************************************/


/** Shape that has a color associated with it */
trait ShapeWithSingleColor extends Shape {
  def c: Color
  
  override def paint(g: Graphics2D) = {
    g.setColor(c)
    draw(g)
  }
}
object ShapeWithSingleColor {
  val defaultColor = Color.BLACK
}


/********************************************************/


// Implementation of a Point
case class Point(x: Double, y: Double, col: Color = ShapeWithSingleColor.defaultColor)
extends ShapeWithSingleColor
{
  val xi = x.toInt
  val yi = y.toInt
  
  override val c = col
  override val controlPoint = this
  
  override def draw(g: Graphics2D) = g.drawLine(xi, yi, xi, yi)
  override def isAt(p: Point) = this == p
  
  override def at(p: Point) = p
  
  // Point math
  def unary_- = Point(-x, -y)
  def +(p: Point) = Point(x + p.x, y + p.y)
  def -(p: Point) = this + (-p)
  def *(a: Double) = Point(a*x, a*y)
  def abs = Point(scala.math.abs(x), scala.math.abs(y))
}
object Point {
  implicit def awtpoint2point(p: java.awt.Point): Point = Point(p.getX(), p.getY())
  implicit def point2awtpoint(p: Point): java.awt.Point = new java.awt.Point(p.xi, p.yi)
  implicit def tuple2point(t: Tuple2[Double, Double]) = new Point(t._1, t._2)
}


/********************************************************/


// Lines and their operations
case class Line(pone: Point, ptwo: Point, col: Color = ShapeWithSingleColor.defaultColor)
extends ShapeWithSingleColor
{
  def this(x1: Double, y1: Double, x2: Double, y2: Double, 
      col: Color) =
    this((x1, y1), (x2, y2), col)
    
  def this(x1: Double, y1: Double, x2: Double, y2: Double) =
    this((x1, y1), (x2, y2), ShapeWithSingleColor.defaultColor)
  
  override val c = col
  val p1 = pone
  val p2 = ptwo
  
  override def isAt(p: Point) = {
    if (p == p1 || p == p2) true
    else {
      // We check if it's inside the bounding rectangle because if we only check the slope it
      // might be on the same line, but outside of the current segment
      Rectangle(p1, p2).isAt(p) &&
      	( (p1.x == p2.x) ||	// Check for problem case when it is a vertical line
      		(scala.math.abs(slope - Line(p1, p).slope) < 0.005)	)  // Double math, we approximate
    }
    // Experimenting, 0.005 was small enough to be close to pixel precision
  }
  
  override def controlPoint = p1
  override def at(p: Point) = Line(p, p+(p2-p1), c)
  
  override def draw(g: Graphics2D) = g.drawLine(p1.xi, p1.yi, p2.xi, p2.yi)
  
  def slope: Double = (p2.y - p1.y) / (p2.x - p1.x)
}
object Line {
  def apply(pone: Point, ptwo: Point): Line = Line(pone, ptwo, ShapeWithSingleColor.defaultColor)
  
  def apply(x1: Double, y1: Double, x2: Double, y2: Double, col: Color): Line =
    Line((x1, y1), (x2, y2), col)
    
  def apply(x1: Int, y1: Int, x2: Int, y2: Int): Line =
    Line(x1, y1, x2, y2, ShapeWithSingleColor.defaultColor)
  
  implicit def pointTuple2Line(t: Tuple2[Point, Point]): Line = Line(t._1, t._2)
}


/********************************************************/


// Rectangle
case class Rectangle(topleft: Point, bottomright: Point,
    col: Color = ShapeWithSingleColor.defaultColor) extends ShapeWithSingleColor
{
  override val c = col
  
  def this(x1: Double, y1: Double, x2: Double, y2: Double, c: Color) =
    this((x1, y1), (x2, y2), c)
    
  def this(x1: Double, y1: Double, x2: Double, y2: Double) =
    this((x1, y1), (x2, y2), ShapeWithSingleColor.defaultColor)
    
  val width = bottomright.xi - topleft.xi
  val height = bottomright.yi - topleft.yi
  
  val tl = Point(min(topleft.x, bottomright.x), min(topleft.y, bottomright.y))
  val br = Point(max(topleft.x, bottomright.x), max(topleft.y, bottomright.y))
  
  override val controlPoint = tl
    
  override def draw(g: Graphics2D) = g.drawRect(topleft.xi, topleft.yi, width, height)
  
  override def isAt(p: Point) = 
    p.x >= tl.x && p.x <= br.x && p.y >= tl.y && p.y <= br.y
    
  override def at(p: Point) = Rectangle(p, Point(p.x + width, p.y + height), c)
}
object Rectangle {
  def apply(x1: Int, y1: Int, x2: Int, y2: Int, c: Color) =
    new Rectangle(x1, y1, x2, y2, c)
  def apply(x1: Int, y1: Int, x2: Int, y2: Int) = new Rectangle(x1, y1, x2, y2)
}


/********************************************************/


// Filled Rectangle
class FilledRect(t: Point, b: Point, col: Color = ShapeWithSingleColor.defaultColor)
extends Rectangle(t, b, col)
{
  override def draw(g: Graphics2D) = g.fillRect(tl.xi, tl.yi, width, height)
  override def at(p: Point) = FilledRect(super.at(p))
}
object FilledRect {
  def apply(t: Point, b: Point, col: Color = ShapeWithSingleColor.defaultColor) =
    new FilledRect(t, b, col)
  
  def apply(x1: Double, y1: Double, x2: Double, y2: Double, c: Color): FilledRect =
    FilledRect((x1, y1), (x2, y2), c)
    
  def apply(x1: Double, y1: Double, x2: Double, y2: Double): FilledRect =
    FilledRect((x1, y1), (x2, y2))
  
  def apply(r: Rectangle): FilledRect = FilledRect(r.tl, r.br, r.c)
}


/********************************************************/


// Ovals
case class Oval(center: Point, w: Double, h: Double, col: Color = ShapeWithSingleColor.defaultColor)
extends ShapeWithSingleColor
{
  override val c = col
  override val controlPoint = center
  
  // Rectangle that contains this oval
  protected val boundingRect =
    Rectangle((center.x - w/2, center.y - h/2), (center.x + w/2, center.y + h/2))
  
    
  override def draw(g: Graphics2D) =
    g.drawOval(boundingRect.tl.xi, boundingRect.tl.yi, w.toInt, h.toInt)
  
//  override def isAt(p: Point) = boundingRect.isAt(p)
  override def isAt(p: Point) = {
    val np = p - center
    val npx = np.x.toDouble
    val npy = np.y.toDouble
    val w2: Double = w*w / 4
    val h2: Double = h*h / 4
    (npx*npx / w2) + (npy*npy / h2) < 1
  }
  
  override def at(p: Point) = Oval(p, w, h, c)
}


/********************************************************/


class FilledOval(center: Point, w: Double, h: Double, c: Color = ShapeWithSingleColor.defaultColor)
extends Oval(center, w, h, c)
{
  override def draw(g: Graphics2D) =
    g.fillOval(boundingRect.tl.xi, boundingRect.tl.yi, w.toInt, h.toInt)
  override def at(p: Point) = FilledOval(super.at(p))
}
object FilledOval {
  def apply(center: Point, w: Double, h: Double, c: Color = ShapeWithSingleColor.defaultColor) =
    new FilledOval(center, w, h, c)
    
  def apply(o: Oval): FilledOval = FilledOval(o.center, o.w, o.h, o.c)
}


/********************************************************/


case class ShapeGroup(xs: List[Shape]) extends Shape {
  def this(xs: Shape*) = this(xs.toList)
  
  var shapes: List[Shape] = xs
  
  override def controlPoint = (0.0, 0.0)
  override def draw(g: Graphics2D): Unit = shapes foreach (_ paint g)
  override def isAt(p: Point): Boolean = shapes.foldLeft(false) ((x, s) => x || (s isAt p))
  
  override def at(p: Point) = ShapeGroup(shapes map (_ movedBy(p.x, p.y)))
}