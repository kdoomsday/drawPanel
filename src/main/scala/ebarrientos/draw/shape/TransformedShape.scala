package ebarrientos.draw.shape
import ebarrientos.draw.shape.util.AffineTransform2d
import java.awt.Graphics2D

case class TransformedShape(s: Shape, t: AffineTransform2d = AffineTransform2d()) extends Shape {

  override def draw(g: Graphics2D) = {
    val foo = g.getTransform()
    g setTransform t
    s paint g
    g setTransform foo
  }
  
  override def at(p: Point) = TransformedShape(s at p, t)
  override def movedBy(x: Double, y: Double) = {
    val pp = t inv (x, y)
    // This is vector and not pos. Need to calculate
    super.movedBy(pp.x, pp.y)
  }
  override def isAt(p: Point) = s isAt (t inv p)
  override def controlPoint = s.controlPoint
}