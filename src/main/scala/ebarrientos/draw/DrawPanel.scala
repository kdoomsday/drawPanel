package ebarrientos.draw
import java.awt.Graphics2D
import java.awt.Color
import scala.swing.Panel
import scala.swing.Container
import ebarrientos.draw.shape.Shape
import ebarrientos.collection.Dequeue

class DrawPanel(parent: Container, xs: List[Shape]) extends Panel {
  def this(parent: Container) = this(parent, List[Shape]())
  
  // List of shapes managed by this class
  private var shapes = Dequeue(xs)

  // Add Shapes to the list
  def add(ss: List[Shape]): Unit = { Dequeue.drainInto(shapes, ss); remake }
  def add(ss: Shape*): Unit = add(ss.toList)

  // Move a Shape to the front
  def move2front(s: Shape) = if (shapes findL2R s) { shapes.swapWithRight; remake }
  
  // Move a Shape to the back
  def move2back(s: Shape) = if (shapes findR2L s) { shapes.swapWithLeft; remake }
  
  def moveForward(s: Shape) = if (shapes findL2R s) {shapes.swapWithNext; remake }
  
  def moveBack(s: Shape) = if (shapes findR2L s) { shapes.swapWithPrev; remake }

  // Look for topmost shape at a specific point
  def shapeAt(x: Double, y: Double): Option[Shape] = {
    for (s <- shapes.reverseIter) if (s isAt (x, y)) return Some(s)
    return None
  }
  
  def remove(s: Shape) = { shapes remove s; remake }
  
  private def remake = parent.repaint
  
  override def paint(g: Graphics2D) = for (s <- shapes) s.paint(g)
}