import scala.swing.SimpleSwingApplication
import scala.swing.MainFrame
import ebarrientos.draw.DrawPanel
import ebarrientos.draw.shape._
import ebarrientos.draw.shape.util.AffineTransform2d
import java.awt.Dimension
import java.awt.Color
import scala.swing.event.MouseDragged
import scala.swing.event.MousePressed
import scala.swing.event.MouseReleased
import scala.swing.event.MouseMoved
import scala.swing.event.KeyTyped

object DrawTest extends SimpleSwingApplication {
  var p: DrawPanel = _
  var last: Option[Shape] = None
  var lastLoc = Point(0,0)

  override def top = new MainFrame {
    title = "Draw Panel"
    
    p = new DrawPanel(this)
    
    val r1 = FilledRect(50, 50, 100, 70)
    val r2 = FilledRect(70, 30, 80, 100, Color.RED)
    
    
//    p add (Rectangle(10, 10, 20, 40), Point(15, 15), r1, r2)
    p add (FilledOval((100.0, 100.0), 100, 50, Color.BLUE))
    p add (Line(15, 20, 200, 90, Color.GREEN))
    p add (Line(100, 25, 100, 175), Line(25, 100, 175, 100))
    
    p add (ShapeGroup(List(FilledOval((200.0, 200.0), 50, 100, Color.GRAY), FilledRect(200, 200, 300, 220))))
    
    
//    p add (TransformedShape(FilledRect((0.0, 0.0), (50.0, 50.0)),
//        new AffineTransform2d() rotate scala.math.Pi/6 move (50, 50)))
    
    p add (TransformedShape(FilledRect((100.0, 100.0), (150.0, 150.0)),
        (new AffineTransform2d() scalex 2) rotate (scala.math.Pi/4)))
    
//    p add (TransformedShape(FilledRect((0.0, 0.0), (50.0, 50.0)),
//        new AffineTransform2d() move (100, 100)))
    
    contents = p
    
    size = new Dimension(400, 300)
    centerOnScreen()
    p requestFocus()
    
    listenTo(p.mouse.clicks, p.mouse.moves, p.keys)
    reactions += {
      case e: MousePressed => {
        lastLoc = e.point
        last = p.shapeAt(lastLoc.x, lastLoc.y)
      }
      case MouseDragged(_, newpoint, _) => last match {
        case Some(s) => {
          p.remove(s)
          val tmp = s movedBy(newpoint.x - lastLoc.x, newpoint.y - lastLoc.y)
          p add (tmp)
          last = Some(tmp)
          lastLoc = newpoint
        }
        case None =>
      }
      case e: KeyTyped => doKeys(e.char)
        
    }
  }
  
  private def doKeys(c: Char) = c match {
    case 'f' => last map ( p moveForward(_) )
    case 'b' => last map ( p moveBack(_) )
    case 'd' => last map ( p remove(_) )
    
    case _ =>
  }
  
  private def doTo[T](s: Option[Shape])(f: (Shape) => T) = s map (f(_))
}