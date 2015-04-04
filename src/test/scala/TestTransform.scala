import org.scalatest._

import ebarrientos.draw.shape._
import ebarrientos.draw.shape.util.AffineTransform2d

class TestTransform extends FlatSpec with Matchers {
  private[this] val threshold = 0.0001
  
  "An AffineTransform2d" should "not change a point when it's the identity" in {
    val at = AffineTransform2d()  // should be identity by default
    val p = Point(42, 24)
    
    at.transform(p) should be (p)
    at.inv(p) should be (p)
  }
  
  
  it should "translate the origin by exactly as much as specified" in {
    val at = AffineTransform2d().move(42, 24)
    val p = Point(0, 0)
    
    val p1 = at.transform(p)
    p1 should be (Point(42, 24))
    
    at.inv(p1) should be (p)
  }
  
  
  it should "rotate the point around the origin" in {
    val at = AffineTransform2d().rotate(-scala.math.Pi / 2)
    val p = Point(10, 0)
    
    val p1 = at.transform(p)
    p1.x should be (0.0 +- threshold)
    p1.y should be (10.0 +- threshold)
  }
  
  it should "combine transforms correctly" in {
    val at = AffineTransform2d() rotate (-scala.math.Pi / 2) move (10, 0)
    val p = Point(0, 0)
    
    val p1 = at transform p
    p1.x should be (0.0 +- threshold)
    p1.y should be (10.0 +- threshold)
  }
  
}
