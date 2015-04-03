package ebarrientos.util

import scala.language.implicitConversions

case class Matrix(height: Int, width: Int) {
  val w = width
  val h = height

  private val values = Array.ofDim[Double](h, w)
  
  
  def apply(i: Int, j: Int) = values(i)(j)
  def apply(i: Int): Row = values(i)
  
  def update(i: Int, j: Int, v: Double) = values(i)(j) = v
  // TODO Missing row length check
  def update(i: Int, r: Array[Double]) = values(i) = r
  

  /**
   * Matrix multiplication
   */
  def mult(that: Matrix) = {
    val res = new Matrix(this.h, that.w)
    for (i <- 0 until h)
      for (j <- 0 until that.w)
        for (k <- 0 until w)
          res(i,j) += this(i,k) * that(k,j)

     res
  }
  
  def *(that: Matrix) = mult(that)
  
  
  /** Create a copy of the matrix */
  def copy: Matrix = {
    val r = new Matrix(height, width)
    for (i <- 0 until height) r(i) = this(i).copy
    r
  }
  
  /** Obtain inverse of the matrix */
  def invert = {
    val ini = copy
    val res = Matrix.identity(width)
    
    for (i <- 0 until width) {
      // i is the position in the diagonal we're processing
      
      if (ini(i, i) != 1) {	// If it's not already one, divide row to make (i,i) one
        val div = ini(i, i)
        for (k <- 0 until width) {
          ini(i, k) = ini(i, k) / div
          res(i, k) = res(i, k) / div
        }
      }
      
      // Now we need to make every (x, i) position 0 except (i,i)
      for (j <- 0 until height) {
        if (j != i && ini(j, i) != 0) {
          val mult = -ini(j, i)
          for (k <- 0 until width) {
            ini(j, k) = ini(j, k) + ini(i, k)*mult
            res(j, k) = res(j, k) + res(i, k)*mult
          }
        }
      }
      
    }
    
    res
  }
  

  /** toString impl */
  override def toString =  {
    var res = ""
  
    for (i <- 0 until h) {
      res += "|"
	  for (j <- 0 until w) {
	    res += values(i)(j)
	    if (j < w-1) res += " "
	  }
	  res += ("|%n" format())
  	}
    
    res
  }
}

object Matrix {
  def identity(n: Int): Matrix = {
    val res = new Matrix(n, n)
    (0 until n) foreach (x => res(x, x) = 1)
    res
  }
}


/** Class to represent a row in a matrix */
class Row(r: Array[Double]) {
  val v = r
  
  def width = v.length
  
  def unary_- = v map (-_)
  
  def + (that: Row): Row = {
    val s = for (i <- 0 until v.length) yield (v(i) + that.v(i))
    Row(s.toArray)
  }
  
  def - (that: Row) = this + (-that)
  
  def * (a: Double): Row = v map (_ * a)
  def / (a: Double) = this * (1/a)
  
  
  def copy: Row = {
    val r = Array.ofDim[Double](v.length)
    for (i <- 0 until width) r(i) = v(i)
    r
  }
  
  
  override def toString = {
    var res = "|"
    v foreach (res += _ + " ")
    res += "|"
    res
  }
}
object Row {
  def apply(r: Array[Double]) = new Row(r)
  implicit def arr2row(a: Array[Double]): Row = Row(a)
  implicit def row2arr(r: Row): Array[Double] = r.v
}