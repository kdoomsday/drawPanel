package ebarrientos.collection
import scala.collection.Iterable
import scala.collection.Iterator
import scala.annotation.tailrec

class Dequeue[A] extends Iterable[A] {
  /**
   * Data structure used to hold information about the Dequeue
   */
  private[Dequeue] case class Item[A](	var value: A,
		  								var next: Option[Item[A]],
		  								var prev: Option[Item[A]] )
  {
    def this(value: A) = this(value, None, None)
  }
  private[Dequeue] object Item {
    def apply[A](aval: A): Item[A] = return new Item(aval, None, None)
  }
  
  
  
  // Iterator for Dequeueues
  private[Dequeue] class Iter[A](var it: Option[Item[A]],
      					val left2right: Boolean) extends Iterator[A]
  {
    override def hasNext =	it match {
      case None => false
      case Some(x) => true
    }
    
    override def next: A =
      it match {
      	case Some(x) => {
      	  val res = x.value
      	  if (left2right) it = x.next else it = x.prev
      	  res
      	}
      	case None => throw new RuntimeException("No value!")
      }
  }
  
  
  
  // null is necessary for initialization, but we never return it
  var left: Item[A] = _	// Leftmost element of the Dequeueue
  var right: Item[A] = _ // Rightmost element
  private var len = 0
  
  private var elem = left	// Current element
  
  
  override def size = len	// Get size of Dequeueue
  
  
  // Obtain a left to right iterator of this instance
  override def iterator =
    if (left == null) new Iter(None, true) else new Iter(Some(left), true)
  
  // Obtain a right to left iterator of this instance
  def reverseIter =
    if (left == null) new Iter(None, false) else new Iter(Some(right), false)
  
  
  // Append elements on the left of this Dequeueue
  def appendLeft(it: A) = {
    val value = Item(it)
    if (size == 0) {
      left = value
      right = left
      elem = left
    }
    else {
      value.next = Some(left)
      left.prev = Some(value)
      left = value
    }
    
    len += 1
    this
  }
  
  def +:(it: A) = appendLeft(it)
  
  
  // Append elements to the right of this Dequeue
  def appendRight(it: A) = {
    val value = Item(it)
    if (size == 0) {
      right = value
      left = right
      elem = left
    }
    else {
      value.prev = Some(right)
      right.next = Some(value)
      right = value
    }
    
    len += 1
    this
  }
  
  def :+(it: A) = appendRight(it)
  
  
  // Get Elements
  def getLeft: Option[A] = if (size > 0) Some(left.value) else None
  def l = getLeft
  def getRight: Option[A] = if (size > 0) Some(right.value) else None
  def r = getRight
  def getCurrent: Option[A] = if (size > 0) Some(elem.value) else None
  
  // Get or else elements
  def leftGetOrElse(default: A) = getLeft getOrElse default
  def loe(default: A) = leftGetOrElse(default)
  def rightGetOrElse(default: A) = getRight getOrElse default
  def roe(default: A) = rightGetOrElse(default)
  def getCurrentOrElse(default: A) = getCurrent getOrElse default
  
  
  // Optionally get items
  private def geItem(item: Item[A]): Option[Item[A]] = if (item == null) None else Some(item)
  
  
  // Operations related to current element
  private def getCurrentItem: Option[Item[A]] = if (size > 0) Some(elem) else None
  def goFirst = elem = left
  def goLast = elem = right
  private def go(x: =>Option[Item[A]]) = {
    if (size > 0) x match { case Some(a) => elem = a; case None => }
    elem
  }
  def goNext = go(elem.next)
  def goPrev = go(elem.prev)
  
  private def has(x: =>Option[Item[A]]) = if (size == 0) false else x match {
    case Some(_) => true
    case None => false
  }
  def hasNext = has(elem.next)
  def hasPrev = has(elem.prev)
  
  // Swapping elements
  private def swap(x: Option[Item[A]], y: Option[Item[A]]) = {
    (x, y) match {
      case (Some(xe), Some(ye)) => {
    	  val tmp = xe.value
    	  xe.value = ye.value
    	  ye.value = tmp
      }
      case _ =>
    }
  }
  
  private def swapHelp(h: Boolean, f: =>Option[Item[A]]): Unit = {
    if (h) swap(Some(elem), f)
  }
  
  def swapWithNext = swapHelp(hasNext, elem.next)
  def swapWithPrev = swapHelp(hasPrev, elem.prev)
  
  def swapWithLeft = swap(this.geItem(elem), this.geItem(left))
  def swapWithRight = swap(this.geItem(elem), this.geItem(right))
  
  
  /**
   * Move the current pointer left to right until a matching element is found. If it is, the
   * current pointer will point to it. If it's not, it will pont to the last element.
   * 
   * @return Wether the element was found or not
   */
  def findL2R(x: A): Boolean = {
    if (size == 0) return false
    goFirst
    if (elem.value == x) return true
    while (hasNext) {
      goNext
      if (elem.value == x) return true
    }
    
    false
  }
  
  
  /**
   * Move the current pointer right to left until a matching element is found. If it is, the
   * current pointer will point to it. If it's not, it will pont to the first element.
   * 
   * @return Wether the element was found or not
   */
  def findR2L(x: A): Boolean = {
    if (size == 0) return false
    goLast
    if (elem.value == x) return true
    while (hasPrev) {
      goPrev
      if (elem.value == x) return true
    }
    
    false
  }
  
  
  // Removal functions
  /**
   * Remove current element. After this is done, the current element points to the next Item on the
   * left, if it wasn't leftmost. If the current item was leftmost, the current Item is now the
   * leftmost Item. If there are no Items left on the dequeue after removal, current points nowhere.
   */
  def removeCurrent = {
    getCurrentItem match {
      case Some(Item(_, Some(n), Some(p))) =>
        n.prev = Some(p)
        p.next = Some(n)
        elem = p.asInstanceOf[Item[A]]
        len = len-1
        
      case Some(Item(_, None, Some(p))) =>
        p.next = None
        right = p.asInstanceOf[Item[A]]
        elem = p.asInstanceOf[Item[A]]
        len = len-1
        
      case Some(Item(_, Some(n), None)) =>
        n.prev = None
        left = n.asInstanceOf[Item[A]]
        elem = n.asInstanceOf[Item[A]]
        len = len-1
        
      case Some(Item(_, None, None)) =>
        left = null
	    right = null
	    elem = null
	    len = 0
	    
      case None =>
    }
  }
  
  def remove(x: A) = if (findL2R(x)) removeCurrent	// Find sets current in the appropriate place
  
  override def toString = "Dequeueue(" + this.mkString(", ") + ")"
}

object Dequeue {
  def apply[A](xs: List[A]) = {
    val deq = new Dequeue[A]
    drainInto(deq, xs)
    deq
  }
  
  def drainInto[T, A <: T](deq: Dequeue[T], xs: List[A]): Unit = xs match {
    case y::ys =>
      deq :+ y
      drainInto(deq, ys)
      
    case Nil =>
  }
}
