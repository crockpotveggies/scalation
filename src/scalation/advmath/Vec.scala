/* $Id$ */

/*::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::*/
/**
 * @author  John Miller, Michael Cotterell
 * @see     LICENSE (MIT style license file).
 */

package scalation
package advmath

import collection.IndexedSeqLike
import collection.mutable.{Builder, ArrayBuffer}
import collection.generic._

import util.Error

/*::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::*/
/**
 * Numeric Vector (will be VectorN eventually) The Vec class stores and 
 * operates on Numeric Vectors of various sizes and types. The element type may 
 * be any sub-type of Numeric.
 * @author Michael Cotterell
 * @param v the 1D array used to store vector elements
 * @param length the dimension/size of the vector
 */
class Vec[A: Numeric: ClassManifest](val v: Array[A], val length: Int)
	extends IndexedSeq[A]
	with IndexedSeqLike[A, Vec[A]]
    with PartiallyOrdered[Vec[A]]
    with ScalaTion
	with Error
{
	
	private val nu = implicitly[Numeric[A]]
	private val cm = implicitly[ClassManifest[A]]
	
	import nu._
	import cm._
	
	/*::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::*/
	/**
	 * The integral range of vector's indices.
	 */
	private val range = 0 until length

	/*::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::*/
	/**
	 * Returns a new Vec[A] of the given length
	 */
	def this(length: Int) = this(Array.ofDim[A](length), length)
	
	/*::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::*/
	// Mandatory re-implementation of 'newBuilder' in 'IndexedSeq'
	override def newBuilder: Builder[A, Vec[A]] = 
		new ArrayBuffer[A] mapResult Vec.fromSeq[A]
	
	/*::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::*/
	/**
	 * Get the value at index i
	 * @param idx
	 * @return
	 */
	def apply(i: Int): A = 
	{
		if (i < 0 || length <= i) throw new IndexOutOfBoundsException
		v(i)
	} // apply
	
	/*::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::*/
	/**
	 * Update the value at index i
	 * @param i index
	 * @param value new value
	 */
	def update(i: Int, value: A) = v(i) = value
	
	/*::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::*/
	/**
	 * 
	 * @return
	 */
	def negate(vec: Vec[A]) = 
		Vec.fromSeq(for (i <- range) yield -vec(i))
	
	/*::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::*/
	/**
	 * Vector addition
	 * @return
	 */
	def plus(lhs: Vec[A], rhs: Vec[A]) = 
		Vec.fromSeq(for (i <- range) yield lhs(i) + rhs(i))
	
	/*::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::*/
	/**
	 * Scaler addition
	 * @return
	 */
	def plus(lhs: Vec[A], rhs: A) = 
		Vec.fromSeq(for (i <- range) yield lhs(i) + rhs)
	
	/*::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::*/
	/**
	 * Vector subtraction
	 * @return
	 */
	def minus(lhs: Vec[A], rhs: Vec[A]) = 
		Vec.fromSeq(for (i <- range) yield lhs(i) - rhs(i))
	
	/*::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::*/
	/**
	 * Scaler subtraction
	 * @return
	 */	
	def minus(lhs: Vec[A], rhs: A) = 
		Vec.fromSeq(for (i <- range) yield lhs(i) - rhs)
		
	/*::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::*/
	/**
	 * Vector multiplication
	 * @return
	 */
	def times(lhs: Vec[A], rhs: Vec[A]) = 
		Vec.fromSeq(for (i <- range) yield lhs(i) * rhs(i))
	
	/*::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::*/
	/**
	 * Scaler multiplication
	 * @return
	 */
	def times(lhs: Vec[A], rhs: A) = 
		Vec.fromSeq(for (i <- range) yield lhs(i) * rhs)
	
	/*::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::*/
	/**
	 * Vector division
	 * @return
	 */
	def div(lhs: Vec[A], rhs: Vec[A])(implicit fu: Fractional[A]): Vec[A] =
	{
		import fu._
		Vec.fromSeq(for (i <- range) yield lhs(i) / rhs(i))
	} // div
	
	/*::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::*/
	/**
	 * Scaler division
	 * @return
	 */
	def div(lhs: Vec[A], rhs:A)(implicit fu: Fractional[A]): Vec[A] =
	{
		import fu._
		Vec.fromSeq(for (i <- range) yield lhs(i) / rhs)
	} // div
	
	/*::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::*/
	/**
	 * Vector remainder
	 * @return
	 */
	def rem(lhs: Vec[A], rhs: Vec[A])(implicit iu: Integral[A]): Vec[A] =
	{
		import iu._
		Vec.fromSeq(for (i <- range) yield lhs(i) % rhs(i))
	} // rem
	
	/*::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::*/
	/**
	 * Scaler remainder
	 * @return
	 */
	def rem(lhs: Vec[A], rhs: A)(implicit iu: Integral[A]): Vec[A] =
	{
		import iu._
		Vec.fromSeq(for (i <- range) yield lhs(i) % rhs)
	} // rem
	
	/*::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::*/
	/**
	 * Vector signum
	 * @return
	 */
	def signum(vec: Vec[A]) =
		Vec.fromSeq(for (i <- range) yield vec(i).signum)
		
	/*::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::*/
	/**
	 * Vector exponentiation
	 * @return
	 */
	def pow(lhs: Vec[A], rhs: Vec[A]) = 
		Vec.fromSeq(for (i <- range) yield lhs(i) ↑ rhs(i))
		
	/*::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::*/
	/**
	 * Scaler exponentiation
	 * @return
	 */
	def pow(lhs: Vec[A], rhs: A) = 
		Vec.fromSeq(for (i <- range) yield lhs(i) ↑ rhs)
	
	/*::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::*/
	/**
	 * Vector absolute value
	 * @return
	 */
	def abs(vec: Vec[A]) = 
		Vec.fromSeq(for (i <- range) yield vec(i).abs())
		
	def +(rhs: Vec[A]) = plus(this, rhs)
	def +(rhs: A) = plus(this, rhs)
	def -(rhs: Vec[A]) = minus(this, rhs)
	def -(rhs: A) = minus(this, rhs)
	def *(rhs: Vec[A]) = times(this, rhs)
	def *(rhs: A) = times(this, rhs)
	def /(rhs: Vec[A])(implicit fu: Fractional[A]) = div(this, rhs)
	def /(rhs: A)(implicit fu: Fractional[A]) = div(this, rhs)
	def %(rhs: Vec[A])(implicit iu: Integral[A]) = rem(this, rhs)
	def %(rhs: A)(implicit iu: Integral[A]) = rem(this, rhs)
	def signum(): Vec[Int] = Vec.this.signum(this)
	def ↑(rhs: Vec[A]) = pow(this, rhs)
	def ↑(rhs: A) = pow(this, rhs)
	def unary_-() = negate(this)
	def abs(): Vec[A] = Vec.this.abs(this) 
		
	/*::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::*/
    /**
     * Convert into a Vec [Int].
     */
    def toInt(): Vec[Int] =
    	new Vec[Int](v.map((x: A) => nu.toInt(x)), length)
    
    /*::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::*/
    /**
     * Convert into a Vec [Long].
     */
    def toLong(): Vec[Long] =
    	new Vec[Long](v.map((x: A) => nu.toLong(x)), length)
    	
    /*::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::*/
    /**
     * Convert into a Vec [Double].
     */
    def toDouble(): Vec[Double] =
    	new Vec[Double](v.map((x: A) => nu.toDouble(x)), length)
    
    /*::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::*/
    /**
     * Convert into a Vec [Float].
     */
    def toFloat(): Vec[Float] =
    	new Vec[Float](v.map((x: A) => nu.toFloat(x)), length)
    	
    /*::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::*/
    /**
     * Set each value in this vector to x.
     * @param x  the value to be assigned
     */
    def set(x: A) = 
    {
		for (i <- range) v(i) = x
		this
    }

    /*::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::*/
    /**
     * Set the values in this vector to the values in array u.
     * @param u  the array of values to be assigned
     */
    def setAll(u: Array [A]) = 
    {
    	for (i <- range) v(i) = u(i)
    	this
    }
    
    /*::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::*/
    /**
     * Create a same-length Vec with all elements set to 0.
     */
    def zero() = Vec.ofLength(length).set(nu.zero) 
    
    /*::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::*/
	/**
	 * Create a same-length Vec with the j-th element set to 1 and all other
	 * elemens set to 0.
	 * @param j the location for the 1
	 */
    def oneAt(j: Int) = 
    {
    	val z = zero()
    	z(j) = nu.one
    	z
    } // oneAt
    
    /*::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::*/
    /**
     * Sum the elements of this vector skipping the ith element.
     * @param i  the index of the element to skip
     */
    def sum_ne (i: Int): A =
    	Vec.fromSeq(for (j <- range if j != i) yield v(j)).sum

    /*::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::*/
    /**
     * Cummulate the values of this vector from left to right (e.g., create a
     * cdf from a pmf).
     */
    def cummulate() = Vec.fromSeq(view.scanLeft(v(0))(_+_).slice(0, length))
    
    /*::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::*/
    /**
     * Normalize this vector so that it sums to one (e.g., for a probability
     * vector).
     */
    def normalize(implicit fu: Fractional[A]): Vec[A] = this / this.sum
    
    /*::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::*/
    /**
     * Compute the dot product (or inner product) of this vector with vector b.
     * @param b  the other vector
     */
    def dot (b: Vec [A]) = (this * b).sum
    
    /*::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::*/
    /**
     * Compare this vector with vector b.
     * @param b  the other vector
     */
    def tryCompareTo[B >: Vec[A]](b: B)
        (implicit view1: (B) => PartiallyOrdered[B]): Option [Int] =
    {
        val c = b.asInstanceOf[Vec[A]]
        
        val series = for (i <- range) yield (nu.compare(v(i), c(i)) > 0, nu.compare(v(i), c(i)) < 0)
        
        val (le, ge) = series reduceLeft {
        	(a, b) => {
        		val (a1, a2) = a
        		val (b1, b2) = b
        		(a1 && b1, a2 && b2)
        	}
        }
        
        (le, ge) match {
        	case (true, true) 	=> Some(0)
        	case (true, false) 	=> Some(-1)
        	case (false, true)  => Some(1)
        	case (false, false) => None
        }
    }
    	
}

/*::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::*/
/**
 * Numeric Vector
 * @author Michael Cotterell
 */
object Vec {

	/*::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::*/
	/**
	 * Construct a new Vec of a given length.
	 */
	def ofLength[A: Numeric: ClassManifest](length: Int) = 
		new Vec[A](length)
	
	/*::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::*/
	/**
	 * Construct a new Vec from a Numeric sequence.
	 */
	def fromSeq[A: Numeric: ClassManifest](buf: Seq[A]): Vec[A] = 
	{
		val v = Array.ofDim[A](buf.length)
		for (i <- 0 until buf.length) v(i) = buf(i)
		new Vec(v, buf.length)
	}
	
	/*::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::*/
	/**
	 * Construct a new Vec from another Vec.
	 */
	def fromVec[A: Numeric: ClassManifest](vector: Vec[A]): Vec[A] = 
	{
		val v = Array.ofDim[A](vector.length)
		for (i <- 0 until vector.length) v(i) = vector(i)
		new Vec(v, vector.length)
	}
	
	/*::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::*/
	/**
	 * Construct a new Vec from an array.
	 */
	def fromArray[A: Numeric: ClassManifest](array: Array[A]): Vec[A] = 
		new Vec(array, array.length)

	/*::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::*/
	/**
	 * Construct a new Vec from a Range.
	 */
	def fromRange[A: Numeric: ClassManifest](range: Range) = 
		fromSeq(range.toSeq)
	
	/*::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::*/
	/**
	 * Construct a new Vec from a sequence of values.
	 */
	def apply[A: Numeric: ClassManifest](values: A*) = 
		fromSeq(values)
		
	/*::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::*/
	/**
	 * 
	 * @return
	 */
	def newBuilder[A: Numeric: ClassManifest]: Builder[A, Vec[A]] = 
		new ArrayBuffer mapResult fromSeq[A]
  
	/*::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::*/
	/**
	 * 
	 * @return
	 */
    implicit def canBuildFrom[A: Numeric: ClassManifest]: CanBuildFrom[Vec[A], A, Vec[A]] = 
    	new CanBuildFrom[Vec[A], A, Vec[A]] {
    		def apply(): Builder[A, Vec[A]] = newBuilder
    		def apply(from: Vec[A]): Builder[A, Vec[A]] = newBuilder
    	}
	
	/*::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::*/
    /**
     * Create a vector of the form (0, ... 1, ... 0) where the 1 is at position j.
     * @param j     the position to place the 1
     * @param size  the size of the vector (upper bound = size - 1)
     */
	def oneAt[A: Numeric: ClassManifest](j: Int, length: Int): Vec[A] =
	{
		val nu = implicitly[Numeric[A]]
		val v = Array.ofDim[A](length)
		for (i <- 0 until length) v(i) = nu.zero
		v(j) = nu.one
		new Vec(v, length)
	} // oneAt
	
	/*::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::*/
    /**
     * Create a vector consisting of values 0, 1, 2, ..., size - 1.
     * @param size  the size of the vector (upper bound = size - 1)
     */
    def increasing[A: Numeric: ClassManifest](length: Int): Vec[A] =
    {
    	val nu = implicitly[Numeric[A]]
    	Vec.fromSeq(for (i <- 0 until length) yield nu.fromInt(i))
    } // increasing
    
} // Vec

object VecTest extends ScalaTion
{
	def main(args : Array[String]) 
	{
		
		for (l <- 1 to 4) {
	        println ("\n\tTest VectorN on integer vectors of dim " + l)
	        val a = Vec.ofLength[Int](l)
	        val b = Vec.ofLength[Int](l)
	        a.set (2)
	        b.set (3)
	        println ("a + b        = " + (a + b))
	        println ("a - b        = " + (a - b))
	        println ("a * b        = " + (a * b))
	        println ("a * 4        = " + (a * 4))
	        println ("a.min        = " + a.min)
	        println ("a.max        = " + a.max)
	        println ("a.sum        = " + a.sum)
	        println ("a.sum_ne     = " + a.sum_ne (0))
	        println ("a dot b      = " + (a dot b))
	        println ("a < b        = " + (a < b))
	        for (x <- a) print (" " + x)
	        println
	
	        println ("\n\tTest VectorN on real vectors of dim " + l)
	        val x = Vec.ofLength[Double](l)
	        val y = Vec.ofLength[Double](l)
	        x.set (2)
	        y.set (3)
	        println ("x + y        = " + (x + y))
	        println ("x - y        = " + (x - y))
	        println ("x * y        = " + (x * y))
	        println ("x * 4.0      = " + (x * 4.0))
	        println ("x.min        = " + x.min)
	        println ("x.max        = " + x.max)
	        println ("x.sum        = " + x.sum)
	        println ("x.sum_ne     = " + x.sum_ne (0))
	        println ("x dot y      = " + (x dot y))
	        println ("x < y        = " + (x < y))
	    } // for
	
	    val z = Vec(4, 2, 3, 1)
	    for (e <- z) println ("e = " + e) 
	    println ("Vec.increasing[Int](10) = " + Vec.increasing[Int](10))
	}
}