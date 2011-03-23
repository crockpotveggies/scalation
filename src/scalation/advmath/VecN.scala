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
import collection.generic.CanBuildFrom

import util.Error

/*::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::*/
/**
 * Numeric Vector (will be VectorN eventually) The VecN class stores and 
 * operates on Numeric Vectors of various sizes and types. The element type may 
 * be any sub-type of Numeric.
 * @author Michael Cotterell
 * @param v the 1D array used to store vector elements
 * @param length the dimension/size of the vector
 */
class VecN[A: Numeric: ClassManifest](val v: Array[A], val length: Int)
	extends IndexedSeq[A] 
	with IndexedSeqLike[A, VecN[A]]
    with PartiallyOrdered[VecN[A]]
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
	 * Returns a new VecN[A] of the given length
	 */
	def this(length: Int) = this(Array.ofDim[A](length), length)
	
	/*::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::*/
	// Mandatory re-implementation of 'newBuilder' in 'IndexedSeq'
	override def newBuilder: Builder[A, VecN[A]] = 
		new ArrayBuffer[A] mapResult VecN.fromSeq[A]
	
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
	}
	
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
	def negate(vec: VecN[A]) = 
		VecN.fromSeq(for (i <- range) yield -vec(i))
	
	/*::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::*/
	/**
	 * 
	 * @return
	 */
	def plus(lhs: VecN[A], rhs: VecN[A]) = 
		VecN.fromSeq(for (i <- range) yield lhs(i) + rhs(i))
		
	def plus(lhs: VecN[A], rhs: A) = 
		VecN.fromSeq(for (i <- range) yield lhs(i) + rhs)
	
	/*::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::*/
	/**
	 * 
	 * @return
	 */
	def minus(lhs: VecN[A], rhs: VecN[A]) = 
		VecN.fromSeq(for (i <- range) yield lhs(i) - rhs(i))
	
	def minus(lhs: VecN[A], rhs: A) = 
		VecN.fromSeq(for (i <- range) yield lhs(i) - rhs)
		
	/*::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::*/
	/**
	 * 
	 * @return
	 */
	def times(lhs: VecN[A], rhs: VecN[A]) = 
		VecN.fromSeq(for (i <- range) yield lhs(i) * rhs(i))
	
	def times(lhs: VecN[A], rhs: A) = 
		VecN.fromSeq(for (i <- range) yield lhs(i) * rhs)
		
	def div(lhs: VecN[A], rhs: VecN[A])(implicit fu: Fractional[A]): VecN[A] =
	{
		import fu._
		VecN.fromSeq(for (i <- range) yield lhs(i) / rhs(i))
	}
	
	def div(lhs: VecN[A], rhs:A)(implicit fu: Fractional[A]): VecN[A] =
	{
		import fu._
		VecN.fromSeq(for (i <- range) yield lhs(i) / rhs)
	}
	
	def rem(lhs: VecN[A], rhs: VecN[A])(implicit iu: Integral[A]): VecN[A] =
	{
		import iu._
		VecN.fromSeq(for (i <- range) yield lhs(i) % rhs(i))
	}
	
	def rem(lhs: VecN[A], rhs: A)(implicit iu: Integral[A]): VecN[A] =
	{
		import iu._
		VecN.fromSeq(for (i <- range) yield lhs(i) % rhs)
	}
	
	def signum(vec: VecN[A]) =
		VecN.fromSeq(for (i <- range) yield vec(i).signum)
	
	/*::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::*/
	/**
	 * 
	 * @return
	 */
	def abs(vec: VecN[A]) = 
		VecN.fromSeq(for (i <- range) yield vec(i).abs())
		
	def +(rhs: VecN[A]) = plus(this, rhs)
	def +(rhs: A) = plus(this, rhs)
	def -(rhs: VecN[A]) = minus(this, rhs)
	def -(rhs: A) = minus(this, rhs)
	def *(rhs: VecN[A]) = times(this, rhs)
	def *(rhs: A) = times(this, rhs)
	def /(rhs: VecN[A])(implicit fu: Fractional[A]) = div(this, rhs)
	def /(rhs: A)(implicit fu: Fractional[A]) = div(this, rhs)
	def %(rhs: VecN[A])(implicit iu: Integral[A]) = rem(this, rhs)
	def %(rhs: A)(implicit iu: Integral[A]) = rem(this, rhs)
	def signum(): VecN[Int] = VecN.this.signum(this)
	def unary_-() = negate(this)
	def abs(): VecN[A] = VecN.this.abs(this) 
		
	/*::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::*/
    /**
     * Convert into a VecN [Int].
     */
    def toInt(): VecN[Int] =
    	new VecN[Int](v.map((x: A) => nu.toInt(x)), length)
    
    /*::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::*/
    /**
     * Convert into a VecN [Long].
     */
    def toLong(): VecN[Long] =
    	new VecN[Long](v.map((x: A) => nu.toLong(x)), length)
    	
    /*::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::*/
    /**
     * Convert into a VecN [Double].
     */
    def toDouble(): VecN[Double] =
    	new VecN[Double](v.map((x: A) => nu.toDouble(x)), length)
    
    /*::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::*/
    /**
     * Convert into a VecN [Float].
     */
    def toFloat(): VecN[Float] =
    	new VecN[Float](v.map((x: A) => nu.toFloat(x)), length)
    	
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
     */
    def zero() = VecN.ofLength(length).set(nu.zero) 
    
    def oneAt(j: Int) = 
    {
    	val z = zero()
    	z(j) = nu.one
    	z
    }
    
    /*::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::*/
    /**
     * Sum the elements of this vector skipping the ith element.
     * @param i  the index of the element to skip
     */
    def sum_ne (i: Int): A =
    	VecN.fromSeq(for (j <- range if j != i) yield v(j)).sum

    /*::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::*/
    /**
     * Cummulate the values of this vector from left to right (e.g., create a
     * cdf from a pmf).
     */
    def cummulate() = VecN.fromSeq(view.scanLeft(v(0))(_+_).slice(0, length))
    
    /*::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::*/
    /**
     * Normalize this vector so that it sums to one (e.g., for a probability
     * vector).
     */
    def normalize(implicit fu: Fractional[A]): VecN[A] = this / this.sum
    
    /*::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::*/
    /**
     * Compute the dot product (or inner product) of this vector with vector b.
     * @param b  the other vector
     */
    def dot (b: VecN [A]) = (for (i <- range) yield v(i) * b(i)).sum
    
    /*::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::*/
    /**
     * Compare this vector with vector b.
     * @param b  the other vector
     */
    def tryCompareTo[B >: VecN[A]](b: B)
        (implicit view1: (B) => PartiallyOrdered[B]): Option [Int] =
    {
        val c = b.asInstanceOf[VecN[A]]
        
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
object VecN {

	/*::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::*/
	/**
	 * Construct a new VecN of a given length.
	 */
	def ofLength[A: Numeric: ClassManifest](length: Int) = 
		new VecN[A](length)
	
	/*::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::*/
	/**
	 * Construct a new VecN from a Numeric sequence.
	 */
	def fromSeq[A: Numeric: ClassManifest](buf: Seq[A]): VecN[A] = 
	{
		val v = Array.ofDim[A](buf.length)
		for (i <- 0 until buf.length) v(i) = buf(i)
		new VecN(v, buf.length)
	}
	
	/*::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::*/
	/**
	 * Construct a new VecN from another VecN.
	 */
	def fromVecN[A: Numeric: ClassManifest](vector: VecN[A]): VecN[A] = 
	{
		val v = Array.ofDim[A](vector.length)
		for (i <- 0 until vector.length) v(i) = vector(i)
		new VecN(v, vector.length)
	}
	
	/*::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::*/
	/**
	 * Construct a new VecN from an array.
	 */
	def fromArray[A: Numeric: ClassManifest](array: Array[A]): VecN[A] = 
		new VecN(array, array.length)

	/*::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::*/
	/**
	 * Construct a new VecN from a Range.
	 */
	def fromRange[A: Numeric: ClassManifest](range: Range) = 
		fromSeq(range.toSeq)
	
	/*::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::*/
	/**
	 * Construct a new VecN from a sequence of values.
	 */
	def apply[A: Numeric: ClassManifest](values: A*) = 
		fromSeq(values)
		
	/*::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::*/
	/**
	 * 
	 * @return
	 */
	def newBuilder[A: Numeric: ClassManifest]: Builder[A, VecN[A]] = 
		new ArrayBuffer mapResult fromSeq[A]
  
	/*::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::*/
	/**
	 * 
	 * @return
	 */
    implicit def canBuildFrom[A: Numeric: ClassManifest]: CanBuildFrom[VecN[A], A, VecN[A]] = 
    	new CanBuildFrom[VecN[A], A, VecN[A]] {
    		def apply(): Builder[A, VecN[A]] = newBuilder
    		def apply(from: VecN[A]): Builder[A, VecN[A]] = newBuilder
    	}
	
	/*::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::*/
    /**
     * Create a vector of the form (0, ... 1, ... 0) where the 1 is at position j.
     * @param j     the position to place the 1
     * @param size  the size of the vector (upper bound = size - 1)
     */
	def oneAt[A: Numeric: ClassManifest](j: Int, length: Int): VecN[A] =
	{
		val nu = implicitly[Numeric[A]]
		val v = Array.ofDim[A](length)
		for (i <- 0 until length) v(i) = nu.zero
		v(j) = nu.one
		new VecN(v, length)
	}
	
	/*::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::*/
    /**
     * Create a vector consisting of values 0, 1, 2, ..., size - 1.
     * @param size  the size of the vector (upper bound = size - 1)
     */
    def increasing[A: Numeric: ClassManifest](length: Int): VecN[A] =
    {
    	val nu = implicitly[Numeric[A]]
    	VecN.fromSeq(for (i <- 0 until length) yield nu.fromInt(i))
    }
    
}

object VecNTest extends ScalaTion
{
	def main(args : Array[String]) 
	{
		
		val vec = VecN(1, 1, 1, 1)
		
		println(vec.cummulate)
	}
}