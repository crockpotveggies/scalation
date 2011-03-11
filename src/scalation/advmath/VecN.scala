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
 * Numeric Vector (will be VectorN eventually)
 * @author Michael Cotterell
 */
class VecN[A: Numeric: ClassManifest](val v: Array[A], val length: Int) 
	extends IndexedSeq[A] 
	with IndexedSeqLike[A, VecN[A]]
	with ScalaTion
	with Error
{
	private val nu = implicitly[Numeric[A]]
	private val range = 0 until length

	/**
	 * Returns a new VecN[A] of the given length
	 */
	def this(length: Int) = this(Array.ofDim[A](length), length)
	
	// Mandatory re-implementation of 'newBuilder' in 'IndexedSeq'
	override protected[this] def newBuilder: Builder[A, VecN[A]] = 
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
    def set(x: A) = for (i <- range) v(i) = x

    /*::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::*/
    /**
     * Set the values in this vector to the values in array u.
     * @param u  the array of values to be assigned
     */
    def setAll(u: Array [A]) = for (i <- range) v(i) = u(i)
    
    /*::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::*/
    /**
     * Add this vector and vector b.
     * @param b  the vector to add
     */
    def +(b: VecN[A]): VecN[A] =
    	VecN[A](for (i <- range) yield nu.plus (v(i), b.v(i)))

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
	 * Construct a new VecN from another VecN.
	 */
	def fromArray[A: Numeric: ClassManifest](array: Array[A]): VecN[A] = 
		new VecN(array, array.length)

	def apply[A: Numeric: ClassManifest](values: A*) = 
		fromSeq(values)
	
	def apply[A: Numeric: ClassManifest](vector: VecN[A]) = 
		fromVecN[A](vector)
	
	def apply[A: Numeric: ClassManifest](seq: IndexedSeq[A]) = 
		fromSeq(seq)
		
	def apply[A: Numeric: ClassManifest](range: Range) = 
		fromSeq(range.toSeq)
		
	def newBuilder[A: Numeric: ClassManifest]: Builder[A, VecN[A]] = 
		new ArrayBuffer mapResult fromSeq[A]
  
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
	def one[A: Numeric: ClassManifest](j: Int, length: Int): VecN[A] =
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
    	val v = Array.ofDim[A](length)
		for (i <- 0 until length) v(i) = nu.plus(nu.fromInt(i), nu.one)
		new VecN(v, length)
    }
    
}