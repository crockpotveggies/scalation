 
/*:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::*/
/**
 * @author  John Miller
 * @version 1.0
 * @date    Wed Aug 26 18:41:26 EDT 2009
 * @see     LICENSE (MIT style license file).
 */

package scalation.advmath

import scala.math._
import scala.Numeric._
import scalation.util.Error

/*:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::*/
/**
 * Convenience definitions for commonly used types of vectors.
 */
object Vectors
{
    type VectorI = VectorN [Int]
    type VectorL = VectorN [Long]
    type VectorF = VectorN [Float]
    type VectorD = VectorN [Double]
    type VectorC = VectorN [Complex]
    type ArrayI  = Array [Int]
    type ArrayL  = Array [Long]
    type ArrayF  = Array [Float]
    type ArrayD  = Array [Double]
    type ArrayC  = Array [Complex]

    /*::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::*/
    /**
     * Indicator function, returning 1 if i == j, 0 otherwise.
     * @param i  the first integer value (e.g., index)
     * @param j  the second integer value (e.g., index)
     */
    def ind (i: Int, j: Int): Int =
    {
        if (i == j) 1 else 0
    } // ind

} // Vectors object

/*:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::*/
/**
 * The VectorN class stores and operates on Numeric Vectors of various sizes
 * and types.  The element type may be any subtype of Numeric.
 * @param dim  the dimension/size of the vector
 * @param v    the 1D array used to store vector elements
 */
case class VectorN [T <% Ordered [T]: ClassManifest] (dim: Int,
                                            private var v: Array [T] = null)
     extends PartiallyOrdered [VectorN [T]] with Error
{
    {
        if (v == null) {
            v = new Array [T] (dim)
        } else if (dim != v.length) {
            flaw ("constructor", "dimension is wrong")
        } // if
    } // primary constructor

    /** Range for the storage array
     */
    private val range = 0 until dim

    /*::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::*/
    /**
     * Construct a vector from an array of values.
     * @param u  the array of values
     */
    def this (u: Array [T])
    {
        this (u.length, u)                         // invoke primary constructor
    } // constructor

    /*::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::*/
    /**
     * Construct a vector from repeated values.
     * @param u  the repeated values
     */
    def this (u: T*)
    {
        this (u.length)                            // invoke primary constructor
        for (i <- range) v(i) = u(i)
    } // constructor

    /*::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::*/
    /**
     * Construct a vector and assign values from vector u.
     * @param u  the other vector
     */
    def this (u: VectorN [T])
    {
        this (u.dim)                               // invoke primary constructor
        for (i <- range) v(i) = u(i)
    } // constructor

    /*::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::*/
    /**
     * Create a vector of the form (0, ... 1, ... 0) where the 1 is at position j.
     * @param j     the position to place the 1
     * @param size  the size of the vector (upper bound = size - 1)
     */
    def one (j: Int, size: Int = dim) (implicit nu: Numeric [T]): VectorN [T] =
    {
        val _0 = nu.zero; val _1 = nu.one
        val u  = VectorN [T] (size)
        for (i <- range) u(i) = if (i == j) _1 else _0
        u
    } // one

    /*::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::*/
    /**
     * Create a vector consisting of values 0, 1, 2, ..., size - 1.
     * @param size  the size of the vector (upper bound = size - 1)
     */
    def increasing (size: Int = dim) (implicit nu: Numeric [T]): VectorN [T] =
    {
        val _0 = nu.zero; val _1 = nu.one
        val u  = VectorN [T] (size)
        for (i <- range) u(i) = if (i == 0) _0 else nu.plus (u(i - 1), _1)
        u
    } // increasing

    /*::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::*/
    /**
     * Convert a VectorN [T] into a VectorN [Int].
     * @param u  the vector to convert an integer vector
     */
    def toInt (implicit nu: Numeric [T]): VectorN [Int] =
    {
        val u = VectorN [Int] (dim)
        for (i <- 0 until dim) u(i) = nu.toInt (v(i))
        u
    } // toDouble

    /*::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::*/
    /**
     * Convert a VectorN [T] into a VectorN [Double].
     * @param u  the vector to convert a double vector
     */
    def toDouble (implicit nu: Numeric [T]): VectorN [Double] =
    {
        val u = VectorN [Double] (dim)
        for (i <- 0 until dim) u(i) = nu.toDouble (v(i))
        u
    } // toDouble

    /*::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::*/
    /**
     * Get this vector's element at the i-th index position. 
     * @param i the index
     */
    def apply (i: Int): T =
    {
        v(i)
    } // apply

    /*::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::*/
    /**
     * Get this vector's entire array.
     */
    def apply (): Array [T] =
    {
        v
    } // apply

    /*::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::*/
    /**
     * Set this vector's element at the i-th index position. 
     * @param i  the index
     * @param x  the value to assign
     */
    def update (i: Int, x: T)
    {
        v(i) = x
    } // update

    /*::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::*/
    /**
     * Set each value in this vector to x.
     * @param x  the value to be assigned
     */
    def set (x: T)
    {
        for (i <- range) v(i) = x
    } // set

    /*::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::*/
    /**
     * Set the values in this vector to the values in array u.
     * @param u  the array of values to be assigned
     */
    def setAll (u: Array [T])
    {
        for (i <- range) v(i) = u(i)
    } // setAll

    /*::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::*/
    /**
     * Iterate over the vector element by element.
     * @param f  the function to apply
     */
    def foreach [U] (f: T => U)
    {
        var i = 0    
        while (i < dim) {
            f (v(i))
            i += 1
        } // while
    } // foreach

    /*::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::*/
    /**
     * Slice this vector from to end.
     * @param from  the start of the slice
     * @param end   the end of the slice
     */
    def slice (from: Int, end: Int): VectorN [T] =
    {
        VectorN [T] (end - from, v.slice (from, end))
    } // slice

    /*::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::*/
    /**
     * Concatenate this vector and vector b.
     * @param b  the vector to concatenated
     */
    def ++ (b: VectorN [T]): VectorN [T] =
    {
        val c = VectorN [T] (dim + b.dim)
        for (i <- c.range) c.v(i) = if (i < dim) v(i) else b.v(i - dim)
        c
    } // ++

    /*::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::*/
    /**
     * Add this vector and vector b.
     * @param b  the vector to add
     */
    def + (b: VectorN [T]) (implicit nu: Numeric [T]): VectorN [T] =
    {
        val c = VectorN [T] (dim)
        for (i <- range) c.v(i) = nu.plus (v(i), b.v(i))
        c
    } // +

    /*::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::*/
    /**
     * Add this vector and scalar s.
     * @param s  the scalar to add
     */
    def + (s: T) (implicit nu: Numeric [T]): VectorN [T] =
    {
        val c = VectorN [T] (dim)
        for (i <- range) c.v(i) = nu.plus (v(i), s)
        c
    } // +
 
    /*::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::*/
    /**
     * From this vector substract vector b.
     * @param b  the vector to subtract
     */
    def - (b: VectorN [T]) (implicit nu: Numeric [T]): VectorN [T] =
    {
        val c = VectorN [T] (dim)
        for (i <- range) c.v(i) = nu.minus (v(i), b.v(i))
        c
    } // -
 
    /*::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::*/
    /**
     * From this vector subtract scalar s.
     * @param s  the scalar to subtract
     */
    def - (s: T) (implicit nu: Numeric [T]): VectorN [T] =
    {
        val c = VectorN [T] (dim)
        for (i <- range) c.v(i) = nu.minus (v(i), s)
        c
    } // -
 
    /*::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::*/
    /**
     * Multiply this vector by scalar s.
     * @param s  the scalar to multiply by
     */
    def * (s: T) (implicit nu: Numeric [T]): VectorN [T] =
    {
        val c = VectorN [T] (dim)
        for (i <- range) c.v(i) = nu.times (v(i), s)
        c
    } // *

    /*::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::*/
    /**
     * Multiply this vector by vector b.
     * @param b  the vector to multiply by
     */
    def * (b: VectorN [T]) (implicit nu: Numeric [T]): VectorN [T] =
    {
        val c = VectorN [T] (dim)
        for (i <- range) c.v(i) = nu.times (v(i), b.v(i))
        c
    } // *

    /*::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::*/
    /**
     * Multiply this 'row' vector by matrix m.
     * @param m  the matrix to multiply by
     */
    def * (m: MatrixN [T]) (implicit nu: Numeric [T]): VectorN [T] =
    {
        m.t * this
    } // *

    /*::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::*/
    /**
     * Divide this vector by vector b (element-by-element).
     * @param b  the vector to divide by
     */
    def / (b: VectorN [T]) (implicit nu: Fractional [T]): VectorN [T] =
    {
        val c = VectorN [T] (dim)
        for (i <- range) c.v(i) = nu.div (v(i), b.v(i))
        c
    } // /

    /*::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::*/
    /**
     * Divide this vector by scalar s.
     * @param s  the scalar to divide by
     */
    def / (s: T) (implicit nu: Fractional [T]): VectorN [T] =
    {
        val c = VectorN [T] (dim)
        for (i <- range) c.v(i) = nu.div (v(i), s)
        c
    } // /

    /*::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::*/
    /**
     * Sum the elements of this vector.
     */
    def sum (implicit nu: Numeric [T]): T =
    {
        val _0 = nu.zero
        v.foldLeft (_0) (nu.plus (_, _))
    } // sum

    /*::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::*/
    /**
     * Sum the elements of this vector skipping the ith element.
     * @param i  the index of the element to skip
     */
    def sum_ne (i: Int) (implicit nu: Numeric [T]): T =
    {
        val _0 = nu.zero
        var s  = _0
        for (j <- range if j != i) s = nu.plus (s, v(j))
        s
    } // sum_ne

    /*::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::*/
    /**
     * Product of the elements of this vector.
     */
    def product (implicit nu: Numeric [T]): T =
    {
        val _1 = nu.one
        v.foldLeft (_1) (nu.times (_, _))
    } // product
    
    /*::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::*/
    /**
     * Cummulate the values of this vector from left to right (e.g., create a
     * cdf from a pmf).
     */
    def cummulate (implicit nu: Numeric [T]): VectorN [T] =
    {
        val b = VectorN [T] (dim)
        b.v(0) = v(0)
        for (i <- 1 until b.dim) b.v(i) = nu.plus (b.v(i - 1), v(i))
        b
    } // cummulate

    /*::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::*/
    /**
     * Normalize this vector so that it sums to one (e.g., for a probability
     * vector).
     */
    def normalize (implicit nu: Fractional [T]): VectorN [T] =
    {
        val _1 = nu.one
        this * (nu.div (_1, sum))
    } // normalize

    /*::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::*/
    /**
     * Compute the dot product (or inner product) of this vector with vector b.
     * @param b  the other vector
     */
    def dot (b: VectorN [T]) (implicit nu: Numeric [T]): T =
    {
        val _0   = nu.zero
        var x: T = _0
        for (i <- range) x = nu.plus (x, (nu.times (v(i), b.v(i))))
        x
    } // dot

    /*::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::*/
    /**
     * Compute the outer product of this vector with vector b.
     * @param b  the other vector
     */
    def outer (b: VectorN [T]) (implicit nu: Numeric [T]): MatrixN [T] =
    {
        val c = MatrixN [T] (dim, b.dim)
        for (i <- range; j <- b.range) c(i, j) = nu.times (v(i), b.v(j))
        c
    } // outer

    /*::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::*/
    /**
     * Compute the Euclidean norm (2-norm) squared of this vector.  If of the
     * appropriate type (see below), the sqrt may be used to compute the actual
     * norm.
     */
    def norm2 (implicit nu: Numeric [T]): T =
    {
        this dot this
    } // norm2

    /*::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::*/
    /**
     * Compute the Euclidean norm (2-norm) of this vector.
     */
    def norm (implicit nu: Fractional [T]): Double =
    {
        sqrt ((norm2 (nu)).asInstanceOf [Double])
    } // norm

    /*::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::*/
    /**
     * Take the minimum of this vector with vector b (element-by element).
     * @param b  the other vector
     */
    def min (b: VectorN [T]): VectorN [T] =
    {
        val c = VectorN [T] (dim)
        for (i <- range) c.v(i) = if (b.v(i) < v(i)) b.v(i) else v(i)
        c
    } // min

    /*::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::*/
    /**
     * Take the maximum of this vector with vector b (element-by element).
     * @param b  the other vector
     */
    def max (b: VectorN [T]): VectorN [T] =
    {
        val c = VectorN [T] (dim)
        for (i <- range) c.v(i) = if (b.v(i) > v(i)) b.v(i) else v(i)
        c
    } // max

    /*::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::*/
    /**
     * Find the minimum in this vector.
     */
    def min: T =
    {
        var x = v(0)
        for (i <- 1 until dim if v(i) < x) x = v(i)
        x
    } // min

    /*::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::*/
    /**
     * Find the minimum in this vector starting from element k.
     * @param k  the starting point for finding the min
     */
    def min (k: Int): T =
    {
        var x = v(k)
        for (i <- k + 1 until dim if v(i) < x) x = v(i)
        x
    } // min

    /*::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::*/
    /**
     * Find the minimum in this vector starting from element k.
     */
    def max: T =
    {
        var x = v(0)
        for (i <- 1 until dim if v(i) > x) x = v(i)
        x
    } // max

    /*::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::*/
    /**
     * Find the maximum in this vector starting from element k.
     * @param k  the starting point for finding the max
     */
    def max (k: Int): T =
    {
        var x = v(k)
        for (i <- k + 1 until dim if v(i) > x) x = v(i)
        x
    } // max

    /*::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::*/
    /**
     * Find the argument minimum in this vector starting from element k.
     * @param k  the starting point for finding the min
     * @param l  skip this element
     */
    def argmin (k: Int = 0, l: Int = -1): Int =
    {
        var j = k
        for (i <- k + 1 until dim if k != l && v(i) < v(j)) j = i
        j
    } // argmin

    /*::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::*/
    /**
     * Find the argument minimum in this vector starting from element k and
     * return it if it is negative, otherwise return -1.
     * @param k  the starting point for finding the max
     * @param l  skip this element
     */
    def argminNeg (k: Int = 0, l: Int = -1) (implicit nu: Numeric [T]): Int =
    {
        val _0 = nu.zero
        val j = argmin (k, l)
        if (v(j) < _0) j else -1
    } // argmaxPos

    /*::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::*/
    /**
     * Find the argument maximum in this vector starting from element k.
     * @param k  the starting point for finding the max
     * @param l  skip this element
     */
    def argmax (k: Int = 0, l: Int = -1): Int =
    {
        var j = k
        for (i <- k + 1 until dim if k != l && v(i) > v(j)) j = i
        j
    } // argmax

    /*::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::*/
    /**
     * Find the argument maximum in this vector starting from element k and
     * return it if it is positive, otherwise return -1.
     * @param k  the starting point for finding the max
     * @param l  skip this element
     */
    def argmaxPos (k: Int = 0, l: Int = -1) (implicit nu: Numeric [T]): Int =
    {
        val _0 = nu.zero
        val j = argmax (k, l)
        if (v(j) > _0) j else -1
    } // argmaxPos

    /*::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::*/
    /**
     * Find the index of the first negative element in this vector starting from
     * element k.
     * @param k  the starting point for finding first negative element
     */
    def firstNeg (k: Int = 0) (implicit nu: Numeric [T]): Int =
    {
        val _0 = nu.zero
        for (i <- k until dim if v(i) < _0) return i
        -1
    } // firstNeg

    /*::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::*/
    /**
     * Find the index of the first positive element in this vector starting from
     * element k.
     * @param k  the starting point for finding first positive element
     */
    def firstPos (k: Int = 0) (implicit nu: Numeric [T]): Int =
    {
        val _0 = nu.zero
        for (i <- k until dim if v(i) > _0) return i
        -1
    } // firstPos

    /*::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::*/
    /**
     * Check whether the other vector is at least as long as this vector.
     * @param b  the other vector
     */
    def sameDimensions (b: VectorN [T]): Boolean =
    {
        dim <= b.dim
    } // sameDimensions

    /*::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::*/
    /**
     * Check whether this vector is nonnegative (has no negative elements).
     */
    def isNonnegative (implicit nu: Numeric [T]): Boolean =
    {
        val _0 = nu.zero
        for (i <- range if v(i) < _0) return false
        true
    } // isNonnegative
 
    /*::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::*/
    /**
     * Compare this vector with vector b.
     * @param b  the other vector
     */
    def tryCompareTo [B >: VectorN [T]] (b: B)
        (implicit view$1: (B) => PartiallyOrdered [B]): Option [Int] =
    {
        var le = true
        var ge = true

        for (i <- range) {
            if (ge && (v(i) compare b(i)) < 0) ge = false
            else if (le && (v(i) compare b(i)) > 0) le = false
        } // for
        if (ge && le) Some (0) else if (le) Some (-1) else if (ge) Some (1) else None
    } // tryCompareTo

    /*::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::*/
    /**
     * Convert type B to type VectorN [T].
     */
    implicit def BtoVectorN [B >: VectorN [T]] (b: B): VectorN [T] =
    {
       b.asInstanceOf [VectorN [T]]
    } // BtoVectorN
 
    /*::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::*/
    /**
     * Convert this vector to a string.
     */
    override def toString: String =
    {
        "VectorN" + v.deep.toString.substring (5)
    } // toString
  
} // VectorN class

/*:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::*/
/**
 * The VectorNTest object tests the operations provided by VectorN.
 */
object VectorNTest extends Application
{
    for (l <- 1 to 4) {
        println ("\n\tTest VectorN on integer vectors of dim " + l)
        val a = VectorN [Int] (l)
        val b = VectorN [Int] (l)
        a.set (2)
        b.set (3)
        println ("a + b    = " + (a + b))
        println ("a - b    = " + (a - b))
        println ("a * b    = " + (a * b))
        println ("a * 4    = " + (a * 4))
        println ("a.min    = " + a.min)
        println ("a.max    = " + a.max)
        println ("a.sum    = " + a.sum)
        println ("a.sum_ne = " + a.sum_ne (0))
        println ("a dot b  = " + (a dot b))
        println ("a.norm2  = " + a.norm2)
        println ("a < b    = " + (a < b))
        for (x <- a) print (" " + x)
        println

        println ("\n\tTest VectorN on real vectors of dim " + l)
        val x = VectorN [Double] (l)
        val y = VectorN [Double] (l)
        x.set (2)
        y.set (3)
        println ("x + y    = " + (x + y))
        println ("x - y    = " + (x - y))
        println ("x * y    = " + (x * y))
        println ("x * 4.0  = " + (x * 4.0))
        println ("x.min    = " + x.min)
        println ("x.max    = " + x.max)
        println ("x.sum    = " + x.sum)
        println ("x.sum_ne = " + x.sum_ne (0))
        println ("x dot y  = " + (x dot y))
        println ("x.norm2  = " + x.norm2)
        println ("x.norm   = " + x.norm)
        println ("x < y    = " + (x < y))
    } // for

    val z = new VectorN [Int] (4, 2, 3, 1)
    for (e <- z) println ("e = " + e) 
    println ("z.increasing = " + z.increasing ().apply ().deep)

} // VectorNTest

