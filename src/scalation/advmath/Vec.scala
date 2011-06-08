/* $Id$ */

/*::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::*/
/** @author  John Miller, Michael Cotterell
 *  @see     LICENSE (MIT style license file).
 */

package scalation
package advmath

import util.Error

import collection.GenSeq
import scala.collection.generic._
import scala.collection.mutable.{ ArrayBuffer, Builder, IndexedSeqLike }

object Vec {

    implicit def canBuildFrom[A: Numeric: ClassManifest]: CanBuildFrom[Vec[A], A, Vec[A]] =
        new CanBuildFrom[Vec[A], A, Vec[A]] {
            def apply(): Builder[A, Vec[A]] = newBuilder
            def apply(from: Vec[A]): Builder[A, Vec[A]] = newBuilder
        }

    // Mandatory re-implementation of `newBuilder` in `IndexedSeq`
    def newBuilder[A: Numeric: ClassManifest]: Builder[A, Vec[A]] = new ArrayBuffer[A] mapResult fromSeq[A]

    /*::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::*/
    /** Build a numeric vector from a numeric sequence.
     */
    def fromSeq[A: Numeric: ClassManifest](buf: Seq[A]): Vec[A] = new Vec(buf.toArray)

    /*::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::*/
    /** Build a numeric vector from a numeric sequence.
     */
    def apply[A: Numeric: ClassManifest](values: A*) = fromSeq(values)

    /*::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::*/
    /** Build a numeric vector of certain length.
     */
    def ofLength[A: Numeric: ClassManifest](length: Int) = new Vec(Array.ofDim(length))

    /*::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::*/
    def fill[A: Numeric: ClassManifest](dim: Int)(f: => A): Vec[A] = fromSeq(Array.fill(dim)(f).toSeq)

    /*::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::*/
    def one[A: Numeric: ClassManifest](j: Int, dim: Int): Vec[A] = {
        val nu = implicitly[Numeric[A]]
        import nu._
        val v = Array.fill[A](dim)(nu.zero)
        v(j) = nu.one
        new Vec[A](v)
    } // one
    
}

/*::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::*/
/** Numeric Vector (will be VectorN eventually) The Vec class stores and
 *  operates on Numeric Vectors of various sizes and types. The element type may
 *  be any sub-type of Numeric.
 *  @author Michael Cotterell
 *  @param v the 1D array used to store vector elements
 *  @param length the dimension/size of the vector
 */
class Vec[A: Numeric: ClassManifest] protected (x: Array[A])
    extends IndexedSeq[A]
    with IndexedSeqLike[A, Vec[A]]
    with ScalaTion
    with PartiallyOrdered[Vec[A]]
    with Ordering[Vec[A]]
    with Error {

    // import numeric evidence
    private val nu = implicitly[Numeric[A]]
    import nu._

    // Mandatory re-implementation of `newBuilder` in `IndexedSeq`
    override def newBuilder = Vec.newBuilder

    // Mandatory implementation of compare
    def compare(a: Vec[A], b: Vec[A]): Int = {
        for (i <- a.range) {
            if (a(i) > b(i))  1
            if (a(i) < b(i)) -1
        }
        0
    }
    
    override def reverse = Vec.fromSeq(x.toSeq.reverse)
    
    /*::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::*/
    /** Return the mean of the numeric vector.
     */
    def mean(): Double = sum.toDouble / length
    
    /*::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::*/
    /**
     * Compare this vector with vector b.
     * @param b  the other vector
     */
    def tryCompareTo [B >: Vec[A]] (b: B)
        (implicit view$1: (B) => PartiallyOrdered [B]): Option [Int] =
    {
        Some(compare(this, b.asInstanceOf[Vec[A]]))
    } // tryCompareTo
    
    /*::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::*/
    /** Return the i-th element of the vector.
     */
    def apply(i: Int): A = x(i)

    /*::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::*/
    /** Return a new vector containing the elements found in the indexed range.
     */
    def apply(r: Range): Vec[A] = {
        val values = for (i ← r) yield x(i)
        Vec.fromSeq(values)
    } // apply

    /*::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::*/
    /** Update the value of the i-th element of the vector.
     */
    def update(i: Int, value: A) = x(i) = value

    /*::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::*/
    /** Return the length of the vector.
     */
    def length = x.length

    /*::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::*/
    /** Return the range of the vector.
     */
    def range = 0 until length

    /*::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::*/
    /** Return the mean of the vector.
     */
    def mean(implicit fu: Fractional[A]) = {
        import fu._
        sum / fromInt(length)
    }

    /*::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::*/
    /** Return a new vector containing all the values of this vector as Ints.
     */
    def toInt(): Vec[Int] = {
        val values = for (i ← range) yield x(i).toInt
        Vec.fromSeq(values)
    }

    /*::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::*/
    /** Return a new vector containing all the values of this vector as Longs.
     */
    def toLong(): Vec[Long] = {
        val values = for (i ← range) yield x(i).toLong
        Vec.fromSeq(values)
    }

    /*::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::*/
    /** Return a new vector containing all the values of this vector as Floats.
     */
    def toFloat(): Vec[Float] = {
        val values = for (i ← range) yield x(i).toFloat
        Vec.fromSeq(values)
    }

    /*::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::*/
    /** Return a new vector containing all the values of this vector as Doubles.
     */
    def toDouble(): Vec[Double] = {
        val values = for (i ← range) yield x(i).toDouble
        Vec.fromSeq(values)
    }

    /*::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::*/
    /** Return a new vector containing the unary negative of each value
     *  in this vector.
     */
    def unary_-() = map(-_)

    /*::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::*/
    /** Return a new vector containing the values of this vector added with
     *  a scaler.
     */
    def +(rhs: A) = map(_ + rhs)

    /*::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::*/
    /** Return a new vector containing the values of this vector added with
     *  the values of another vector.
     */
    def +(rhs: Vec[A]) = {
        val values = for (i ← range) yield x(i) + rhs(i)
        Vec.fromSeq(values)
    }

    /*::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::*/
    /** Add a scaler to this vector.
     */
    def +=(rhs: A) = for (i ← range) x(i) += rhs

    /*::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::*/
    /** Add the values of another vector to this vector.
     */
    def +=(rhs: Vec[A]) = for (i ← range) x(i) += rhs(i)

    /*::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::*/
    /** Return a new vector containing the values of this vector subtracted with
     *  a scaler.
     */
    def -(rhs: A) = map(_ - rhs)

    /*::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::*/
    /** Return a new vector containing the values of this vector subtracted with
     *  the values of another vector.
     */
    def -(rhs: Vec[A]) = {
        val values = for (i ← range) yield x(i) - rhs(i)
        Vec.fromSeq(values)
    }

    /*::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::*/
    /** Subtract a scaler from this vector.
     */
    def -=(rhs: A) = for (i ← range) x(i) -= rhs

    /*::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::*/
    /** Subtract the values of another vector from this vector.
     */
    def -=(rhs: Vec[A]) = for (i ← range) x(i) -= rhs(i)

    /*::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::*/
    /** Return a new vector containing the values of this vector multiplied by
     *  a scaler.
     */
    def *(rhs: A) = map(_ * rhs)

    /*::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::*/
    /** Return a new vector containing the values of this vector multiplied by
     *  the values of another vector.
     */
    def *(rhs: Vec[A]) = {
        val values = for (i ← range) yield x(i) * rhs(i)
        Vec.fromSeq(values)
    }
    
    /*::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::*/
    /**
     * Multiply this 'row' vector by matrix m.
     * @param m  the matrix to multiply by
     */
    def * (m: MatrixN [A]): Vec[A] = m.t * this

    /*::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::*/
    /** Multiply this vector by a scaler.
     */
    def *=(rhs: A) = for (i ← range) x(i) *= rhs

    /*::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::*/
    /** Multiply this vector by the values in another vector.
     */
    def *=(rhs: Vec[A]) = for (i ← range) x(i) *= rhs(i)

    /*::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::*/
    /** Return a new vector containing the values of this vector divided by
     *  a scaler.
     */
    def /(rhs: A)(implicit fu: Fractional[A]) = {
        import fu._
        map(_ / rhs)
    }

    /*::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::*/
    /** Return a new vector containing the values of this vector divided by
     *  the values of another vector.
     */
    def /(rhs: Vec[A])(implicit fu: Fractional[A]) = {
        import fu._
        val values = for (i ← range) yield x(i) / rhs(i)
        Vec.fromSeq(values)
    }

    /*::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::*/
    /** Divide this vector by a scaler.
     */
    def /=(rhs: A)(implicit fu: Fractional[A]) = {
        import fu._
        for (i ← range) x(i) /= rhs
    }

    /*::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::*/
    /** Divide this vector by the values in another vector.
     */
    def /=(rhs: Vec[A])(implicit fu: Fractional[A]) = {
        import fu._
        for (i ← range) x(i) /= rhs(i)
    }

    /*::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::*/
    /** Take the minimum of this vector with vector b (element-by element).
     *  @param b  the other vector
     */
    def min(rhs: Vec[A]): Vec[A] = {
        val values = for (i ← range) yield if (rhs(i) < x(i)) rhs(i) else x(i)
        Vec.fromSeq(values)
    } // min

    /*::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::*/
    /** Take the maximum of this vector with vector b (element-by element).
     *  @param b  the other vector
     */
    def max(rhs: Vec[A]): Vec[A] = {
        val values = for (i ← range) yield if (rhs(i) > x(i)) rhs(i) else x(i)
        Vec.fromSeq(values)
    } // max

    /*::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::*/
    /** Sum the elements of this vector skipping the ith element.
     *  @param i  the index of the element to skip
     */
    def sum_ne(i: Int): A = {
        val values = for (j ← range if j != i) yield x(j)
        values.reduceLeft(_ + _)
    } // sum_ne 

    /*::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::*/
    /** Cummulate the values of this vector from left to right (e.g., create a
     *  cdf from a pmf).
     */
    def cummulate: Vec[A] = scanLeft(x(0))(_ + _)

    /*::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::*/
    /** Normalize this vector so that it sums to one (e.g., for a probability
     *  vector).
     */
    def normalize(implicit fu: Fractional[A]): Vec[A] = {
        import fu._
        this * (fu.one / sum)
    } // normalize
    
    /*::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::*/
    /**
     * Compute the dot product (or inner product) of this vector with vector b.
     * @param b  the other vector
     */
    def dot(rhs: Vec[A]): A = {
        val v = Vec.fromSeq(for (i <- range) yield x(i) * rhs(i))
        v.sum
	} // dot
    
    def ⋅(rhs: Vec[A]) = dot(rhs)
    
    /*::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::*/
    /**
     * Compute the Euclidean norm (2-norm) of this vector.
     */
    def norm(implicit fu: Fractional [A]): Double = math.sqrt (norm2.toDouble)
    
    /*::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::*/
    /**
     * Compute the Euclidean norm (2-norm) squared of this vector.  If of the
     * appropriate type (see below), the sqrt may be used to compute the actual
     * norm.
     */
    def norm2(): A = this dot this
    
    /*::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::*/
    /**
     * Create a vector of the form (0, ... 1, ... 0) where the 1 is at position j.
     * @param j     the position to place the 1
     * @param size  the size of the vector (upper bound = size - 1)
     */
    def one (j: Int, size: Int = length) = Vec.one[A](j, size)
    
    /*::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::*/
    /**
     * Compute the outer product of this vector with vector b.
     * @param b  the other vector
     */
    def outer (rhs: Vec[A]): MatrixN [A] =
    {
        val c = MatrixN [A] (length, rhs.length)
        for (i <- range; j <- rhs.range) c(i, j) = x(i) * rhs(j)
        c
    } // outer
    
    /*::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::*/
    def deep() = x.deep
    
    /*::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::*/
    /**
     * Find the argument minimum in this vector starting from element k.
     * @param k  the starting point for finding the min
     * @param l  skip this element
     */
    def argmin (k: Int = 0, l: Int = -1): Int =
    {
        var j = k
        for (i <- k + 1 until length if k != l && x(i) < x(j)) j = i
        j
    } // argmin

    /*::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::*/
    /**
     * Find the argument minimum in this vector starting from element k and
     * return it if it is negative, otherwise return -1.
     * @param k  the starting point for finding the max
     * @param l  skip this element
     */
    def argminNeg (k: Int = 0, l: Int = -1): Int =
    {
        val _0 = nu.zero
        val j = argmin (k, l)
        if (x(j) < _0) j else -1
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
        for (i <- k + 1 until length if k != l && x(i) > x(j)) j = i
        j
    } // argmax

    /*::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::*/
    /**
     * Find the argument maximum in this vector starting from element k and
     * return it if it is positive, otherwise return -1.
     * @param k  the starting point for finding the max
     * @param l  skip this element
     */
    def argmaxPos (k: Int = 0, l: Int = -1): Int =
    {
        val _0 = nu.zero
        val j = argmax (k, l)
        if (x(j) > _0) j else -1
    } // argmaxPos

    /*::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::*/
    /**
     * Find the index of the first negative element in this vector starting from
     * element k.
     * @param k  the starting point for finding first negative element
     */
    def firstNeg (k: Int = 0) : Int =
    {
        val _0 = nu.zero
        for (i <- k until length if x(i) < _0) return i
        -1
    } // firstNeg

    /*::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::*/
    /**
     * Find the index of the first positive element in this vector starting from
     * element k.
     * @param k  the starting point for finding first positive element
     */
    def firstPos (k: Int = 0): Int =
    {
        val _0 = nu.zero
        for (i <- k until length if x(i) > _0) return i
        -1
    } // firstPos

    /*::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::*/
    /**
     * Check whether the other vector is at least as long as this vector.
     * @param b  the other vector
     */
    def sameDimensions (b: Vec[A]): Boolean =
    {
        length <= b.length
    } // sameDimensions

    /*::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::*/
    def min: A = {
        val values = x.sorted
        x(0)
    }

    /*::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::*/
    def max: A = {
        val values = x.sorted
        x(length -1)
    }
    
    /*::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::*/
    /**
     * Set the values in this vector to the values in array u.
     * @param u  the array of values to be assigned
     */
    def setAll (u: Array [A])
    {
        for (i <- range) x(i) = u(i)
    } // setAll
    
    /*::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::*/
    /**
     * Create a vector consisting of values 0, 1, 2, ..., size - 1.
     * @param size  the size of the vector (upper bound = size - 1)
     */
    def increasing (size: Int = length): Vec[A] =
    {
        val _0 = nu.zero; val _1 = nu.one
        val u  = Vec.ofLength[A] (size)
        for (i <- range) u(i) = if (i == 0) _0 else nu.plus (u(i - 1), _1)
        u
    } // increasing

    /**
	 * Returns the vector raised to the p power as a Double. Using
	 * Knuth's up-arrow notation
	 */
	def ↑(p: Int): Vec[Double] =
	{
		import scala.math._
		Vec.fromSeq(this map (e => pow(e.toDouble, p.toDouble)))
	}
    
    
}

