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
import scala.collection.mutable.{ ArrayBuffer, Builder, IndexedSeqLike, ListMap }

object SparseVec {

    implicit def canBuildFrom[A: Numeric: ClassManifest]: CanBuildFrom[SparseVec[A], A, SparseVec[A]] =
        new CanBuildFrom[SparseVec[A], A, SparseVec[A]] {
            def apply(): Builder[A, SparseVec[A]] = newBuilder
            def apply(from: SparseVec[A]): Builder[A, SparseVec[A]] = newBuilder
        }

    // Mandatory re-implementation of `newBuilder` in `IndexedSeq`
    def newBuilder[A: Numeric: ClassManifest]: Builder[A, SparseVec[A]] = new ArrayBuffer[A] mapResult fromSeq[A]

    /*::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::*/
    /** Build a numeric SparseVector from a numeric sequence.
     */
    def fromSeq[A: Numeric: ClassManifest](buf: Seq[A]): SparseVec[A] = 
    {
        val svec = new SparseVec(new ListMap())
        for (i <- 0 until buf.length) svec(i) = buf(i)
        svec
    }

    /*::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::*/
    /** Build a numeric SparseVector from a numeric sequence.
     */
    def apply[A: Numeric: ClassManifest](values: A*) = fromSeq(values)

    /*::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::*/
    def fill[A: Numeric: ClassManifest](dim: Int)(f: => A): SparseVec[A] = fromSeq(Array.fill(dim)(f).toSeq)

    /*::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::*/
    def one[A: Numeric: ClassManifest](j: Int, dim: Int): SparseVec[A] = {
        val nu = implicitly[Numeric[A]]
        import nu._
        val v = Array.fill[A](dim)(nu.zero)
        v(j) = nu.one
        fromSeq(v.toSeq)
    } // one
    
}

/*::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::*/
/** Numeric SparseVector (will be SparseVectorN eventually) The SparseVec class stores and
 *  operates on Numeric SparseVectors of various sizes and types. The element type may
 *  be any sub-type of Numeric.
 *  @author Michael Cotterell
 *  @param v the 1D array used to store SparseVector elements
 *  @param length the dimension/size of the SparseVector
 */
class SparseVec[A: Numeric: ClassManifest] protected (x: ListMap[Int, A])
    extends IndexedSeq[A]
    with IndexedSeqLike[A, SparseVec[A]]
    with ScalaTion
    with PartiallyOrdered[SparseVec[A]]
    with Ordering[SparseVec[A]]
    with Error {

    // import numeric evidence
    private val nu = implicitly[Numeric[A]]
    import nu._

    // Mandatory re-implementation of `newBuilder` in `IndexedSeq`
    override def newBuilder = SparseVec.newBuilder

    // Mandatory implementation of compare
    def compare(a: SparseVec[A], b: SparseVec[A]): Int = {
        for (i <- a.range) {
            if (a(i) > b(i))  1
            if (a(i) < b(i)) -1
        }
        0
    }
    
    override def reverse = 
    {
        val values = for (i <- range) yield this(i)
        val svec = SparseVec.fromSeq(values.reverse)
        svec
    }
    
    /*::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::*/
    /** Return the mean of the numeric SparseVector.
     */
    def mean(): Double = sum.toDouble / length
    
    /*::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::*/
    /**
     * Compare this SparseVector with SparseVector b.
     * @param b  the other SparseVector
     */
    def tryCompareTo [B >: SparseVec[A]] (b: B)
        (implicit view$1: (B) => PartiallyOrdered [B]): Option [Int] =
    {
        Some(compare(this, b.asInstanceOf[SparseVec[A]]))
    } // tryCompareTo
    
    /*::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::*/
    /** Return the i-th element of the SparseVector.
     */
    def apply(i: Int): A = 
    {
        try {
            x(i)
        } catch {
            case nsee: NoSuchElementException => nu.zero
        } // try
    } // apply

    /*::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::*/
    /** Return a new SparseVector containing the elements found in the indexed range.
     */
    def apply(r: Range): SparseVec[A] = {
        val values = for (i ← r) yield x(i)
        SparseVec.fromSeq(values)
    } // apply

    /*::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::*/
    /** Update the value of the i-th element of the SparseVector.
     */
    def update(i: Int, value: A) = x(i) = value

    /*::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::*/
    /** Return the length of the SparseVector.
     */
    def length = x.keySet.toIndexedSeq(x.keySet.size - 1)

    /*::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::*/
    /** Return the range of the SparseVector.
     */
    def range = 0 until length

    /*::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::*/
    /** Return the mean of the SparseVector.
     */
    def mean(implicit fu: Fractional[A]) = {
        import fu._
        sum / fromInt(length)
    }

    /*::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::*/
    /** Return a new SparseVector containing all the values of this SparseVector as Ints.
     */
    def toInt(): SparseVec[Int] = {
        val values = for (i ← range) yield x(i).toInt
        SparseVec.fromSeq(values)
    }

    /*::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::*/
    /** Return a new SparseVector containing all the values of this SparseVector as Longs.
     */
    def toLong(): SparseVec[Long] = {
        val values = for (i ← range) yield x(i).toLong
        SparseVec.fromSeq(values)
    }

    /*::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::*/
    /** Return a new SparseVector containing all the values of this SparseVector as Floats.
     */
    def toFloat(): SparseVec[Float] = {
        val values = for (i ← range) yield x(i).toFloat
        SparseVec.fromSeq(values)
    }

    /*::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::*/
    /** Return a new SparseVector containing all the values of this SparseVector as Doubles.
     */
    def toDouble(): SparseVec[Double] = {
        val values = for (i ← range) yield x(i).toDouble
        SparseVec.fromSeq(values)
    }

    /*::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::*/
    /** Return a new SparseVector containing the unary negative of each value
     *  in this SparseVector.
     */
    def unary_-() = map(-_)

    /*::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::*/
    /** Return a new SparseVector containing the values of this SparseVector added with
     *  a scaler.
     */
    def +(rhs: A) = map(_ + rhs)

    /*::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::*/
    /** Return a new SparseVector containing the values of this SparseVector added with
     *  the values of another SparseVector.
     */
    def +(rhs: SparseVec[A]) = {
        val values = for (i ← range) yield x(i) + rhs(i)
        SparseVec.fromSeq(values)
    }

    /*::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::*/
    /** Add a scaler to this SparseVector.
     */
    def +=(rhs: A) = for (i ← range) x(i) += rhs

    /*::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::*/
    /** Add the values of another SparseVector to this SparseVector.
     */
    def +=(rhs: SparseVec[A]) = for (i ← range) x(i) += rhs(i)

    /*::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::*/
    /** Return a new SparseVector containing the values of this SparseVector subtracted with
     *  a scaler.
     */
    def -(rhs: A) = map(_ - rhs)

    /*::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::*/
    /** Return a new SparseVector containing the values of this SparseVector subtracted with
     *  the values of another SparseVector.
     */
    def -(rhs: SparseVec[A]) = {
        val values = for (i ← range) yield x(i) - rhs(i)
        SparseVec.fromSeq(values)
    }

    /*::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::*/
    /** Subtract a scaler from this SparseVector.
     */
    def -=(rhs: A) = for (i ← range) x(i) -= rhs

    /*::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::*/
    /** Subtract the values of another SparseVector from this SparseVector.
     */
    def -=(rhs: SparseVec[A]) = for (i ← range) x(i) -= rhs(i)

    /*::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::*/
    /** Return a new SparseVector containing the values of this SparseVector multiplied by
     *  a scaler.
     */
    def *(rhs: A) = map(_ * rhs)

    /*::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::*/
    /** Return a new SparseVector containing the values of this SparseVector multiplied by
     *  the values of another SparseVector.
     */
    def *(rhs: SparseVec[A]) = {
        val values = for (i ← range) yield x(i) * rhs(i)
        SparseVec.fromSeq(values)
    }
    
    /*::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::*/
    /**
     * Multiply this 'row' SparseVector by matrix m.
     * @param m  the matrix to multiply by
     */
    def * (m: MatrixN [A]): SparseVec[A] = SparseVec.fromSeq((m.t * this.toVec).toSeq)

    /*::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::*/
    /** Multiply this SparseVector by a scaler.
     */
    def *=(rhs: A) = for (i ← range) x(i) *= rhs

    /*::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::*/
    /** Multiply this SparseVector by the values in another SparseVector.
     */
    def *=(rhs: SparseVec[A]) = for (i ← range) x(i) *= rhs(i)

    /*::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::*/
    /** Return a new SparseVector containing the values of this SparseVector divided by
     *  a scaler.
     */
    def /(rhs: A)(implicit fu: Fractional[A]) = {
        import fu._
        map(_ / rhs)
    }

    /*::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::*/
    /** Return a new SparseVector containing the values of this SparseVector divided by
     *  the values of another SparseVector.
     */
    def /(rhs: SparseVec[A])(implicit fu: Fractional[A]) = {
        import fu._
        val values = for (i ← range) yield x(i) / rhs(i)
        SparseVec.fromSeq(values)
    }

    /*::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::*/
    /** Divide this SparseVector by a scaler.
     */
    def /=(rhs: A)(implicit fu: Fractional[A]) = {
        import fu._
        for (i ← range) x(i) /= rhs
    }

    /*::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::*/
    /** Divide this SparseVector by the values in another SparseVector.
     */
    def /=(rhs: SparseVec[A])(implicit fu: Fractional[A]) = {
        import fu._
        for (i ← range) x(i) /= rhs(i)
    }

    /*::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::*/
    /** Take the minimum of this SparseVector with SparseVector b (element-by element).
     *  @param b  the other SparseVector
     */
    def min(rhs: SparseVec[A]): SparseVec[A] = {
        val values = for (i ← range) yield if (rhs(i) < x(i)) rhs(i) else x(i)
        SparseVec.fromSeq(values)
    } // min

    /*::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::*/
    /** Take the maximum of this SparseVector with SparseVector b (element-by element).
     *  @param b  the other SparseVector
     */
    def max(rhs: SparseVec[A]): SparseVec[A] = {
        val values = for (i ← range) yield if (rhs(i) > x(i)) rhs(i) else x(i)
        SparseVec.fromSeq(values)
    } // max

    /*::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::*/
    /** Sum the elements of this SparseVector skipping the ith element.
     *  @param i  the index of the element to skip
     */
    def sum_ne(i: Int): A = {
        val values = for (j ← range if j != i) yield x(j)
        values.reduceLeft(_ + _)
    } // sum_ne 

    /*::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::*/
    /** Cummulate the values of this SparseVector from left to right (e.g., create a
     *  cdf from a pmf).
     */
    def cummulate: SparseVec[A] = scanLeft(x(0))(_ + _)

    /*::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::*/
    /** Normalize this SparseVector so that it sums to one (e.g., for a probability
     *  SparseVector).
     */
    def normalize(implicit fu: Fractional[A]): SparseVec[A] = {
        import fu._
        this * (fu.one / sum)
    } // normalize
    
    /*::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::*/
    /**
     * Compute the dot product (or inner product) of this SparseVector with SparseVector b.
     * @param b  the other SparseVector
     */
    def dot(rhs: SparseVec[A]): A = {
        val v = SparseVec.fromSeq(for (i <- range) yield x(i) * rhs(i))
        v.sum
	} // dot
    
    def ⋅(rhs: SparseVec[A]) = dot(rhs)
    
    /*::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::*/
    /**
     * Compute the Euclidean norm (2-norm) of this SparseVector.
     */
    def norm(implicit fu: Fractional [A]): Double = math.sqrt (norm2.toDouble)
    
    /*::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::*/
    /**
     * Compute the Euclidean norm (2-norm) squared of this SparseVector.  If of the
     * appropriate type (see below), the sqrt may be used to compute the actual
     * norm.
     */
    def norm2(): A = this dot this
    
    /*::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::*/
    /**
     * Create a SparseVector of the form (0, ... 1, ... 0) where the 1 is at position j.
     * @param j     the position to place the 1
     * @param size  the size of the SparseVector (upper bound = size - 1)
     */
    def one (j: Int, size: Int = length) = SparseVec.one[A](j, size)
    
    /*::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::*/
    /**
     * Compute the outer product of this SparseVector with SparseVector b.
     * @param b  the other SparseVector
     */
    def outer (rhs: SparseVec[A]): MatrixN [A] =
    {
        val c = MatrixN [A] (length, rhs.length)
        for (i <- range; j <- rhs.range) c(i, j) = x(i) * rhs(j)
        c
    } // outer
    
    /*::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::*/
    def deep() = x.toArray.deep
    
    /*::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::*/
    /**
     * Find the argument minimum in this SparseVector starting from element k.
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
     * Find the argument minimum in this SparseVector starting from element k and
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
     * Find the argument maximum in this SparseVector starting from element k.
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
     * Find the argument maximum in this SparseVector starting from element k and
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
     * Find the index of the first negative element in this SparseVector starting from
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
     * Find the index of the first positive element in this SparseVector starting from
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
     * Check whether the other SparseVector is at least as long as this SparseVector.
     * @param b  the other SparseVector
     */
    def sameDimensions (b: SparseVec[A]): Boolean =
    {
        length <= b.length
    } // sameDimensions

    /*::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::*/
    def min: A = {
        val values = x.toSeq.sorted
        x(0)
    }

    /*::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::*/
    def max: A = {
        val values = x.toSeq.sorted
        x(length -1)
    }
    
    /*::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::*/
    /**
     * Set the values in this SparseVector to the values in array u.
     * @param u  the array of values to be assigned
     */
    def setAll (u: Array [A])
    {
        for (i <- range) x(i) = u(i)
    } // setAll
    
    /*::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::*/
    /**
     * Create a SparseVector consisting of values 0, 1, 2, ..., size - 1.
     * @param size  the size of the SparseVector (upper bound = size - 1)
     */
    def increasing (size: Int = length): SparseVec[A] =
    {
        val _0 = nu.zero; val _1 = nu.one
        val u  = SparseVec.fromSeq(Array.ofDim[A](length).toSeq)
        for (i <- range) u(i) = if (i == 0) _0 else nu.plus (u(i - 1), _1)
        u
    } // increasing

    /**
	 * Returns the SparseVector raised to the p power as a Double. Using
	 * Knuth's up-arrow notation
	 */
	def ↑(p: Int): SparseVec[Double] =
	{
		import scala.math._
		SparseVec.fromSeq(this map (e => pow(e.toDouble, p.toDouble)))
	}
	
	def toVec [A: Numeric: ClassManifest] =
	{
	    val values = for (i <- range) yield this(i)
	    Vec.fromSeq(values)
	}
    
    
}

