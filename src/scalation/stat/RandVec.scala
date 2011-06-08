package scalation
package stat

import advmath._

/** Companion object for RandVec class.
 */
object RandVec {
    def ofLength(length: Int) = new RandVec(length)
}

/** The RandVec class is a random numeric vector of a specific length. If values
 *  and probabilities are provided, then the mean statistic takes into
 *  consideration the probability distribution. If no probability distribution
 *  is provided, then RandVec assumes a uniform distribution of probabilities.
 */
class RandVec (length: Int, var v: VecD = null, var p: VecD = null) extends Vec[Double] (Array.ofDim[Double](length)) {

    // random distribution
    private val r = new Random(0)
  
    {
        // if no probabilities are given, assume uniform distribution of probabilities
        if (p == null) p = Vec.fromSeq(Array.fill(length)(1.0/length).toSeq)
     
        // if no array is given, then provide a random one
        if (v == null) v = Vec.fill[Double](length)(r.gen)
            
        for (i <- range) this(i) = v(i)
    }
    
    /*::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::*/
    /** Return the mean of the random vector.
     */
    override def mean(): Double = {
        val values = for (i <- range) yield this(i) * p(i)
        values.reduceLeft(_+_)
    }
    
    
}