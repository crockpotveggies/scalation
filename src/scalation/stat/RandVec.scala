package scalation
package stat

import advmath._

/*::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::*/
/** Companion object for RandVec class.
 */
object RandVec 
{   
    /*::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::*/
    /** Returns a new RandVec of a specified length.
     */
    def ofLength(length: Int, v: Vec[Double] = null, p: Vec[Double] = null) = new RandVec(length, v, p)
}

/*::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::*/
/** The RandVec class is a random numeric vector of a specific length. If values
 *  and probabilities are provided, then the mean statistic takes into
 *  consideration the probability distribution. If no probability distribution
 *  is provided, then RandVec assumes a uniform distribution of probabilities.
 */
class RandVec (length: Int, var v: Vec[Double] = null, var p: Vec[Double] = null) 
extends Vec[Double] (Array.ofDim[Double](length)) 
    with ScalaTion 
{

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
    /** Return the mean statistic of the random vector. (weighted average)
     */
    override def mean(): Double = ∑(range, (i: Int) ⇒ this(i) * p(i))
    
}