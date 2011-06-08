package scalation
package stat

import advmath._

object rand {
    val r = new Random(0)
    def gen = r.gen
}

object RandVec {
    def ofLength(length: Int) = new RandVec(length)
}

class RandVec (length: Int, var v: VecD = null, var p: VecD = null) extends Vec[Double] (Array.ofDim[Double](length)) {

    {
        // if no probabilities are given, assume uniform distribution of probabilities
        if (p == null) p = Vec.fromSeq(Array.fill(length)(1.0/length).toSeq)
     
        // if no array is given, then provide a random one
        if (v == null) v = Vec.fill[Double](length)(rand.gen)
            
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