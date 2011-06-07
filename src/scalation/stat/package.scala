/* $Id: package.scala 92 2011-04-20 15:35:35Z mepcotterell@gmail.com $ */

package scalation

/*::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::*/
/**
 * Statistics Package
 */
package object stat
{

    import advmath._
    
    implicit def mkVecD2RandVec(v: VecD) = new RandVec(v.length, v)
    
	object Stat extends ScalaTion {
	    
	    /*::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::*/
	    /** Returns the mean of a RandVec
	     */
	    def µ(x: RandVec): Double = x.mean()
	    
	    /*::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::*/
	    /** Returns the mean square of a RandVec
	     */
	    def ms(x: RandVec): Double = µ(x↑2)
	    
	    /*::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::*/
	    /** Returns the variance of a RandVec
	     */
	    def σ2(x: RandVec): Double = ms(x) - µ(x) ↑ 2
	    
	    /*::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::*/
	    /** Returns the standard deviation of a RandVec
	     */
	    def σ(x: RandVec): Double = σ2(x) ↓ 2
	
	    /*::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::*/
	    /** Returns the skewness statistic of a RandVec
	     */
	    def γ1(x: RandVec) = {
	        val (µ1, µ3, σ1) = (µ(x), µ(x↑3), σ(x))
	        (µ3 - 3 * µ1 * σ1↑2 - µ1↑3) / (σ1↑3)
	    }
	    
	    /*::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::*/
	    /** Returns the covariance statistic of two RandVecs
	     */
	    def cov(x: RandVec, y: RandVec) = µ(x*y) - µ(x) * µ(y)
	    
	    /*::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::*/
	    /** Returns the correlation statistic of two RandVecs
	     */
	    def ρ(x: RandVec, y: RandVec): Double = cov(x, y) / (σ(x) * σ(y))
	    
	    /*::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::*/
	    /** Returns the autocorrelation statistic of a RandVec
	     */
	    def ρ(x: RandVec): Double = {
	        val n = x.length
	        ρ(x(0⋯(n-2)), x(1⋯(n-1)))
	    }
    }
}