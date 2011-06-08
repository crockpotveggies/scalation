/* $Id: package.scala 92 2011-04-20 15:35:35Z mepcotterell@gmail.com $ */

package scalation

/*::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::*/
/** Statistics Package
 */
package object stat {

    import advmath._

    // implicitly converts Vec[Double]s into RandVecs as needed.
    implicit def mkVecD2RandVec(v: Vec[Double]) = new RandVec(v.length, v)

    /*::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::*/
    /** The Stat trait includes functions that are useful for Output Analysis.
     */
    trait Stat extends ScalaTion {

        /*::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::*/
        /** Returns the mean of a RandVec
         */
        def µ(x: RandVec): Double = x.mean()

        /*::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::*/
        /** Returns the mean square of a RandVec
         */
        def ms(x: RandVec): Double = µ(x ↑ 2)

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
            val (µ1, µ3, σ1) = (µ(x), µ(x ↑ 3), σ(x))
            (µ3 - 3 * µ1 * σ1 ↑ 2 - µ1 ↑ 3) / (σ1 ↑ 3)
        }

        /*::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::*/
        /** Returns the covariance statistic of two RandVecs
         */
        def cov(x: RandVec, y: RandVec) = µ(x * y) - µ(x) * µ(y)

        /*::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::*/
        /** Returns the correlation statistic of two RandVecs
         */
        def ρ(x: RandVec, y: RandVec): Double = cov(x, y) / (σ(x) * σ(y))

        /*::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::*/
        /** Returns the autocorrelation statistic of a RandVec
         */
        def ρ(x: RandVec): Double = {
            val n = x.length
            ρ(x(0 ⋯ (n - 2)), x(1 ⋯ (n - 1)))
        }

    }
    
    /*::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::*/
    /** The Batch Means object includes functions that are useful for the 
     *  method of batch means in Output Analysis.
     */
    object BatchMeans extends Stat with ScalaTion {
        
        // @TODO include the code for batch means
        
    }
    
    /*::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::*/
    /** The Anova class provides the functions necessary for performing a 
     *  one-way Analysis of Variance on the input matrix x.
     */
    class Anova(x: MatrixN[Double]) extends Stat with ScalaTion {
        
        val m = x.dim1	// m rows
        val n = x.dim2	// n columns
        
        /*::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::*/
        /** Returns the grand mean.
         */
        def gµ = ∑(0, m-1, (i: Int) => µ(x(i))) / m
        
        /*::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::*/
        /** Returns the total sum of squares.
         */
        def sst = ∑(0, m-1, (i: Int) => ∑(x(i)↑2)) - m*n*gµ↑2
        
        /*::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::*/
        /** Returns the between-groups sum of squares.
         */
        def ssb = ∑(0, m-1, (i: Int) => ∑(x(i))↑2/n) - m*n*gµ↑2
        
        /*::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::*/
        /** Returns the within-groups sum of squares.
         */
        def ssw = sst - ssb
        
        /*::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::*/
        /** Returns the F-statistic.
         */
        def f = (ssb / m-1) / (ssw / m*(n-1))
        
    }
}