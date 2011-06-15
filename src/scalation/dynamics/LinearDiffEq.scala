
/*::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::*/
/**
 * @author  John Miller
 * @version 1.0
 * @date    Fri Jan 29 18:36:48 EST 2010
 * @see     LICENSE (MIT style license file).
 */

package scalation
package dynamics

import scala.math.exp
import advmath._
import advmath.Matrices._
import util.Error

/*::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::*/
/**
 * This class may be used for solving a system of linear differential equations
 * that are ordinary and first-order with constant coefficients of the form
 * y(t)' = a * y(t) where ' is d/dt, y(t) is the vector function of time and a is
 * the coefficient matrix.  The initial value vector y0 = y(0) must also be given.
 * Note, higher-order differential equations may be converted to first-order by
 * introducing additional variables.  The above equation is the homogeneous case.
 * Caveats: the following cases are not currently handled:
 * (i) The non-homogeneous equation: y(t)' = a * y(t) + f(t).
 * (ii) Complex or repeated eigenvalues.
 * @param a   the coefficient matrix
 * @param y0  the initial value vector
 */
class LinearDiffEq (a: MatrixD, y0: Vec[Double])
      extends Error
{
     {
         if (a.dim2 != y0.length) flaw ("constructor", "incompatible dimensions")
     } // primary constructor

     /** Vector of eigenvalues
      */
     private val e = (new Eigenvalue (a)).getE

     /** Matrix of eigenvectors
      */
     private val v = (new Eigenvector (a, e)).getV

     /** Vector of constants
      */
     private val c = v.solve (y0)

     /** Matrix of tranformed/final constants
      */
     private val k = v ** c

     /*::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::*/
     /**
      * Apply the exponential 'exp' function to each element of a vector.
      * @param v  the vector to apply the exp function to
      */
     def expV (v: Vec[Double]): Vec[Double] =
     {
         val z = Vec.ofLength[Double] (v.length)
         for (i <- 0 until z.length) z(i) = exp (v(i))
         z
     } // expV

     /*::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::*/
     /**
      * Evaluate the solution for y(t) at time t.
      * @param t  the time point
      */
     def eval (t: Double): Vec[Double] = k * expV (e * t)

     /*::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::*/
     /**
      * Print the solution to the differential equation.
      */
     def print
     {
         println ("---------------------------------------")
         println ("System of Linear Differential Equations")
         println ("Solve: y(t)' = a * y(t) where y(0) = y0")
         println ("coefficient matrix    a  = " + a)
         println ("initial state vector y0 = " + y0)
         println ("eigenvalue vector     e  = " + e)
         println ("eigenvector matrix    v  = " + v)
         println ("constant vector       c  = " + c)
         println ("constant matrix       k  = " + k)
         println ("---------------------------------------")
     } // print

} // LinearDiffEq class


/*::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::*/
/**
 * Object to test the LinearDiffEq class using example at
 * @see biomed.tamu.edu/faculty/wu/BMEN_452/Eigenvalue%20Problems.doc
 * The eigenvalues should be (-3, -1)
 * The constant matrix should be [ (.375, .625), (-.75, 1.25) ]
 */
object LinearDiffEqTest extends App
{
    val a  = new MatrixD (2, -2.,  0.5,                         // 2-by-2 matrix
                              2., -2.)
    val y0 = Vec(1., 0.5)
    val de = new LinearDiffEq (a, y0)
    de.print

    for (i <- 0 until 10) {
        val t = 0. + i
        println ("at t = " + t + " trajectory = " + de.eval (t))
    } // for

} // LinearDiffEqTest object

