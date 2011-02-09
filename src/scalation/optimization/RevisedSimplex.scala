
/*:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::*/
/**
 * @author  John Miller
 * @version 0.1
 * @date    Sun Sep 26 15:20:15 EDT 2010
 * @see     LICENSE (MIT style license file).
 */

package scalation.optimization

import scalation.advmath._
import scalation.advmath.Matrices._
import scalation.advmath.Vectors._

/*:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::*/
/**
 * This class solves Linear Programming (LP) problems using the Revised Simplex
 * Algorithm.  Given a constraint matrix a, constant vector b and cost vector c,
 * find values for the solution/decision vector x that maximize the objective
 * function, while satisfying all of the constraints, i.e.,
 * maximize    z = c * x
 * subject to  a * x <= b, x >= 0
 * The revised algorithm is more efficient than the Simplex Algorithm.
 * @param a  the constraint matrix
 * @param b  the constant vector
 * @param c  the cost/revenue vector
 */
class RevisedSimplex (a: MatrixD, b: VectorD, c: VectorD)
{
    /** Constant for a value almost 0
     */
    private val EPSILON = 1E-10

    /*:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::*/
    /**
     * Solve a Linear Programming (LP) problem using the Revised Simplex Algorithm.
     */
    def solve: Tuple2 [Double, VectorD] =
    {
        null       // not implemented yet
    } // solve

    /*:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::*/
    /**
     * Convert this object to a string containing the tableau.
     */
    override def toString: String = 
    {
        null       // not implemented yet
    } // toString

} // RevisedSimplex class


/*:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::*/
/**
 * Test the Revised Simplex Algorithm class with the following maximization problem:
 * Maximize    z = 2x_0 + 3x_1 + 4x_2
 * Subject to      3x_0 + 2x_1 + 1x_2 + 1y_3 + 0y_4 = 10
 *                 2x_0 + 5x_1 + 3x_2 + 0y_3 + 1y_4 = 15
 * where z is the objective variable, x are the decision variables and
 * y are slack variables.
 */
object RevisedSimplexTest extends Application
{
    // initialize matrix a and vectors b and c
    //
    val a = new MatrixD (2, 3., 2., 1.,                        // 2-by-3 matrix
                            2., 5., 3.)
    val b = new VectorD (10., 15.)
    val c = new VectorD (2., 3., 4.)

    val sa = new RevisedSimplex (a, b, c)

    println ("------------------------------------------")
    println ("max z = " + sa.solve)
    println ("------------------------------------------")

} // RevisedSimplexTest object

