
//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  John Miller
 *  @version 1.0
 *  @date    Wed Aug 24 15:05:08 EDT 2011
 *  @see     LICENSE (MIT style license file).
 */

package scalation.optimization

import math.abs
import util.control.Breaks.{breakable, break}

import scalation.math.Matrices.MatrixD
import scalation.math.Vectors._
import scalation.util.Error

//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** This class solves Quadratic Programming (QP) problems using the Quadratic
 *  Simplex Algorithm.  Given a constraint matrix 'a', constant vector 'b',
 *  cost matrix 'q' and cost vector 'c', find values for the solution/decision
 *  vector x that minimize the objective function f(x), while satisfying all of
 *  the constraints, i.e.,
 *
 *  minimize    f(x) = 1/2 x q x + c x
 *  subject to  a x >= b, x >= 0
 *
 *  Creates an MM-by-NN simplex tableau.  This implementation is restricted to
 *  linear constraints (a x <= b) and q being a positive semi-definite matrix.
 *  Pivoting must now also handle nonlinear complementary slackness
 *  @see http://www.engineering.uiowa.edu/~dbricker/lp_stacks.html
 *
 *  @param a      the M-by-N constraint matrix
 *  @param b      the M-length constant/limit vector
 *  @param q      the M-by-N cost/revenue matrix (second order component)
 *  @param c      the N-length cost/revenue vector (first order component)
 *  @param x_B    the initial basis (set of indices where x_i is in the basis)
 */
class QuadraticSimplex (a: MatrixD, b: VectorD, q: MatrixD, c: VectorD, var x_B: VectorI = null)
      extends Error
{
    private val DEBUG    = true                      // if true, show each pivot
    private val MAX_ITER = 100                       // maximum number of iterations
    private val EPSILON  = 1.E-10                    // constant for a value almost 0
    private val M        = a.dim1                    // number of constraints (rows in a)
    private val N        = a.dim2                    // number of original variables (columns in a)
    private val MM       = M + q.dim1                // # rows in tableau
    private val NN       = N + M + 2 * q.dim2 + 2    // # columns in tableau

    if (b.dim != M) flaw ("constructor", "b.dim = " + b.dim + " != " + M)
    if (c.dim != N) flaw ("constructor", "c.dim = " + c.dim + " != " + N)

    /** The (MM)-by-(NN) simplex tableau [ x, w, y, v, r | bc ]
     *  The complementary variables are x_i v_i = 0 = w_i y_i
     */
    private val t = new MatrixD (MM, NN)
    for (i <- 0 until M) {
        t.set (i, 0, a(i))                 // x: constraint matrix a
        t.set (i + M, 0, q(i) * -1.)       // x: cost matrix q (quadratic part)
        t.set (i + M, N, a.col(i) * -1)    // w: multipliers for a x <= b (transpose of a)
        t(i, i + M + N)       = 1.         // y: slack identity matrix
        t(i + M, i + 2*M + N) = 1.         // v: multipliers for a x >= 0
        t(i, NN - 1)          = b(i)       // bc: constant vector b
        t(i + M, NN - 1)      = c(i)       // bc: cost vector c (linear part)
    } // for

    for (i <- 0 until MM if t(i, NN - 1) < 0) t(i, NN - 2) = -1.   // r: artificial variable

    if (x_B == null) x_B = setBasis (MM, MM)

    showTableau                            // before pivoting the artificial variable in
    val l = NN - 2                         // entering column/variable (artificial variable)
    val k = t.col (NN - 1).argminNeg ()    // leaving row/variable
    pivot (k, l)                           // special pivot puts artificial variable in basis

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** There are M+N variables, N decision and M slack variables, of which,
     *  for each iteration, M are chosen for a Basic Feasible Solution (BFS).
     *  The the variables not in the basis are set to zero.  Setting j to N
     *  will start with the slack variables in the basis (only works if b >= 0).
     *  @param j  the offset to start the basis
     *  @param l  the size of the basis
     */
    def setBasis (j: Int = N, l: Int = M): VectorI =
    {
        for (i <- 0 until l) yield i + j
    } // setBasis

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Find a variable x_l to enter the basis.  Determine the index of entering
     *  variable corresponding to column l.  Neighter the variable nor its complement
     *  may be in the current basis.   Return -1 to indicate no such column.
     */
    def entering (): Int =
    {
        var found = true
        for (l <- 0 until NN - 1) {
            found = true
            breakable { for (k <- 0 until x_B.dim if l == x_B(k) || comple (l) == x_B(k)) {
                found = false; break
            }} // for
            if (found) return l
        } // for
        return -1
    } // entering

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Return l's complementary variable.
     *  @param l  whose complement
     */
    def comple (l: Int): Int =
    {
             if (l < N)         l + 3 * M
        else if (l < N + M)     l + M
        else if (l < N + 2 * M) l - M
        else                    l - 3 * M
    } // comple

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Find the best variable x_k to leave the basis given that x_l is entering.
     *  Determine the index of the leaving variable corresponding to row k using
     *  the Min-Ratio Rule.  Return -1 to indicate no such row.
     *  @param l  the entering variable (column)
     */
    def leaving (l: Int): Int =
    {
        var k = -1
        for (i <- 0 until MM if t(i, l) > 0) {
            if (k == -1) k = i
            else if ((t(i, NN - 1) / t(i, l)) < (t(k, NN - 1) / t(k, l))) k = i
        } // for
        if (k == -1) println ("the solution is unbounded")
        k
    } // leaving

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Pivot on entry (k, l) using Gauss-Jordan elimination to replace variable
     *  x_k with x_l in the basis.
     *  @param k  the leaving variable (row)
     *  @param l  the entering variable (column)
     */
    def pivot (k: Int, l: Int)
    {
        println ("pivot: " + k + " leaves and " + l + " enters")
        // everything but row k and column l
        for (i <- 0 until MM; j <- 0 until NN if i != k && j != l)
            t(i, j) -= t(k, j) * t(i, l) / t(k, l)

        for (i <- 0 until MM if i != k) t(i, l) = 0.          // zero out column l
        for (j <- 0 until NN if j != l) t(k, j) /= t(k, l)    // scale row k
        t(k, l) = 1.
        x_B(k) = l                                  // update basis (l replaces k)
        if (DEBUG) showTableau
    } // pivot

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Run the simplex algorithm starting from the initial BFS and iteratively
     *  find a non-basic variable to replace a variable in the current basis
     *  so long as the objective improves.
     */
    def solve (): Tuple2 [VectorD, Double] =
    {
        var k = -1       // the leaving variable (row)
        var l = -1       // the entering variable (column)
        showTableau

        breakable { for (it <- 1 to MAX_ITER) {
            l = entering (); if (l == -1) break      // optimal solution found
            k = leaving (l); if (k == -1) break      // solution is unbounded
            pivot (k, l)                       // pivot: k leaves and l enters
        }} // for

        showTableau
        val x = primal
        (x, objValue (x))    // return the primal solution and the optimal value
    } // solve

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Return the tableau (t).
     */
    def tableau: MatrixD = t

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Return the primal solution vector (x).
     */
    def primal: VectorD =
    {
        val x = new VectorD (N)
        for (i <- 0 until MM if x_B(i) < N) x(x_B(i)) = t(i, NN - 1)
        x
    } // primal

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Return the dual solution vector (y).
     */
    def dual: VectorD = null   // FIX

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Return the optimal objective function value (f(x) = 1/2 x q x + c x).
     *  @param x  the primal solution vector
     */
    def objValue (x: VectorD): Double = (x dot (q * x)) * .5 + (c dot x)

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Show the current basis and tableau.
     */
    def showTableau
    {
        println ("basis x_B = " + x_B)
        println ("tableau t = " + t)
    } // showTableau

} // QuadraticSimplex class


//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** This object is used to test the QuadraticSimplex class.
 */
object QuadraticSimplexTest extends App
{
    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Test the QuadraticSimplex Algorithm for solving Quadratic Programs:
     *  max { 1/2 x q x + c x | a x <= b, x >= 0 }.
     *  @param a  the constraint matrix
     *  @param b  the constant vector
     *  @param 1  the cost matrix
     *  @param c  the cost vector
     */
    def test (a: MatrixD, b: VectorD, q: MatrixD, c: VectorD)
    {
        val lp = new QuadraticSimplex (a, b, q, c)
        val x  = lp.solve ()._1
        println ("tableau         t = " + lp.tableau)
        println ("primal vector   x = " + lp.primal)
        println ("dual vector     u = " + lp.dual)
        println ("objective value f = " + lp.objValue (x))
    } // test

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Test case 1: solution x = (9, 9, 4), z = 22.
     */
    def test1 ()
    {
        val a = new MatrixD ((2, 2),  2., 1.,
                                     -1., 1.)
        val b = new VectorD (2., 4.)
        val q = new MatrixD ((2, 2),  2., -2,
                                     -2.,  2)
        val c = new VectorD (-4., -6.)
        test (a, b, q, c)
    } // test1

    println ("--------------------------------")
    test1 ()
    println ("--------------------------------")

} // QuadraticSimplexTest object

