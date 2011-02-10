
/*:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::*/
/**
 * @author  John Miller
 * Translated from Java code in Algorithms, 4th Edition, Robert Sedgewick and Kevin Wayne
 * @version 1.0
 * @date    Sat Sep 25 19:25:18 EDT 2010
 * @see     LICENSE (MIT style license file).
 */

package scalation.optimization

import scala.math.abs

import scalation.advmath._
import scalation.advmath.Matrices._
import scalation.advmath.Vectors._

/*:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::*/
/**
 * This class solves Linear Programming (LP) problems using the Simplex Algorithm.
 * Given an constraint matrix a, constant vector b and cost vector c, find values
 * for the solution/decision vector x that maximize the objective function z,
 * while satisfying all of the constraints, i.e.,
 *
 * maximize    z = c x
 * subject to  a x <= b, x >= 0
 *
 * Assumes that b >= 0 so that x = 0 is a basic feasible solution.
 * Creates an (M+1)-by-(N+M+1) simplex tableaux with
 * -- a (constraint matrix) in columns 0 to N-1
 * -- s (slack identity matrix) in columns N through M+N-1.
 * -- b (constant vector) in column M+N,
 * -- c (cost vector) in row M
 * @see http://www.cs.princeton.edu/algs4/63or/Simplex.java.html
 * @param a  the M-by-N constraint matrix
 * @param b  the M-length constant vector
 * @param c  the N-length cost/revenue vector
 */
class Simplex (a: MatrixD, b: VectorD, c: VectorD)
{
    /** Constant for a value almost 0
     */
    private val EPSILON = 1.0E-10

    /** The number of constraints (rows in matrix a/elements in vector b)
     */
    private val M = b.dim

    /** The number of original variables (columns in matrix a/elements in vector c)
     */
    private val N = c.dim

    /** The (M+1)-by-(N+M+1) simplex tableaux
     */
    private val t = new MatrixD (M + 1, N + M + 1, null)
    for (i <- 0 until M; j <- 0 until N) t(i, j) = a(i, j)    // constraint matrix a
    for (i <- 0 until M) t(i, N + i) = 1.                     // slack identity matrix s
    for (i <- 0 until M) t(i, M + N) = b(i)                   // constant vector b
    for (j <- 0 until N) t(M, j)     = c(j)                   // cost vector c

    /** There are M+N variables, N decision and M slack variables, of which,
     *  for each iteration, M are chosen for a Basic Feasible Solution (BFS).
     *  The the variables not in the basis are set to zero.
     *  basis(i) = the basic variable corresponding to row i
     */
    private val basis = new VectorI (M, null)
    for (i <- 0 until M) basis(i) = N + i     // start with the slack variables in the basis

    /*:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::*/
    /**
     * Run the simplex algorithm starting from the initial BFS and iteratively
     * find a non-basic variable to replace a variable in the current basis
     * so long as the objective improves.
     */
    def solve ()
    {
        var p = 0    // the leaving variable
        var q = 0    // the entering variable

        while (true) {

            // find entering variable (column) => q
            q = t(M).firstPos ()      // use Bland's rule (lowest index, +ve)
//          q = t(M).argmaxPos ()     // use Dantiz's rule (min index, +ve, cycling possible)
            if (q == -1) return       // optimal solution found

            // find leaving variable (row) => p
            p = minRatioRule (q)
            if (p == -1) { println ("solve: Linear program is unbounded"); return }

            pivot (p, q)     // pivot: q enters and p leaves
            basis (p) = q    // update basis (q replaces p)
        } // while
    } // solve

    /*:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::*/
    /**
     * Find the leaving variable (row) p using Min-ratio rule (-1 if no such row).
     * @param q  the entering variable (column)
     */
    private def minRatioRule (q: Int): Int =
    {
        var p = -1
        for (i <- 0 until M if t(i, q) > 0) {
            if (p == -1) p = i
            else if ((t(i, M + N) / t(i, q)) < (t(p, M + N) / t(p, q))) p = i
        } // for
        p
    } // minRatioRule

    /*:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::*/
    /**
     * Pivot on entry (p, q) using Gauss-Jordan elimination.
     * @param p  the leaving variable (row)
     * @param q  the entering variable (column)
     */
    private def pivot (p: Int, q: Int)
    {
        // everything but row p and column q
        for (i <- 0 to M; j <- 0 to M + N if i != p && j != q)
            t(i, j) -= t(p, j) * t(i, q) / t(p, q)

        // zero out column q
        for (i <- 0 to M if i != p) t(i, q) = 0.0

        // scale row p
        for (j <- 0 to M + N if j != q) t(p, j) /= t(p, q)
        t(p, q) = 1.0
    } // pivot

    /*:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::*/
    /**
     * Return the tableau (t).
     */
    def tableau: MatrixD = t

    /*:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::*/
    /**
     * Return the primal solution vector (x).
     */
    def primal: VectorD =
    {
        val x = new VectorD (N, null)
        for (i <- 0 until M if basis(i) < N) x(basis(i)) = t(i, M + N)
        x
    } // primal

    /*:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::*/
    /**
     * Return the dual solution vector (y).
     */
    def dual: VectorD =
    {
        val y = new VectorD (M, null)
        for (i <- 0 until M) y(i) = -t(M, N + i)
        y
    } // dual

    /*:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::*/
    /**
     * Return the optimal objective value (z).
     */
    def value: Double = -t(M, M + N)

    /*:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::*/
    /**
     * Check for feasibility and optimality.
     */
    def check: Boolean = isPrimalFeasible && isDualFeasible && isOptimal

    /*:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::*/
    /**
     * Is the solution primal feasible?
     */
    private def isPrimalFeasible: Boolean =
    {
       val x = primal

       // check that x >= 0
       for (j <- 0 until N if x(j) < 0.) {
           println ("isPrimalFeasible: x(" + j + ") = " + x(j) + " is negative")
           return false
       } // for

       // check that a x <= b
       if (a * x <= b + EPSILON) return true
       println ("isPrimalFeasible: constraint a x <= b is violated")
       return false
    } // isPrimalFeasible

    /*:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::*/
    /**
     * Is the solution dual feasible?
     */
    private def isDualFeasible: Boolean =
    {
       val y = dual

       // check that y >= 0
       for (i <- 0 until M if y(i) < 0.) {
           println ("isDualFeasible: y(" + i + ") = " + y(i) + " is negative")
           return false
       } // for

       // check that y a >= c
       if (y * a >= c - EPSILON) return true
       println ("isDualFeasible: constraint y a >= c is violated")
       false
    } // isDualFeasible

    /*:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::*/
    /**
     * Check that the optimal value z = c x = y b.
     */
    private def isOptimal: Boolean =
    {
       val x     = primal       // primal solution vector
       val y     = dual         // dual solution vector
       val z     = value        // optimal value
       val prod1 = c dot x      // c x
       val prod2 = y dot b      // y b

       // check that z = cx = yb
       if (abs (z - prod1) > EPSILON || abs (z - prod2) > EPSILON) {
           println ("isOptimal: failed since z = " + z + ", c x = " + prod1 +
                                                         ", y b = " + prod2)
           return false
       } // if
       true
    } // isOptimal

} // Simplex class


/*:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::*/
/**
 * This object is used to test the Simplex class.
 */
object SimplexTest extends Application
{
    /*:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::*/
    /**
     * Test the Simplex Algorithm for solving Linear Programs:
     * max { c x | a x <= b, x >= 0 }.
     * @param a the constraint matrix
     * @param b the constant vector
     * @param c the cost vector
     */
    def test (a: MatrixD, b: VectorD, c: VectorD)
    {
        val lp = new Simplex (a, b, c)
        lp.solve ()
        println ("check             = " + lp.check)
        println ("tableau         t = " + lp.tableau)
        println ("primal vector   x = " + lp.primal)
        println ("dual vector     y = " + lp.dual)
        println ("objective value z = " + lp.value)
    } // test

    /*:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::*/
    /**
     * Test case 1: solution x = (9, 9, 4), z = 22.
     */
    def test1 ()
    {
        val a = new MatrixD (5, -1.,  1.,  0.,                 // 5-by-3 matrix 
                                 1.,  4.,  0.,
                                 2.,  1.,  0.,
                                 3., -4.,  0.,
                                 0.,  0.,  1.)
        val b = new VectorD (5., 45., 27., 24., 4.)
        val c = new VectorD (1.,  1.,  1.)
        test (a, b, c)
    } // test1

    /*:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::*/
    /**
     * Test case 2: solution x = (12, 28), z = 800.
     */
    def test2 ()
    {
        val a = new MatrixD (3,  5., 15.,                      // 3-by-2 matrix
                                 4.,  4.,
                                35., 20.)
        val b = new VectorD (480., 160., 1190.)
        val c = new VectorD ( 13.,  23.)
        test (a, b, c)
    } // test2

    /*:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::*/
    /**
     * Test case 3: solution x = (1, 0, 1, 0), z = 1.
     * Cycles if you choose most positive objective function coefficient.
     */
    def test3 ()
    {
        val a = new MatrixD (3, .5, -5.5, -2.5, 9.,            // 3-by-4 matrix
                                .5, -1.5, -0.5, 1.,
                               1.0,  0.0,  0.0, 0.)
        val b = new VectorD ( 0.,   0.,  1.)
        val c = new VectorD (10., -57., -9., -24.)
        test (a, b, c)
    } // test3

    /*:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::*/
    /**
     * Test case 4: no solution LP is unbounded.
     */
    def test4 ()
    {
        val a = new MatrixD (2, -2., -9.,  1.,  9.,            // 2-by-4 matrix
                                 1.,  1., -1., -2.)
        val b = new VectorD (3., 2.)
        val c = new VectorD (2., 3., -1., -12.)
        test (a, b, c)
    } // test4

    println ("--------------------------------")
    test1 ()
    println ("--------------------------------")
    test2 ()
    println ("--------------------------------")
    test3 ()
    println ("--------------------------------")
    test4 ()
    println ("--------------------------------")

} // SimplexTest object

