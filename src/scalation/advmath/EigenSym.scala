/**:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
 * @author  John Miller, Robert Davis
 * @version 1.0
 * @date    Thu Jan 28 13:29:27 EST 2010
 * @see     LICENSE (MIT style license file).
 *
 * This file contains classes for Householder Tridiagonalizations, 
 * symmetric QR steps, and finding eigenvalues of symmetric matrices.
 */

package scalation.advmath

import scala.math.{abs, pow, sqrt}
import scalation.advmath.Matrices._
import scalation.advmath.Vectors._
import scalation.util.Error

/**:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
 * This class performs a Householder Tridiagonalization on a symmetric matrix.
 * @see Algorithm 8.3.1 in Matrix Computations.
 * @param a  the symmetric matrix to tridiagonalize
 */
class Householder (a: MatrixN [Double])
      extends Error
{
    /** The Householder tridiagonal matrix
     */
    private val t = new SymmetricTridMatrixN [Double] (a.dim1)
    {
        if (a.dim1 != a.dim2) flaw ("constructor", "must have m == n")
        if (! a.isSymmetric)  flaw ("constructor", "matrix a must be symmetric")
        val n = a.dim1 - 1         // the last index
        for (k <- 0 to n - 2) {
            val ts = a.col (k).slice (k + 1, n + 1)
            val v_b = house (ts)
            val v   = v_b._1; val b = v_b._2
            val p   = a.slice (k + 1, n + 1, k + 1, n + 1) * v * b 
            val w   = p - v * ((b / 2) * (p.dot(v)))
            t(k, k) = a(k, k)
            t(k + 1, k) = ts.norm;
            for (i <- k + 1 to n; j <- k + 1 to n) {
                a(i, j) = a(i, j) - (v(i - (k + 1)) * w(j - (k + 1)) +
                                     w(i - (k + 1)) * v(j - (k + 1)))
            } // for
        } // for
        t(n - 1, n)     = a(n-1,n)
        t(n - 1, n - 1) = a(n-1,n-1)
        t(n, n)         = a(n, n)
    } // primary constructor

    /**:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
     * Compute the Householder vector v and its corresponding scalar b,
     * where I - b * v * v.t is an orthogonal matrix.
     * @see Algorithm 5.1.1 in Matrix Computations.
     * @param x  the vector to create the Householder vector from
     */
    def house (x: VectorD): Tuple2 [VectorD, Double] =
    {
        var b  = 0.
        var v = new VectorD (x)
        v(0) = 1.
        val s= v.norm2 - 1
        if (s != 0.) {
            val y = x(0)
            val m = sqrt (y * y + s)
            val z = if (y <= 0) y - m else -s / (y + m)
            v(0) = z
            b = 2. * z * z / (z * z + s)
            v /= z
        } // if
        Tuple2 (v, b)
    } // house

    /**:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
     * Get the Householder Tridiagonal matrix.
     */
    def getT: SymmetricTridMatrixN [Double] = t

} // Householder class

/**:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
 * This class performs an in place symmetric QR step with a Wilkinson shift on
 * an unreduced submatrix of a symmetric tridiagonal matrix.
 * @see Algorithm 8.3.2 in Matrix Computations.
 * @param t  the unreduced symmetric tridiagonal matrix
 * @param p  the starting index of the submatrix
 * @param q  the ending index of the submatrix
 */
class SymmetricQRstep (t: SymmetricTridMatrixN [Double], p: Int, q: Int) extends Error
{ 
    {
        val n   = q
        val d   = (t(n - 1, n - 1) - t(n, n)) / 2.
        val t2  = pow (t(n, n - 1), 2)
        val m   = t(n, n) - t2 / (d + (if (d < 0) -1 else 1) * sqrt (d * d + t2))
        var g   = t(p, p) - m
        var s   = 1.0
        var c   = 1.0
        var phi = 0.0
        for (k <- p until n) {
            var f = s * (t.sd(k))
            var b = c * (t.sd(k))
            var r = sqrt(g * g + f * f)
            c   = g / r
            s   = f / r
            if( k != 0 ) t.sd(k - 1) = r
            g   = t.dg(k) - phi
            r   = (t.dg(k + 1) - g) * s + 2 * c * b
            phi = s * r
            t.dg(k) = g + phi
            g = c * r - b
        } // for
        t.dg(n) = t.dg(n) - phi
        t.sd(n - 1) = g
    } 

} // SymmetricQRstep class

/**:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
 * This class is used to find the eigenvalues of an n by n symmetric matrix 'a'
 * using an iterative technique, the Symmetric QR Algorithm.
 * @see Algorithm 8.3.3 in Matrix Computations.
 * Caveats: (i) it will not handle eigenvalues that are complex numbers,
 *          (ii) it uses a simple shifting strategy that may slow convergence (FIX).
 * @param a  the symmetric matrix whose eigenvalues are sought
 */
class EigenvalueSym (a: MatrixN [Double])
      extends Error
{
    /** Flag indicating whether tracing is on to monitor convergence
     */
    private val trace = true

    /** The vector of eigenvalues
     */
    private var d: SymmetricTridMatrixN [Double] = null

    /** Error tolerance value
     */
    private val EPSILON = 1e-9

    {
        val m = a.dim1
        val n = a.dim2
        if (m != n)          flaw ("constructor", "must have m == n")
        if (! a.isSymmetric) flaw ("constructor", "matrix a must be symmetric")
        var q = 0
        var p = n
        d = (new Householder (a)).getT
        while (q != n - 1) {
            for (i <- 0 to n - 2 if abs (d(i, i + 1)) <=  EPSILON) {
                d(i, i + 1) = 0.
            } // for
            q = 0; p = m - 1
            while (p > 0 && d(p, p - 1) == 0. && q < n) {
                q = q + 1;
                p = p - 1;
            } // while
            while (p > 0 && d(p, p - 1) != 0.) {
                p = p - 1;
            } // while
            
            if (q != n - 1) new SymmetricQRstep(d, p, n - q - 1)
           
        } // while
    } // primary constructor

    /**:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
     * Get the eigenvalue e vector.
     */
    def getE: VectorD = d.dg     // the diagonal of the tridiagonal matrix

} // EigenvalueSym

/**:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
 * This object is used to test the all the classes used in computing Eigenvalues
 * and Eigenvectors for the symmetric/special case.
 */
object EigenSymTest extends Application
{
    val t = new MatrixD (2, 2.64,  -0.48,
                            -0.48,  2.64)
    val v = (new EigenvalueSym (t)).getE
    println ("t = " + t)
    println ("v = " + v)

} // EigenSymTest object

