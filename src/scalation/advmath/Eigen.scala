/*::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::*/
/**
 * @author  John Miller, Robert Davis
 * @version 1.0
 * @date    Thu Jan 28 13:29:27 EST 2010
 * @see     LICENSE (MIT style license file).
 *
 * This file contains classes for Hessenburg reductions, QR decompositions,
 * finding Eigenvalues and computing Eigenvectors.  The first two are used
 * in finding Eigenvalues, but are also useful in their own right. 
 */

package scalation.advmath

import scala.math.{abs, pow, signum, sqrt}
import scalation.advmath.Matrices._
import scalation.advmath.Vectors._
import scalation.util.Error

/*::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::*/		
/**
 * This class is used to reduce, via similarity transformations, an n by n matrix
 * 'a' to Hessenburg form 'h', where all elements two below the main diagonal are
 * zero (or close to zero).  Note, similarity transformations do not changes the
 * eigenvalues.
 * @param a  the matrix to reduce to Hessenburg form
 */
class Hessenburg (a: MatrixD)
      extends Error
{
    /** The Hessenburg h matrix
     */
    private var h = new MatrixN [Double] (a)

    {
        val m = a.dim1
        val n = a.dim2
        if (m != n) flaw ("constructor", "must have m == n")

        for (j <- 0 until n) {                // for each column j
            val x  = h.col(j, j)              // jth column from jth position
            val u  = x + x.one (0) * x.norm * (if (x(0) < 0.) -1. else 1.)
            val ident1 = new MatrixN [Double] (n - j, 1., 0.)
            val ident2 = new MatrixN [Double] (j, 1., 0.)
            val pp = ident1 - (u outer u) * (2. / u.norm2)
            val p  = ident2 diag pp
            h = p.t * h * p
        } // for
    } // primary constructor

    /*::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::*/
    /**
     * Get the Hessenburg h matrix.
     */
    def getH: MatrixD = h

} // Hessenburg class

/*::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::*/
/**
 * This class is used to decompose an m by n matrix 'a' into an orthogonal m by n
 * matrix 'q' and an n by n right upper triangular matrix 'r' such that a = q * r.
 * It uses Gram-Schmidt orthogonalization.
 * Note, orthogonal means that * q.t * q = I.
 * @see http://www.stat.wisc.edu/~larget/math496/qr.html
 * @see http://en.wikipedia.org/wiki/Gram–Schmidt_process
 *      (stabilized Gram–Schmidt orthonormalization)
 * @param a  the matrix to decompose into q and r
 */
class QRdecomposition (a: MatrixD)
      extends Error
{
    /** The orthogonal q matrix
     */
    private val q = new MatrixN [Double] (a)

    /** The right upper triangular r matrix
     */
    private val r = new MatrixN [Double] (a.dim2, a.dim2)

    {
        val m = a.dim1
        val n = a.dim2
        if (n > m) flaw ("constructor", "must have m >= n")

        for (j <- 0 until n) {                // for each column j
            val _norm = q.col(j).norm         // norm of the jth column
            r(j, j) = _norm

            if (_norm != 0.) {
                for (i <- 0 until m) q(i, j) /= _norm
                for (k <- j + 1 until n) {
                    r(j, k) = q.col(j) dot q.col(k)
                    for (i <- 0 until m) q(i, k) -=  q(i, j) * r(j, k)
                } // for
             } // if

         } // for
    } // primary constructor

    /*::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::*/
    /**
     * Get the orthogonal q matrix.
     */
    def getQ: MatrixD = q

    /*::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::*/
    /**
     * Get the right upper triangular r matrix.
     */
    def getR: MatrixD = r

} // QRdecomposition class

/*::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::*/
/**
 * This class is used to find the eigenvalues of an n by n matrix 'a' using an
 * iterative technique that applies similarity transformations to convert 'a' into
 * an upper triangular matrix, so that the eigenvalues appear along the diagonal.
 * To improve performance, the 'a' matrix is first reduced to Hessenburg form.
 * During the iterative steps, a shifted QR decomposition is performed.
 * Caveats: (i) it will not handle eigenvalues that are complex numbers,
 *          (ii) it uses a simple shifting strategy that may slow convergence (FIX).
 * @param a  the matrix whose eigenvalues are sought 
 */
class Eigenvalue (a: MatrixD)
      extends Error
{
    /** Flag indicating whether tracing is on to monitor convergence
     */
    private val trace = true

    /** The vector of eigenvalues
     */
    private val e = VectorN [Double] (a.dim1)

    /** Error tolerance value
     */
    private val EPSILON = 1e-6

    {
        val m = a.dim1
        val n = a.dim2
        if (m != n) flaw ("constructor", "must have m == n")
        var g = (new Hessenburg (a)).getH         // convert g matrix to upper triangular
        var converging = true                     // still converging, has not converged yet
        val iterations = 6                        // increase --> more precision, but slower
        var lastE      = Double.PositiveInfinity  // save an eigenvalue from last iteration

        for (k <- 0 until iterations if converging) {  // major iterations
            converging = true
            for (l <- 0 until iterations) {            // minor iterations
                val s = g(n - 1, n - 1)               // the shift parameter
                val ident = new MatrixN [Double] (g.dim1, 1., 0.)
                val qr = new QRdecomposition (g - ident * s)
                g = qr.getR * qr.getQ + ident * s
            } // for

            for (i <- 0 until n) e(i) = g(i, i)       // extract eigenvalues from diagonal
            val e0 = e(0)                             // consider one eigenvalue
            if (abs ((lastE - e0) / e0) < EPSILON) {  // relative error
                converging = false                    // end major iterations
            } else {
                lastE = e0                            // save this eigenvalue
            } // if

            if (trace) {
                println ("-------------------------------------------")
                println ("Eigenvalue: on iteration " + k + " g = " + g)
                println ("Eigenvalue: on iteration " + k + " e = " + e)
                if ( ! converging) println ("Eigenvalue: converged!")
            } // if
        } // for
    } // primary constructor

    /*::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::*/
    /**
     * Get the eigenvalue e vector.
     */
    def getE: VectorD = e
} // Eigenvalue class

/*::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::*/
/**
 * This class is used to find the eigenvectors of an n by n matrix 'a' by solving
 * equations of the form (a - eI)v = 0 where e is the eigenvalue and v is the
 * eigenvector.  Place the eigenvectors in a matrix column-wise.
 * @param a   the matrix whose eigenvectors are sought 
 * @param _e  the vector of eigenvalues of matrix a
 */
class Eigenvector (a: MatrixD, _e: VectorD = null)
      extends Error
{
    /** The matrix of eigenvectors (each row holds an eigenvector)
     */
    private val v = MatrixN [Double] (a.dim1, a.dim1)

    {
        val m = a.dim1
        val n = a.dim2
        if (n != m) flaw ("constructor", "must have m == n")

        val e = if (_e == null) (new Eigenvalue (a)).getE else _e
        for (i <- 0 until n) {        // compute eigenvector for ith eigenvalue
            val ident = new MatrixN [Double] (a.dim1, 1., 0.)
            v.setColumn (i, (a - ident * e(i)).slice(0,n-1).nullspace)
        } // for
    } // primary constructor

    /*::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::*/
    /**
     * Get the eigenvector v matrix.
     */
    def getV: MatrixD = v 

} // Eigenvector class

/*::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::*/
/**
 * This object is used to test the all the classes used in computing Eigenvalues
 * and Eigenvectors for the non-symmetric/general case.
 * @see http://en.wikipedia.org/wiki/QR_decomposition
 */
object EigenTest extends Application
{
    val b = new MatrixD (3, -149., -50., -154.,                 // 3-by-3 matrix
                             537., 180.,  546.,
                             -27.,  -9.,  -25.)
    val e = (new Eigenvalue (b)).getE
    println ("e = " + e)                  // should give 3, 2, 1
    val v = (new Eigenvector (b, e)).getV

    println ("v = " + v)
    for (i <- 0 until v.dim1) {   // check that b * v_i = e_i * v_i
        println ("b    * v(i) = " + (b    * v.row (i)))
        println ("v(i) * e(i) = " + (v.row (i) * e(i)))
    } // for

/***
    val b = new MatrixD (3, 2.,  1.,  3.,                       // 3-by-3 matrix
                           -1.,  0.,  7.,
                            0., -1., -1.)
    val h = (new Hessenburg (b)).getH
    println ("h = " + h)

    val b = new MatrixD (3, 12.,  -51.,   4.,                   // 3-by-3 matrix
                             6.,  167., -68.,
                            -4.,   24., -41.)
    val qr = new QRdecomposition (b)
    val q = qr.getQ
    val r = qr.getR
    println ("b = " + b)
    println ("q = " + q)
    println ("r = " + r)
    println ("q*r = " + q * r)
    println ("q.t*q = " + q.t * q)
***/

} // EigenTest object
