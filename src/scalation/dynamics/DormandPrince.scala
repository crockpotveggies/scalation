
/*:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::*/
/**
 * @author  John Miller
 * @version 1.0
 * @date    Mon Mar 29 14:59:50 EDT 2010
 * @see     LICENSE (MIT style license file).
 */

package scalation.dynamics

import scala.math._
import scalation.advmath._
import scalation.stat._
import scalation.advmath.Vectors._
import scalation.util.Error

/*:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::*/
/**
 * Given an unknown, time-dependent function y(t) governed by an Ordinary
 * Differential Equation (ODE) of the form y(t)' = f(t, y) where ' is d/dt,
 * compute y(t) using a (4,5)-order Dormand-Prince integrator.  Note: the
 * integrateV method for a system of ODE's is mixed in from the Integrator trait.
 * @see http://adorio-research.org/wordpress/?p=6565
 */
object DormandPrince
       extends Integrator
{
    /** Butcher tableau @see http://en.wikipedia.org/wiki/Dormand–Prince_method
     */
    val a21 = 1./5.
    val a31 = 3./40.;       val a32 = 9./40.
    val a41 = 44./45.;      val a42 = -56./15.;       val a43 = 32./9.
    val a51 = 19372./6561.; val a52 = -25360./2187.;  val a53 = 64448./6561.
    val a54 = -212./729.
    val a61 = 9017./3168.;  val a62 = -355./33.;      val a63 = 46732./5247.
    val a64 = 49./176.;     val a65 = -5103./18656.
    val a71 = 35./384.;     val a72 = 0.;             val a73 = 500./1113.
    val a74 = 125./192.;    val a75 = -2187./6784.;   val a76 = 11./84.
 
    val c2 = 1./5.
    val c3 = 3./10.
    val c4 = 4./5.
    val c5 = 8./9.
    val c6 = 1.
    val c7 = 1.
 
    val b1 = 35./384.
    val b2 = 0.
    val b3 = 500./1113.
    val b4 = 125./ 192.
    val b5 = -2187./6784.
    val b6 = 11./84.
    val b7 = 0.
 
    val b1p = 5179./57600.
    val b2p = 0.
    val b3p = 7571./16695.
    val b4p = 393./640.
    val b5p = -92097./339200.
    val b6p = 187./2100.
    val b7p = 1./40.

    def integrate (f: Derivative, y0: Double, t: Double,
                   t0: Double = 0., _step: Double = defaultStepSize): Double = 0.

    /*:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::*/
    /**
     * Compute y(t) governed by a differential equation using numerical integration
     * of the derivative function f(t, y) using a 5th-order Dormand-Prince method to
     * return the value of y(t) at time t.
     * @param f         the derivative function f(t, y)
     * @param y0        value of the y-function at time t0, y0 = y(t0)
     * @param t         the time value at which to compute y(t)
     * @param hmin      the minimum step size
     * @param hmax      the maximum step size
     * @param t0        the initial time
     * @param tol       the tolerance
     * @param maxSteps  the maximum number of steps
     */
    def integrate2 (f: Derivative, y0: Double, t: Double, hmin: Double, hmax: Double,
                   t0: Double = 0., tol: Double = 1E-5, maxSteps: Int = 1000): Double =
    {
        var ti    = t0
        var y     = y0
        var h     = hmax
        var delta = 0.
        var k1    = 0.
        var k2    = 0.
        var k3    = 0.
        var k4    = 0.
        var k5    = 0.
        var k6    = 0.
        var k7    = 0.
 
        for (i <- 1 to maxSteps) {
            k1 = f (ti,          y)
            k2 = f (ti + c2 * h, y + h * (a21*k1))
            k3 = f (ti + c3 * h, y + h * (a31*k1 + a32*k2))
            k4 = f (ti + c4 * h, y + h * (a41*k1 + a42*k2 + a43*k3))
            k5 = f (ti + c5 * h, y + h * (a51*k1 + a52*k2 + a53*k3 + a54*k4))
            k6 = f (ti +      h, y + h * (a61*k1 + a62*k2 + a63*k3 + a64*k4 + a65*k5))
            k7 = f (ti +      h, y + h * (a71*k1 + a72*k2 + a73*k3 + a74*k4 + a75*k5 + a76*k6))
 
            error = abs ( (b1-b1p) * k1 + (b3-b3p) * k3 + (b4-b4p) * k4 +
                          (b5-b5p) * k5 + (b6-b6p) * k6 + (b7-b7p) * k7 )
 
            delta = 0.84 * pow (tol / error, .2)   // error control
            if (error < tol) {
                ti += h
                y  += h * (b1*k1 + b3*k3 + b4*k4 + b5*k5 + b6*k6)
            } // if
 
            if (delta <= .1)       h *= .1
            else if (delta >= 4. ) h *= 4.
            else                   h *= delta
 
            if (h > hmax) h = hmax
 
            if (ti >= t)         return y
            else if (ti + h > t) h = t - ti
            else if (h < hmin)   return y
       } // for
 
       y         // the value of the function at time t, y = f(t)

    } // integrate
 
} // DormandPrince object 

/*:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::*/
/**
 * This object is used to test the DormandPrince object.
 */
object DormandPrinceTest extends Application
{
    import scalation.dynamics.DormandPrince._

    def derv1 (t: Double, y: Double) = t + y
    val y0   = 1.24
    val t    = 1.0
    val hmin = 0.01
    val hmax = 1.0

    //def integrate (f: Derivative, y0: Double, t: Double, hmin: Double, hmax: Double,
    //               t0: Double = 0., tol: Double = 1E-5, maxSteps: Int = 1000): Double =
    println ("\n==> at t = " + t + " y = " + integrate2 (derv1, y0, t, hmin, hmax))

} // DormandPrinceTest object

