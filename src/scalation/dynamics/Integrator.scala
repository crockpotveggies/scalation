
/*:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::*/
/**
 * @author  John Miller
 * @version 1.0
 * @date    Sat Jan 30 13:19:22 EST 2010
 * @see     LICENSE (MIT style license file).
 */

package scalation
package dynamics

import scala.math._
import advmath._
import util.Error

/*:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::*/
/**
 * This trait provides a template for writing numerical integrators (e.g., Runge
 * Kutta) to produce trajectories for first-order Ordinary Differential Equations
 * (ODE's).  The ODE is of the form y(t)' = f(t, y) where ' is d/dt.  Its initial
 * condition is y0 = y(0).
 *------------------------------------------------------------------------------
 * If f is a linear function of the form a(t) * y(t) + b(t), then the ODE is
 * linear, if a(t) = a (i.e., a constant) the ODE has constant coefficients and
 * if b(t) = 0 the ODE is homogeneous.  Note this package provides a solver (not
 * an integrator) as an option for linear, constant coefficient, homogeneous,
 * first-order ODE.
 * @see scalation.dynamics.LinearDiffEq.scala
 */
trait Integrator
      extends Error
{
    /** Function type for derivative functions: f (t, y)
     */
    type Derivative = (Double, Double) => Double

    /** The default step size for the t dimension
     */
    protected val defaultStepSize = .01

    /** Estimate of the error in calculating y
     */
    protected var error = 0.

    /*:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::*/
    /**
     * Use numerical integration to compute the trajectory of an unknown, time-
     * dependent function y(t) governed by a first-order ODE of the form y(t)' = f(t, y),
     * i.e., the time derivative of y(t) equals f(t, y).  The derivative function
     * f(t, y) is integrated using a numerical integrator (e.g., Runge-Kutta) to
     * return the value of y(t) at time t.  
     * @param f      the derivative function f(t, y)
     * @param y0     the intial value of the y-function at time t0, y0 = y(t0)
     * @param t      the time value at which to compute y(t)
     * @param t0     the initial time
     * @param _step  the step size
     */
    def integrate (f: Derivative, y0: Double, t: Double,
                   t0: Double = 0., _step: Double = defaultStepSize): Double

    /*:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::*/
    /**
     * Apply the integrate method to each derivative to compute the trajectory of
     * a time-dependent vector function y(t) governed by a system of Ordinary
     * Differential Equations (ODE's) where f(t, y) is an array of derivative functions.
     * @param f      array of derivative functions (f_1(t, y), ..., f_n(t, y))
     * @param y0     the initial value vector, y0 = y(t0)
     * @param t      the time value at which to compute y(t)
     * @param t0     the initial time
     * @param _step  the step size
     */
    def integrateV (f: Array [Derivative], y0: Vec[Double], t: Double,
                    t0: Double = 0., _step: Double = defaultStepSize): Vec[Double] =
    {
        val n = y0.length
        if (n != f.length) {
            flaw ("integrateV", "incompatible dimensions between f and y0")
            null
        } else {
            val y = Vec.ofLength[Double](n)
            for (i <- 0 until n) y(i) = integrate (f(i), y0(i), t, t0, _step)
            y
        } // if
    } // integrateV

    /*:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::*/
    /**
     * Get the error estimate.
     */
    def getError: Double = error

} // Integrator trait

