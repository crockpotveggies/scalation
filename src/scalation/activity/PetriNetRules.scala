
/*:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::*/
/**
 * @author  John Miller
 * @version 1.0
 * @date    Mon Oct 12 18:41:26 EDT 2009
 * @see     LICENSE (MIT style license file).
 */

package scalation
package activity

import dynamics.RungeKutta._
import advmath._
import stat._

/*::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::*/
/**
 * This class is used to define firing rules for the PetriNet class.
 * It supports both constant flow and linear flow models of token
 * (integer valued) and fluid (real valued) flow.
 * Typically, in the constant flow model, a base flow Vec is used
 * for the threshold (require at least this number of tokens/amount
 * of fluid) and the flow (move this number this number of tokens/amount
 * of fluid over the arc).  It is also possible to set the flow
 * below the threshold.
 * In the the linear flow model, a base flow Vec can be augmented
 * by additional flow that is a function of the residual left after
 * the base is taken and the time it takes to fire the transition.
 * The total flow may not exceed the the number/amount at the place.
 * Additional flow models are under development.
 */
trait PetriNetRules
{
    /*::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::*/
    /**
     * Return whether the matrix inequality is true: t >= b.
     * The firing threshold should be checked for every incoming arc.
     * If all return true, the transition should fire.
     * @param  t  the token Vec (number of tokens per color)
     * @param  b  the base constant Vec
     */
    def thresholdI (t: VecI, b: VecI): Boolean = t >= b 

    /*::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::*/
    /**
     * Return whether the matrix inequality is true: f >= b.
     * The firing threshold should be checked for every incoming arc.
     * If all return true, the transition should fire.
     * @param  f  The fluid Vec (amount of fluid per color)
     * @param  b  The base constant Vec
     */
    def thresholdD (f: VecD, b: VecD): Boolean = f >= b

    /*::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::*/
    /**
     * Function to compute the delay in firing a transition.
     * The base time is given by a random variate.
     * This is adjusted by weight Vecs multiplying the number of
     * aggregate tokens and the aggreagate amount of fluids summed over
     * all input places: delay = v + w_t * t + w_f * f.
     * @param  v    the random variate used to compute base firing time
     * @param  w_t  the weight for the token Vec
     * @param  t    the aggregate token Vec (summed over all input places)
     * @param  w_f  the weight for the fluid Vec
     * @param  f    the aggregate fluid level Vec (summed over all input places)
     */
    def calcFiringDelay (v: Variate, w_t: VecD, t: VecI, w_f: VecD, f: VecD): Double =
    {
        var delay = v.gen
        if (w_t != null) delay += w_t dot t.toDouble
        if (w_f != null) delay += w_f dot f
        delay
    } // calcFiringTime

    /*::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::*/
    /**
     * Compute the number of tokens to flow over an arc according to the
     * Vec expression: b + r * (t-b) * d.  If d is 0, returns b.
     * Supports linear (w.r.t. time delay) and constant (d == 0) flow models.
     * @param t  the token Vec (number of tokens per color)
     * @param b  the constant Vec for base token flow
     * @param r  the rate Vec (number of tokens per unit time)
     * @param d  the time delay
     */
    def tokenFlow (t: VecI, b: VecI, r: VecI = null, d: Double = 0): VecI =
    {
        t min (if (d == 0 || r == null) b else b + ((r * (t - b)).toDouble * d).toInt)
    } // tokenFlowDouble 

    /*::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::*/
    /**
     * Compute the amount of fluid to flow over an arc according to the
     * matrix expression: b + r * (f-b) * d.  If r is 0, returns b.
     * Supports linear (w.r.t. time delay) and constant (d == 0) flow models.
     * @param f  the fluid Vec (amount of fluid per color)
     * @param b  the constant Vec for base fluid flow
     * @param r  the rate Vec (amounts of fluids per unit time)
     * @param d  the time delay
     */
    def fluidFlow (f: VecD, b: VecD, r: VecD = null, d: Double = 0): VecD =
    {
        f min (if (d == 0 || r == null) b else b + r * (f - b) * d)
    } // fluidFlow

    /*::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::*/
    /**
     * Compute the amount of fluid to flow over an arc according to the
     * system of first-order Ordinary Differential Equation (ODE's):
     * "integral derv from t0 to t".  Supports ODE base flow models.
     * @param f     the fluid Vec (amount of fluid per color)
     * @param derv  the array of derivative functions
     * @param t0    the current time
     * @param d     the time delay
     */
    def fluidFlow (f: VecD, derv: Array [Derivative], t0: Double, d: Double): VecD =
    {
        println ("fluidFlow: f = " + f + " t0 = " + t0 + " t = " + (t0 + d))
        val g = integrateV (derv, f, t0 + d, t0)
        println ("fluidFlow: f = " + f + " g = " + g + " t0 = " + t0 + " t = " + (t0 + d))
        f min (g - f)
    } // fluidFlow

} // PetriNetRules trait

/*::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::*/
/**
 * This object is used to test the PetriNetRules trait.
 */
object PetriNetRulesTest extends Application with PetriNetRules
{
    //:: Set the initial time.

    val t0 = 1.

    //:: Set sample values for tokens.

    val t   = Vec(5, 4)       // tokens
    val r_t = Vec(1, 1)       // rates
    val b_t = Vec(1, 2)       // base requirement
    val w_t = Vec(.01, .01)   // weight

    //:: Set sample values for fluid.

    val f   = Vec(5.5, 4.5)   // fluid levels
    val r_f = Vec(.5, 1.0)    // rates
    val b_f = Vec(1.5, 2.5)   // base requirement
    val w_f = Vec(.01, .01)   // weight

    //:: Test the firing rules.

    println ("\n *** Show initial conditions\n")

    println ("Token Vec t      = " + t)
    println ("Rate Vec r_t     = " + r_t)
    println ("Base token flow b_t = " + b_t)
    println ("Fluid Vec f      = " + f)
    println ("Rate Vec f_t     = " + r_t)
    println ("Base fluid flow b_f = " + b_f)

    println ("\n *** Test token and fluid firing thresholds (t >= b_t)\n")

    println ("Token threshold:  tokens required: " + thresholdI (t, b_t))
    println ("Fluid threshold:  fluids required: " + thresholdD (f, b_f))

    println ("\n *** Test firing firing delay\n")

    val d = calcFiringDelay (Deterministic (1), w_t, t, w_f, f)
    println ("Firing delay: time for transition: " + d)

    println ("\n *** Test token and fluid flows: constant flow model (b)\n")

    println ("Token flow:  place to transition: " + tokenFlow (t, b_t))
    println ("Fluid flow:  place to transition: " + fluidFlow (f, b_f))
    println ("Token flow:  transition to place: " + tokenFlow (t, b_t))
    println ("Fluid flow:  transition to place: " + fluidFlow (f, b_f))

    println ("\n *** Test token and fluid flows: linear flow model (b + r * (t - b) * d\n")

    println ("Token flow:  place to transition: " + tokenFlow (t, b_t, r_t, d))
    println ("Fluid flow:  place to transition: " + fluidFlow (f, b_f, r_f, d))
    println ("Token flow:  transition to place: " + tokenFlow (t, b_t, r_t, d))
    println ("Fluid flow:  transition to place: " + fluidFlow (f, b_f, r_f, d))

    println ("\n *** Test fluid flows: differential flow model integral derv\n")

    def derv1 (t: Double, y: Double) = y
    def derv2 (t: Double, y: Double) = 2. * y
    val dervs = Array [Derivative] (derv1, derv2)
    println ("Fluid flow:  place to transition: " + fluidFlow (f, dervs, t0, d))

} // PetriNetRulesTest

