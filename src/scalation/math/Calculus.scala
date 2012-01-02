
//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  John Miller
 *  @version 1.0
 *  @date    Wed Aug 24 19:53:22 EDT 2011
 *  @see     LICENSE (MIT style license file).
 */

package scalation.math

import scalation.math.Vectors._

//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** This object contains function for computing derivatives.
 */
object Calculus
{
    private val h = 1E-10     // step size used for estimating derivatives

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Estimate the derivative of the scalar-to-scalar function f at point x.
     *  @param f  the function whose derivative is sought
     *  @param x  the point (scalar) at which to estimate the derivative
     */
    def derivative (f: FunctionS2S [Double], x: Double): Double =
    {
        (f(x + h) - f(x)) / h
    } // derivative

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Estimate the gradient of the vector-to-scalar function f at point x
     *  returning a value for the partial derivative for each dimension of x.
     *  @param f  the function whose gradient is sought
     *  @param x  the point (vector) at which to estimate the gradient
     */
    def gradient (f: FunctionV2S [Double], x: VectorD): VectorD =
    {
        for (i <- 0 until x.dim) yield (f(x + (h, i)) - f(x)) / h
    } // gradient

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Compute the gradient of the vector-to-scalar function f using partial
     *  derivative functions evaluated at point x.  Return a value for the
     *  partial derivative for each dimension of the vector x.
     *  @param d  the array of partial derivative functions
     *  @param x  the point (vector) at which to compute the gradient
     */
    def gradientD (d: Array [FunctionV2S [Double]], x: VectorD): VectorD =
    {
        for (i <- 0 until x.dim) yield d(i)(x)
    } // gradientD

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Compute the slope of the vector-to-scalar function f defined on mixed
     *  real/integer vectors.
     *  @param f  the function whose slope is sought
     *  @param x  the point (vector) at which to estimate the slope
     *  @param n  the number of dimensions that are real-valued (rest are integers)
     */
    def slope (f: FunctionV2S [Double], x: VectorD, n: Int = 0): VectorD =
    {
        for (i <- 0 until x.dim) yield
            if (i < n) (f(x + (h, i)) - f(x)) / h    // derivative
            else       (f(x + (1, i)) - f(x))        // difference
    } // slope

} // Calculus object


//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** This object is used to test the Calculus object.
 */
object CalculusTest extends App
{
    import Calculus._

    def g (y: Double): Double = 2. * (y - 3.) * (y - 3.)

    var y = 0.
    println ("derivative  g(" + y + ") = " + derivative (g, y))
    y = 1.
    println ("derivative  g(" + y + ") = " + derivative (g, y))

    def f (x: VectorD): Double = 2. * (x(0) - 3.) * (x(0) - 3.) + (x(1) - 4.) * (x(1) - 4.)
    def df_dx0 (x: VectorD): Double = 4. * x(0) - 12.
    def df_dx1 (x: VectorD): Double = 2. * x(1) - 8.
    val df = Array [FunctionV2S [Double]] (df_dx0, df_dx1)

    var x = new VectorD (0., 0.)
    println ("gradient  f(" + x + ") = " + gradient (f, x))
    println ("gradientD f(" + x + ") = " + gradientD (df, x))
    x = new VectorD (1., 1.)
    println ("gradient  f(" + x + ") = " + gradient (f, x))
    println ("gradientD f(" + x + ") = " + gradientD (df, x))

} // CalculusTest object

