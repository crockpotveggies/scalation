
/*:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::*/
/**
 * @author  John Miller
 * @version 1.0
 * @date    Mon Feb 22 12:11:17 EST 2010
 * @see     LICENSE (MIT style license file).
 */

package scalation
package advmath

/*:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::*/
/**
 * This class is used to represent complex numbers (a + bi) as (a, b), e.g.,
 * (2.1, 3.2i).  Note: i * i = -1.
 * @param re  the real part
 * @param im  the imaginary part
 */
case class Complex (re: Double, im: Double = 0.)
extends Numeric [Complex] 
    with Fractional [Complex]
    with Ordered [Complex]
{
    /*:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::*/
    /**
     * Compute the unary minus (-).
     */
    def unary_- () = Complex (-re, -im)

    def negate (c: Complex) = -c

    /*:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::*/
    /**
     * Add two complex numbers.
     * @param c  add c to this
     */
    def + (c: Complex) = Complex (re + c.re, im + c.im)

    def plus (c: Complex, d: Complex) = c + d

    /*:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::*/
    /**
     * Substract two complex numbers.
     * @param c  subtract c from this
     */
    def - (c: Complex) = Complex (re - c.re, im - c.im)

    def minus (c: Complex, d: Complex) = c - d

    /*:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::*/
    /**
     * Multiply two complex numbers.
     * @param c  multiply this times c
     */
    def * (c: Complex) = Complex (re * c.re - im * c.im, re * c.im + im * c.re)

    def times (c: Complex, d: Complex) = c * d

    /*:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::*/
    /**
     * Divide two complex numbers.
     * @param c  divide this by c
     */
    def / (c: Complex) = Complex ((re * c.re + im * c.im) / (c.re * c.re + c.im * c.im),
                                  (im * c.re - re * c.im) / (c.re * c.re + c.im * c.im))

    def div (c: Complex, d: Complex) = c / d

    /*:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::*/
    /**
     * Compute the complex conjugate: if z = (a + bi) then z.bar = (a - bi).
     */
    def bar = Complex (re, -im)

    /*:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::*/
    /**
     * Determine whether the complex number is real (no imaginary part).
     */
    def isReal = im == 0

    /*:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::*/
    /**
     * Compare two complex numbers (negative for <, zero for ==, positive for >).
     * @param c  the first complex number to compare
     * @param d  the second complex number to compare
     */
    def compare (c: Complex, d: Complex) =
    {
        if (c.re == d.re) c.im compare d.im else c.re compare d.re
    } // compare

    /*:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::*/
    /**
     * Compare this complex number with that complex number d.
     * @param d  that complex number
     */	
    def compare (d: Complex) =
    {	
        if (re == d.re) im compare d.im else re compare d.re
    } // compare

    /*:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::*/
    /**
     * Convert the complex number to a Double.
     * @param c  the complex number to convert
     */
    def toDouble (c: Complex) = re

    /*:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::*/
    /**
     * Convert the complex number to a Float.
     * @param c  the complex number to convert
     */
    def toFloat (c: Complex) = re.asInstanceOf [Float]

    /*:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::*/
    /**
     * Convert the complex number to a Long.
     * @param c  the complex number to convert
     */
    def toLong (c: Complex) = re.asInstanceOf [Long]

    /*:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::*/
    /**
     * Convert the complex number to an Int.
     * @param c  the complex number to convert
     */
    def toInt (c: Complex) = re.asInstanceOf [Int]

    /*:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::*/
    /**
     * Create a complex number from an Int.
     * @param n  the integer used to create the complex number.
     */
    def fromInt (n: Int) = Complex (n, 0.)

    /*:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::*/
    /**
     * Convert this complex number to a String.
     */
    override def toString = "Complex ( " + re + " , " + im + "i )"

} // Complex class

/*:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::*/
/**
 * This object is used to test the Complex class.
 */
object ComplexTest extends Application
{

    val c = Complex (2., 3.)
    val d = Complex (4., 5.)
    println ("c = " + c)
    println ("d = " + d)
    println ("c + d = " + (c + d))
    println ("c - d = " + (c - d))
    println ("c * d = " + (c * d))
    println ("c / d = " + (c / d))

    //val v = Vec(c, d) 
    //println ("v = " + v)
    //val u = Vec(2)
    //println ("u = " + u)

    //val cm = new MatrixN [Complex] (2, Complex (1.), Complex (0.))    // 2 by 2 identity matrix
    //println ("cm = " + cm)

} // ComplexTest

