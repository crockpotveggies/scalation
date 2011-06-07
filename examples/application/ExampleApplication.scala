/* $Id$ */

package application

// This is all we need to import in order to take advantage of the ScalaTion's
// core functionality.
import scalation.ScalaTion

/**
 * Example application that uses ScalaTion! Notice how it extends ScalaTion?
 * That's how all the magic happens.
 * @author Michael Cotterell
 */
object ExampleApplication extends ScalaTion
{
	
	def f(n: Int): Int = n
	
	def section(title: String) {
	    println()
	    println(title.toUpperCase)
	    println(":::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::")
	}
	
	def main(args : Array[String]) : Unit = {
		
	    /*:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::*/
		/* Exponentiation */
	    val exp1 = 2↑2		// 4
	    val exp2 = 2↑2↑2	// 16
	    
	    section("Exponentiation")
	    println("2↑2   = %s".format(exp1))
	    println("2↑2↑2 = %s".format(exp2))
	    
	    /*:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::*/
		/* Roots */
	    val root1 = 4↓2				// 2
	    val root2 = 4↓2↓2			// 1.41421...
	    val test  = 4↑0.5 == 4↓2	// true
	    
	    section("Roots")
	    println("4↓2          = %s".format(root1))
	    println("4↓2↓2        = %s".format(root2))
	    println("4↑0.5 == 4↓2 = %s".format(test))
	    
	    /*:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::*/
		/* Factorials */
	    val fac1 = 4!	// 24
	    val fac2 = 3.5!	// 11.63172...
	    
	    section("Factorials")
	    println("4!   = %s".format(fac1))
	    println("3.5! = %s".format(fac2))
		
	    /*:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::*/
		/* Rising and Falling Factorials */
	    
	    val rising  = 4 ⇑ 4 // 840
	    val falling = 4 ⇓ 4 // 24
	    
	    section("Rising and Falling Factorials")
	    println("4 ⇑ 4 = %s".format(rising))
	    println("4 ⇓ 4 = %s".format(falling))
	}
}
