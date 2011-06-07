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
	
	def main(args : Array[String]) : Unit = {
		
		val a = Vec(0.99985, 0.00015)
        val u = Vec(1.0078250, 2.0141018)
        
        val i = Vec(1.0, 1.0)
 
        val m = i dot u
        
        println(m)
		
	}
}
