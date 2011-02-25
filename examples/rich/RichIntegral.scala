/* $Id$ */

package rich

import scalation.ScalaTion

/**
 * Example of how to take advantage of RichIntegral
 * @author Michael Cotterell
 */
object RichIntegral extends ScalaTion
{
	private def printSection(title: String)
	{
		println
		for (i <- 1 to 80) print(":"); println
		println(title.toUpperCase)
		for (i <- 1 to 80) print(":"); println
		println
	} 
	
	def main(args : Array[String])
	{
	
		println("Example of how to take advantage of RichIntegral")
		
		printSection("Numeric factorial")
		
		println("3! = " + (3!))
		println("13! = " + (13!))
		
		// more examples to come
		
	}
}
