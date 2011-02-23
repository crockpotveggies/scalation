package rich

import scalation.ScalaTion

/**
 * Example of how to take advantage of RichSet
 * @author Michael Cotterell
 */
object RichSet extends ScalaTion
{
	def printSection(title: String)
	{
		println
		for (i <- 1 to 80) print(":"); println
		println(title.toUpperCase)
		for (i <- 1 to 80) print(":"); println
		println
	}
	
	def main(args : Array[String]) 
	{
		
		println("Example of how to take advantage of RichSet.")
		
		printSection("Define sets")
		
		val ab = Set("a", "b")
		val ac = Set("a", "c")
		val bc = Set("b", "c")
		
		println("ab = " + ab)
		println("ac = " + ac)
		println("bc = " + bc)
		
		printSection("Contains")
		
		println("ab ∋ a = " + ab ∋ "a")
		println("ab ∋ b = " + ab ∋ "b")
		println("ab ∋ c = " + ab ∋ "c")
		
		println("ab ∌ a = " + ab ∌ "a")
		println("ab ∌ b = " + ab ∌ "b")
		println("ab ∌ c = " + ab ∌ "c")
		
		// more examples to come
		
	}
}
