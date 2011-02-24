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
		val x  = Set(1, 2, 3, 4)
		
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
		
		printSection("union")
		
		println("ab ∪ bc = " + ab ∪ bc)
		println("ab ∪ ac = " + ab ∪ ac)
		println("ac ∪ bc = " + ac ∪ bc)
		
		printSection("intersection")
		
		println("ab ∩ ab = " + ab ∩ ab)
		println("ab ∩ bc = " + ab ∩ bc)
		println("ab ∩ ac = " + ab ∩ ac)
		
		printSection("quantifiers")
		
		println("x ∀ (_ < 10) = " + x ∀ (_ < 10))
		println("x ∀ (_ > 10) = " + x ∀ (_ > 10))
		println("ab ∀ (_ == \"a\") == " + ab ∀ (_ == "a"))
		println("ab ∀ (_.isInstanceOf[String]) == " + ab ∀ (_.isInstanceOf[String]))
		
		// more examples to come
		
	}
}
