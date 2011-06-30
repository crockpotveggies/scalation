package application

import scalation.ScalaTion

object VecApp extends App with ScalaTion {
    
    val v = ()
    
    println(v)
    
    
    import scala.collection.mutable.ListMap
    
    val m = ListMap.empty[Int, Double]
    
    m(0) = 5.0
    m(5) = 6.0
    
    println(m)
    
    val z = 0
    
    println(z)
    
}
