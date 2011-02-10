
/*::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::*/
/**
 * @author  John Miller
 * @version 1.0
 * @date    Mon Sep  7 15:05:06 EDT 2009
 * @see     LICENSE (MIT style license file).
 */

package scalation.process

import scala.collection.mutable._

import scalation.animation._
import scalation.animation.CommandType._
import scalation.scala2d._
import scalation.scala2d.Colors._
import scalation.util.Monitor

/*::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::*/
/**
 * The WaitQueue class simply a wrapper for monitoring scala's Queue class.
 */
class WaitQueue (name: String, at: Array [Double])
      extends Queue [SimActor] with Component with Monitor
{
    setName (name)
    setAt (at)

    /*::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::*/
    /**
     * Tell the animation queue to display this WaitQueue.
     */
    def display ()
    {
        director.animate (this, CreateNode, yellow, Rectangle (), at)
    } // display

    /*::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::*/
    /**
     * Wait in the queue.
     */
    def waitIn ()
    {
        val actor = director.theActor
        val timeIn = actor.time
        trace (this, "wait begins", actor, timeIn)
        enqueue (actor)
        actor.yieldToDirector ()
        val timeOut = director.clock
        tally (timeOut - timeIn)
        trace (this, "wait over", actor, timeOut)
    } // waitIn

} // WaitQueue class

