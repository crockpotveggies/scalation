
/**:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
 * @author  John Miller
 * @version 1.0
 * @date    Mon Sep  7 15:05:06 EDT 2009
 * @see     LICENSE (MIT style license file).
 */

package scalation.process

import scalation.animation._
import scalation.animation.CommandType._
import scalation.advmath._
import scalation.stat._
import scalation.scala2d._
import scalation.scala2d.Colors._
import scalation.util.Monitor

/**:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
 * The Resource class provides services to entities (SimActors).
 * It may or may not have an associated waiting queue.
 * @param name         the name of the resource
 * @param line         the line/queue where entities wait
 * @param units        the number of service units (e.g., bank tellers)
 * @param serviceTime  the service time distribution
 * @param at           the location of the resource
 */
class Resource (name: String, line: WaitQueue, units: Int, serviceTime: Variate, at: Array [Double])
               extends Component with Monitor
{
    {
        setName (name)
        setAt (at)
    } // primary constructor

    /** Number of service units of this Resource currently in use
     */
    private var inUse = 0

    /**:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
     * Tell the animation engine to display this Resource.
     */
    def display ()
    {
        director.animate (this, CreateNode, orange, Rectangle (), at)
    } // display

    /**:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
     * Determine whether the Resource is busy (no units available).
     */
    def busy = inUse == units

    /**:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
     * Utilize the Resource for a period of time (models an activity).
     */
    def utilize ()
    {
        if (busy) flaw ("utilize", "no units available")
        val actor    = director.theActor
        val duration = serviceTime.gen
        tally (duration)
        trace (this, "serves for " + duration, actor, actor.time)
        director.animate (actor, MoveToken, null, null, 
                 Array (at(0) + DIAM, at(1) + at(3) / 2. - RAD))
        inUse += 1
        actor.schedule (duration)
        actor.yieldToDirector ()
    } // utilize

    /**:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
     * Release the Resource after service is finished (also check waiting queue).
     */
    def release ()
    {
        val actor = director.theActor
        trace (this, "releases", actor, actor.time)
        if (line != null && ! line.isEmpty) {
            val waitingActor = line.dequeue ()
            waitingActor.schedule (0.0)
        } // if
        inUse -= 1
    } // release

} // Resource

