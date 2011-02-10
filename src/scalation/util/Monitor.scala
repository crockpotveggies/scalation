
/*::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::*/
/**
 * @author  John Miller
 * @version 1.0
 * @date    Mon Sep  7 15:05:06 EDT 2009
 * @see     LICENSE (MIT style license file).
 */

package scalation.util

/*::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::*/
/**
 * The Monitor trait is used to trace the actions/events in the models.
 */
trait Monitor
{
    /** Flag indicating whether tracing is on
     */
    private var tracing = true 

    /*::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::*/
    /**
     * Toggle the tracing flag.
     */
    def toggle () { ! tracing }

    /*::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::*/
    /**
     * Trace an action/event.
     * @param who   who caused the action
     * @param what  what was the action
     * @param whom  whom did the action effect
     * @param when  when was the action taken
     */
    def trace (who: Identity, what: String, whom: Identity, when: Double)
    {
        if (tracing) {
            println (who.me + " " + what + " " + whom.me + " at time " + when + ".")
        } // if
    } // trace

} // Monitor trait

