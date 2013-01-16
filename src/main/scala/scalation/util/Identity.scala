
//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  John Miller
 *  @version 1.0
 *  @date    Tue Sep  15 15:05:06 EDT 2009
 *  @see     LICENSE (MIT style license file).
 */

package scalation.util

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The Identity trait provides unique identification for simulation componenets,
 *  entities and events.  Includes a mandatory id and an optional name.
 */
trait Identity extends Error
{
    /** The globally unique identifier
     */
    private val _id = Counter.next ()

    /** The given name (assigned once)
     */
    private var _name = ""

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Get the id (unique identifier).
     */
    def id = _id

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Get the name.
     */
    def name = _name

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Set the name.
     *  @param label  the name to assign
     */
    def setName (label: String)
    {
        if (_name == "" && label != null) {
            _name = label
        } else {
            flaw ("setName", "name may only be set once")
        } // if
    } // setName

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Get the type of the simulation object.
     */
    def simType = getClass.getSimpleName ()

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Return the full identity.
     */
    def me = simType + "." + _name + "." + _id

} // Identity trait


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** This Counter object is used to generate unique identifiers.
 */
object Counter
{
    /** Used for counter
     */
    private val i = Count () 

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Get the next value from the counter.
     */
    def next () = i++

} // Counter object

