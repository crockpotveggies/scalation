/**:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
 * @author  John Miller
 * @version 1.0
 * @date    Mon Nov  2 15:05:06 EDT 2009
 * @see     LICENSE (MIT style license file).
 */
package process

import scalation.process._
import scalation.stat._
import scalation.advmath._

/**:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
 * This object defines a particular scenario under which to execute the bank model.
 * @see scalation.process.ModelTest for another example of test code.
 */
object Bank extends App
{
    new BankModel ("bank", 100, Uniform (4000, 6000), 2, Uniform (9000, 11000), Uniform (900, 1100))
} // Bank

/**:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
 * This class defines a simple process-interaction model of a bank where service
 * is provided by one or more tellers.
 * @param name        the name of the bank model
 * @param nArrivals   the number of arrivals to generate (stopping condition)
 * @param iArrivalRV  the inter-arrival time distribution
 * @param nUnits      the number of service units (tellers)
 * @param serviceRV   the service time distribution
 * @param moveRV      the time distribution for motion along transports
 */
class BankModel (name: String, nArrivals: Int, iArrivalRV: Variate,
                 nUnits: Int, serviceRV: Variate, moveRV: Variate)
      extends Model (name)
{
    val entry         = new Source ("entry", this, Customer, nArrivals, iArrivalRV, Array (100, 100, 30, 30))
    val tellerQ       = new WaitQueue ("tellerQ", Array (210, 100, 80, 30))
    val teller        = new Resource ("teller", tellerQ, nUnits, serviceRV, Array (290, 100, 30, 30))
    val door          = new Sink ("door", Array (400, 100, 30, 30))
    val entry2tellerQ = new Transport ("entry2tellerQ", moveRV, entry, tellerQ)
    val teller2door   = new Transport ("teller2door", moveRV, teller, door)

    addComponents (List (entry, tellerQ, teller, door, entry2tellerQ, teller2door))

    case class Customer () extends SimActor ("c", this)
    {
        def act ()
        {
            entry2tellerQ.move ()
            if (teller.busy) tellerQ.waitIn ()
            teller.utilize ()
            teller.release ()
            teller2door.move ()
            door.leave ()
        } // act

    } // Customer

    startSim ()

} // BankModel class

