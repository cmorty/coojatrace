package se.sics.cooja.coojatest



import reactive._

import se.sics.cooja._
import se.sics.cooja.coojatest.wrappers._

import se.sics.cooja.contikimote._
import se.sics.cooja.interfaces._



/**
 * Contiki mote wrappers.
 */
package object contikiwrappers {
  /**
   * Wrap a contiki mote.
   * @param cm contiki mote
   * @return contiki mote wrapper
   */
  implicit def contikiMote2RichMote(cm: ContikiMote) = new ContikiRichMote(cm)
  
  /**
   * Register contiki wrapper conversion in [[RichMote]] object.
   */
  def register() {
    RichMote.conversions ::= { case cm: ContikiMote => contikiMote2RichMote(cm) }
  }
}

package contikiwrappers {

  /**
   * Wrapper for a contiki mote.
   */
  class ContikiRichMote(mote: ContikiMote) extends RichMote(mote) {
    override lazy val memory = new ContikiMoteRichMemory(mote)
  }



  /**
   * Helper interface to be added to contiki motes to observe memory changes.
   * 
   * @param mote contiki mote this memory interface belongs to
   */
  @ClassDescription("Memory")
  class MemoryInterface(mote: Mote) extends MoteInterface with PolledAfterActiveTicks {
    /**
     * List of update callback functions, called after every active tick.
     */
    private var updates = List[() => Unit]()

    /**
     * Create signal for contiki mote memory variable.
     * 
     * @param name variable name
     * @param updateFun function which returns variable value, called at every change
     * @return [[Signal]] of variable value
     * @tparam T type of variable / result type of updateFun
     */
    def addVar[T](name: String, updateFun: (String) => T): Signal[T] = {
      // create new signal, get inital value by evaluating updateFun
      val v = Var[T](updateFun(name))

      // add new function to update signal with updateFun to list of callbacks after tick 
      updates ::= ( () => v.update(updateFun(name)) )

      // return signal
      v
    }

    /**
     * Call all update functions after an active tick to check for memory variable changes.
     */
    def doActionsAfterTick() {
        setChanged()
        notifyObservers(this)
        for(u <- updates) u()
    }

    def getInterfaceVisualizer = null
    def releaseInterfaceVisualizer(panel: javax.swing.JPanel) {}
    def getConfigXML = null
    def setConfigXML(configXML: java.util.Collection[org.jdom.Element], visAvailable: Boolean) {}
  }

  /**
   * Companion object for contiki memory helper interface creation.
   */
  object MemoryInterface {
    /**
     * Create or return existing memory interface.
     * @param mote contiki mote for which to return memory interface
     * @return [[MemoryInterface]] for contiki mote
     */
    def apply(mote: Mote) = {
      // create new interface if not found
      if(mote.getInterfaces.get("MemoryInterface") == null) {
        mote.getInterfaces.addInterface(new MemoryInterface(mote))
      }
      
      // return memory interface
      mote.getInterfaces.get("MemoryInterface").asInstanceOf[MemoryInterface]
    }
  }


  /**
   * Wrapper for a contiki mote memory.
   */
  class ContikiMoteRichMemory(val mote: ContikiMote) extends RichMoteMemory {
    /**
     * Helper memory interface for contiki motes.
     */
    lazy val memoryInterface = MemoryInterface(mote)

    lazy val memory = mote.getMemory.asInstanceOf[AddressMemory]

    def addIntVar(name: String) = memoryInterface.addVar(name, memory.getIntValueOf)
    def addByteVar(name: String) = memoryInterface.addVar(name, memory.getByteValueOf)
    def addArrayVar(name: String, length: Int) =
      memoryInterface.addVar(name, memory.getByteArray(_, length))
  }

}

