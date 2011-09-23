package se.sics.cooja.coojatrace



import reactive._

import se.sics.cooja._
import se.sics.cooja.coojatrace.wrappers._
import se.sics.cooja.coojatrace.memorywrappers._

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
  // TODO DOC
  private val updates = collection.mutable.WeakHashMap[Var[_], () => Unit]()


  val logger = org.apache.log4j.Logger.getLogger(this.getClass)  // DEBUG 

  /**
   * Create signal for contiki mote memory variable.
   * 
   * @param addr variable address
   * @param updateFun function which returns variable value at given address,
   *   called at every change
   * @return [[Signal]] of variable value
   * @tparam T type of variable / result type of updateFun
   */
  def addVar[T](addr: Int, updateFun: (Int) => T): Signal[T] = {
    // create new signal, get inital value by evaluating updateFun
    val v = Var[T](updateFun(addr))

    logger.debug("+++ ContikiMote: added var @ " + addr + " = " + v.now) // DEBUG

    // TODO DOC
    val weak = new ref.WeakReference(v)

    // TODO DOC
    updates synchronized {
      updates(v) = { () => weak.get.map(_.update(updateFun(addr))) }
    }

    // return signal
    v
  }

  /**
   * Call all update functions after an active tick to check for memory variable changes.
   */
  def doActionsAfterTick() {
      setChanged()
      notifyObservers(this)

      logger.debug("!! ContikiMote " + mote + " ticked: calling " + updates.size + " funs") // DEBUG

      // TODO DOC
      updates synchronized {
        for(fun <- updates.values.toSeq) fun()  
      }
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

  def addIntVar(addr: Int) = memoryInterface.addVar(addr, int)
  def addByteVar(addr: Int) = memoryInterface.addVar(addr, byte)

  // TODO
  override def pointer(name: String) = int(name) - int("referenceVar")
  override def pointer(addr: Int) = int(addr) - int("referenceVar")

  // TODO
  def addPointerVar(addr: Int) = memoryInterface.addVar(addr, pointer)
  def addArrayVar(addr: Int, length: Int) =
    memoryInterface.addVar(addr, array(_, length))
}

} // package contikiwrappers

