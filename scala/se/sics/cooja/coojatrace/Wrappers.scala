package se.sics.cooja.coojatrace.wrappers



import reactive._

import java.util.{Observable, Observer}

import se.sics.cooja._
import interfaces._

import se.sics.cooja.coojatrace._
import interfacewrappers._
import memorywrappers._



/**
 * Implicit conversions from original cooja simulation/mote/radiomedium objects 
 * to their rich wrappers.
 */
object Conversions {
  implicit def simToRichSim(s: Simulation) = new RichSimulation(s)
  implicit def moteToRichMote(m: Mote) = RichMote(m)
  implicit def radioMediumToRichRadioMedium(rm: RadioMedium)(implicit sim: Simulation) =
   new RichRadioMedium(rm, sim)
}



/**
 * Rich wrapper for a [[Simulation]].
 */
class RichSimulation(val sim: Simulation) {
  /**
   * Get all motes in the simulation in a map with their ID as keys.
   * @return map with (id -> mote) elements
   */
  def motes = sim.getMotes().map(m => m.getID() -> m).toMap

  /**
   * Get current simulation time.
   * @return current simulation time in microseconds
   */ 
  def time = sim.getSimulationTime

  /**
   * Get the simulations' radiomedium.
   * @return [[RadioMedium]] of this simulation
   */
  def radioMedium = sim.getRadioMedium
}



/**
 * Generic mote CPU wrapper.
 */
trait RichCPU  {
  /**
   * Get the value of a register as a signal.
   * @param name name of register
   * @return [[Signal]] with value of register
   */
  def register(name: String): Signal[Int]

  /**
   * Get the stackpointer as a signal.
   * @return [[Signal]] with stackpointer value.
   */
  def stackPointer: Signal[Int]
}



/**
 * Represents a contiki process.
 *
 * @param name process name
 * @param address memory address of process
 */
case class Process(name: String, address: Int) {
  /**
   * Format process address in hex.
   * @return process address in hexadecimal notation
   */
  def hexAddress = "%X".format(address)
}



/**
 * Generic mote wrapper.
 *
 * @param mote mote to be wrapped
 */
class RichMote(val mote: Mote) extends InterfaceAccessors {
  /**
   * Get mote memory.
   *
   * '''Note:''' this methods always throws a exception but can be overridden in a subclass!
   * @return mote memory (wrapped as [[RichMoteMemory]])
   */
  def memory: RichMoteMemory = throw new Exception("Unsupported for this mote type")
  
  /**
   * Get mote CPU.
   *
   * '''Note:''' this methods always throws a exception but can be overridden in a subclass!
   * @return mote CPU (wrapped as [[RichCPU]])
   */
  def cpu: RichCPU = throw new Exception("Unsupported for this mote type")
  
  /**
   * Get mote variable names and addresses.
   * @return map of (address -> variablename) elements
   */
  lazy val varAddresses = {
    memory.memory.getVariableNames.map {
      name => (memory.memory.getVariableAddress(name), name)
    }.toMap
  }

  /**
   * Get the current contiki process as a signal.
   * @return [[Signal]] of currently running [[Process]]
   */
  lazy val currentProcess = {
    memory.intVar("process_current").map(addr => Process(varAddresses(addr), addr))
  }
}

/**
 * Mote wrapper companion object.
 */
object RichMote {
  /**
   * List of [[Mote]] to [[RichMote]] conversions. This list is filled at runtime with available
   * more specialized richmote subclass conversions.
   */
  protected[coojatrace] var conversions = List[PartialFunction[Mote, RichMote]]()

  /**
   * Default [[Mote]] to [[RichMote]] conversion. Creates a generic [[RichMote]] wrapper.
   */
  protected val defaultConversion: PartialFunction[Mote, RichMote] = { 
    case m: Mote => new RichMote(m)
  }

  /**
   * Map of already wrapped motes to their respective wrapper.
   *
   * '''Note:''' This is important to prevent wrappers and signals to be created multiple 
   * times, which breaks magicsignals and increases overhead and memory usage
   */
  protected val cache = collection.mutable.WeakHashMap[Mote, RichMote]()

  /**
   * Wrap a [[Mote]] in its (most specific) [[RichMote]] by searching the conversions list.
   * @param mote [[Mote]] to be wrapped
   * @return [[RichMote]] wrapper for mote
   */
  def apply(mote: Mote): RichMote = cache.getOrElseUpdate(mote,
    conversions.find(_.isDefinedAt(mote)).getOrElse(defaultConversion).apply(mote)
  )

  /**
   * Clears mote conversion cache.
   */
  def clearCache() {
    conversions = Nil
  }
}



/**
 * Generic observable wrapper.
 */
trait RichObservable {
  /**
   * Create a [[EventSource]] which is updated by evaluating the given function at each
   * observer notification.
   *
   * @param fun function which returns new event to be fired by eventsource, is called at every
   *   change of observed object
   * @return new [[EventSource]] which is updated at every change of observed object
   * @tparam ET result type of `fun` / type of eventsource events 
   */
  def observedEvent[ET](fun: => ET) = {
    // create new eventsource
    val es = new EventSource[ET]()

    // create observer which calls fun and fires result
    createObserver(new Observer() {
      def update(obs: Observable, obj: Object) {
        es fire fun
      }
    })

    // return eventsource
    es
  }
  
  /**
   * Create a [[Signal]] which is updated by evaluating the given function at each
   * observer notification.
   *
   * @param fun function which returns new value for signal, is called at every
   *   change of observed object
   * @return new [[Signal]] which is updated at every change of observed object
   * @tparam ET result type of `fun` / type of signal value 
   */
  def observedSignal[ST](fun: => ST) = {
    // create new signal, get initial value by calling fun
    val signal = Var[ST](fun)

    // create observer which calls fun and sets signal to result
    createObserver(new Observer() {
      def update(obs: Observable, obj: Object) {
        signal() = fun
      }
    })

    // return signal
    signal
  }

  /**
   * Add a new observer to the observed object.
   *
   * @param o observer to add
   */
  protected def addObserver(o: Observer): Unit

  /**
   * Remove an observer from the observed object.
   *
   * @param o observer to remove
   */
  protected def removeObserver(o: Observer): Unit

  /**
   * Simulation this obserable belongs to.
   */
  def simulation: Simulation

  /**
   * Add observer to observable now and registers callback to remove observer at deactivation.
   *
   * @param o observer to add to obserable
   */
  protected def createObserver(o: Observer) {
    // add observer using addFun
    addObserver(o)

    // remove observer when deactivating plugin
    CoojaTracePlugin.forSim(simulation).onCleanUp {
      removeObserver(o)
    }
  }
}



/**
 * Generic mote inferface wrapper.
 *
 * @tparam T type of wrapped interface (subtype of [[MoteInterface]])
 */
trait RichInterface[T <: MoteInterface] extends RichObservable {
  /**
   * the wrapped interface.
   */
  val interface: T
  
  def addObserver(o: Observer) { interface.addObserver(o) }
  def removeObserver(o: Observer) { interface.deleteObserver(o) }
}



/**
 * Generic radio medium wrapper.
 */
class RichRadioMedium(val radioMedium: RadioMedium, val simulation: Simulation)
    extends RichObservable {
  /**
   * Get a list of active radio connections as a signal.
   * @return [[Signal]] of a list of active radio connections
   */
  lazy val connections = observedSignal {
    radioMedium.asInstanceOf[se.sics.cooja.radiomediums.AbstractRadioMedium].getActiveConnections
  }

  // uses different observer functions
  def addObserver(o: Observer) { radioMedium.addRadioMediumObserver(o) }
  def removeObserver(o: Observer) { radioMedium.deleteRadioMediumObserver(o) }
}

