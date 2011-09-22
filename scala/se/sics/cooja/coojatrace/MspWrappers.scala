package se.sics.cooja.coojatrace



import reactive._

import se.sics.cooja._
import se.sics.cooja.coojatrace.wrappers._
import se.sics.cooja.coojatrace.memorywrappers._

import se.sics.cooja.mspmote._ 




/**
 * MSP mote wrappers.
 */
package object mspwrappers {
  /**
   * Wrap a MSP mote.
   * @param mm MSP mote
   * @return MSP mote wrapper
   */
  implicit def mspMote2RichMote(mm: MspMote) = new MspRichMote(mm)



  /**
   + Wrap a mote for MSP-specific operations.
   *
   * @param mm mote to wrap, will fail if not subclass of MspMote
   * @return MSP mote-specific wrapper
   */
  implicit def mspMoteOnlyWrap(mm: Mote) = new MspMoteOnlyWrapper(mm.asInstanceOf[MspMote])
  
  /**
   * Register MSP wrapper conversion in [[RichMote]] object.
   */
  def register() {
    RichMote.conversions ::= { case mm: MspMote => mspMote2RichMote(mm) }
  }
}

package mspwrappers {

/**
 * Wrapper for a MSP mote.
 *
 * @param mote MSP mote to wrap
 */
class MspRichMote(mote: MspMote) extends RichMote(mote) {
  override lazy val memory = new MspMoteRichMemory(mote)
  override lazy val cpu = new MspMoteRichCPU(mote)
}



/**
 * Wrapper for MSP mote-specific operations.
 *
 * @param mote MSP mote to wrap
 */
class MspMoteOnlyWrapper(mote: MspMote) {
  /**
   * Add a watchpoint (which will not stop the simulation) to this mote.
   *
   * @param filename source filename in which watchpoint is to be set
   * @param line line in source file at which watchpoint is to be set
   * @param name (optional) name for this watchpoint 
   * @return EventStream which will fire name of watchpoint when it is reached
   */
  def watchpoint(filename: String, line: Int, name: String = null): EventStream[String] = {
    // get file from filename
    val file = mote.getSimulation.getGUI.restorePortablePath(new java.io.File(filename))

    // calculate executable address from file and line
    val addr = mote.getBreakpointsContainer.getExecutableAddressOf(file, line)

    // generate name if not set
    val breakname = if(name != null) null else filename + ":" + line

    // add breakpoint which does not stop simulation
    val bp = mote.getBreakpointsContainer.addBreakpoint(file, line, addr)
    bp.setStopsSimulation(false)

    // create result eventstream
    val es = new EventSource[String]

    // create watchpoint listener
    val listener = new java.awt.event.ActionListener {
      def actionPerformed(e: java.awt.event.ActionEvent) {
        // get last breakpoint
        val bp = mote.getLastWatchpoint.asInstanceOf[mspmote.plugins.MspBreakpoint]

        // check if last checkpoint is "ours"
        if(bp.getExecutableAddress == addr) {
          // yes, fire event
          es fire name
        }
      }
    }

    // add watchpoint listener
    mote.addWatchpointListener(listener)

    // remove watchpoint listener on plugin deactivation
    CoojaTracePlugin.forSim(mote.getSimulation).onCleanUp {
      mote.removeWatchpointListener(listener)
    }

    // return eventstream
    es
  }

  /**
   * Return a stracktrace for this mote.
   *
   * @return stacktrace as output from MSP CLI
   */ 
  def strackTrace = mote.getExecutionDetails
}


/**
 * Wrapper for a MSP mote CPU.
 */
class MspMoteRichCPU(mote: MspMote) extends RichCPU {
  def register(name: String): Signal[Int] = { 
    // get index if register name in reigster array
    val reg = se.sics.mspsim.core.MSP430Constants.REGISTER_NAMES.indexOf(name)

    // create new signal, get initial value
    val v = Var[Int](mote.getCPU.reg(reg))

    // add register write monitor to update signal
    mote.getCPU.setRegisterWriteMonitor(reg, new se.sics.mspsim.core.CPUMonitor() {
      def cpuAction(t: Int, adr: Int, data: Int) {
        v.update(data)
      }
    })

    // return signal
    v
  }

  lazy val stackPointer = register("SP") 
}



/**
 * Wrapper for a MSP mote memory.
 */
class MspMoteRichMemory(val mote: MspMote) extends RichMoteMemory {
  lazy val memory = mote.getMemory.asInstanceOf[MspMoteMemory]
  
  /**
   * Create signal for MSP mote memory variable.
   * 
   * @param addr variable address
   * @oaram updateFun function which returns variable value at given address,
   *   called at every change
   * @oaram convFun function which converts new value from Int to correct type
   * @return [[Signal]] of variable value
   * @tparam T type of variable / result type of updateFun
   */
  private def memVar[T](addr: Int, updateFun: Int => T, convFun: Int => T): Signal[T] = {
    // create new signal, get inital value by evaluating updateFun
    val v = Var[T](updateFun(addr))
    
    // add CPU breakpoint/monitor for variable address
    mote.getCPU.setBreakPoint(addr, new se.sics.mspsim.core.CPUMonitor() {
      def cpuAction(t: Int, adr: Int, data: Int) {
        // ignore everything except writes
        if(t != se.sics.mspsim.core.CPUMonitor.MEMORY_WRITE) return

        // update signal
        // NOTE: this method is called _before_ the actual memory is changed,
        // so we need to take the new value from data and pass it to updateFun
        v.update(convFun(data))
      }
    })
    
    // return signal
    v
  }

  /**
   * Get value of byte variable at address.
   *
   * @param addr address of variable
   * @return byte value of variable
   */
  override def byte(addr: Int) =
    memory.getMemorySegment(addr, 1)(0)

  /**
   * Get value of integer variable at address.
   *
   * @param addr address of variable
   * @return integer value of variable
   */
  override def int(addr: Int) = {
    val bytes = memory.getMemorySegment(addr, 4).map(_ & 0xFF)
    val retVal = (bytes(0) << 8) + bytes(1)
    Integer.reverseBytes(retVal) >> 16
  }


  def addIntVar(addr: Int) = memVar(addr, int, _.toInt)
  def addByteVar(addr: Int) = memVar(addr, byte, _.toByte)

  // TODO
  def addPointerVar(addr: Int) = memVar(addr, pointer, _.toInt)


  /*def addArrayVar(name: String, length: Int) =
    memVar(name, memory.getByteArray(_: String, length))
  }*/
}

} // package mspwrappers
