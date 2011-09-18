package se.sics.cooja.coojatest



import reactive._

import se.sics.cooja._
import se.sics.cooja.coojatest.wrappers._
import se.sics.cooja.coojatest.memorywrappers._

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
   * Register MSP wrapper conversion in [[RichMote]] object.
   */
  def register() {
    RichMote.conversions ::= { case mm: MspMote => mspMote2RichMote(mm) }
  }
}

package mspwrappers {

  /**
   * Wrapper for a MSP mote.
   */
  class MspRichMote(mote: MspMote) extends RichMote(mote) {
    override lazy val memory = new MspMoteRichMemory(mote)
    override lazy val cpu = new MspMoteRichCPU(mote)
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
}
