package se.sics.cooja.coojatest



import reactive._

import se.sics.cooja._
import se.sics.cooja.coojatest.wrappers._

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
     * @param name variable name
     * @oaram updateFun function which returns variable value, called at every change
     * @return [[Signal]] of variable value
     * @tparam T type of variable / result type of updateFun
     */
    private def memVar[T](name: String, updateFun: String => T): Signal[T] = {
      // throw exception if variable name not found
      if(!memory.variableExists(name)) {
        throw new Exception("Variable " + name + "not found in " + mote)
      }
      
      // create new signal, get inital value by evaluating updateFun
      val v = Var[T](updateFun(name))
      
      // add CPU breakpoint/monitor for variable address
      mote.getCPU.setBreakPoint(memory.getVariableAddress(name), new   se.sics.mspsim.core.CPUMonitor() {
        def cpuAction(t: Int, adr: Int, data: Int) {
          // ignore everything except writes
          if(t != se.sics.mspsim.core.CPUMonitor.MEMORY_WRITE) return

          // update signal
          v.update(updateFun(name))
        }
      })
      
      // return signal
      v
    }

    def addIntVar(name: String) = memVar(name, memory.getIntValueOf)
    def addByteVar(name: String) = memVar(name, memory.getByteValueOf)
    def addArrayVar(name: String, length: Int) =
      memVar(name, memory.getByteArray(_, length))
  }
  
}
