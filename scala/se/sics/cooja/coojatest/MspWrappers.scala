package se.sics.cooja.coojatest

import reactive._

import se.sics.cooja._
import se.sics.cooja.coojatest.wrappers._

import se.sics.cooja.mspmote._ 

package object mspwrappers {
  implicit def mspMote2RichMote(mm: MspMote) = new MspRichMote(mm)
    
  def register() {
    RichMote.conversions ::= { case mm: MspMote => mspMote2RichMote(mm) }
  }
}

package mspwrappers {
  class MspRichMote(mote: MspMote) extends RichMote(mote) {
    override lazy val memory = new MspMoteRichMemory(mote)
    override lazy val cpu = new MspMoteRichCPU(mote)
  }
  
  
  
  class MspMoteRichCPU(mote: MspMote) extends RichCPU {
    def register(name: String): Signal[Int] = { 
      val reg = se.sics.mspsim.core.MSP430Constants.REGISTER_NAMES.indexOf(name)
      val v = Var[Int](mote.getCPU.reg(reg))
      mote.getCPU.setRegisterWriteMonitor(reg, new se.sics.mspsim.core.CPUMonitor() {
        def cpuAction(t: Int, adr: Int, data: Int) {
          v.update(data)
        }
      })
      v
    }
  
    lazy val stackPointer = register("SP") 
  }
  
  
  
  class MspMoteRichMemory(val mote: MspMote) extends RichMoteMemory {
    lazy val memory = mote.getMemory.asInstanceOf[MspMoteMemory]
    
    private def memVar[T](name: String, updateFun: String => T): Signal[T] = {
      if(!memory.variableExists(name)) {
        throw new Exception("Variable " + name + "not found in " + mote)
      }
      
      val v = Var[T](updateFun(name))
      
      mote.getCPU.setBreakPoint(memory.getVariableAddress(name), new   se.sics.mspsim.core.CPUMonitor() {
        def cpuAction(t: Int, adr: Int, data: Int) {
          if(t != se.sics.mspsim.core.CPUMonitor.MEMORY_WRITE) return
          v.update(updateFun(name))
        }
      })
      
      v
    }

    def addIntVar(name: String) = memVar(name, memory.getIntValueOf)
    def addByteVar(name: String) = memVar(name, memory.getByteValueOf)
    def addArrayVar(name: String, length: Int) =
      memVar(name, memory.getByteArray(_, length))
  }
}
