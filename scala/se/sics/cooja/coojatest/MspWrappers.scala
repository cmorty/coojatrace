package se.sics.cooja.coojatest

import reactive._

import se.sics.cooja._
import se.sics.cooja.coojatest.wrappers._

import se.sics.cooja.mspmote._ 

package object mspwrappers {
  implicit def mspMoteToMspRichMote(m: Mote) = m match {
    case mm: MspMote => new MspRichMote(mm)
  }
}

package mspwrappers {
  class MspRichMote(mote: MspMote) extends RichMote(mote) {
    lazy val memory = new MspMoteRichMemory(mote)
    lazy val cpu = new MspMoteRichCPU(mote)
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
  
    lazy val stackptr = register("SP") 
  }
  
  
  
  class MspMoteRichMemory(val mote: MspMote) extends RichMoteMemory {
    lazy val memory = mote.getMemory.asInstanceOf[MspMoteMemory]
    
    private def memVar[T](name: String, updateFun: () => T) = {
      if(!memory.variableExists(name)) {
        throw new Exception // TODO
      }
      
      val v = Var[T](updateFun())
      
      mote.getCPU.setBreakPoint(memory.getVariableAddress(name), new   se.sics.mspsim.core.CPUMonitor() {
        def cpuAction(t: Int, adr: Int, data: Int) {
          if(t != se.sics.mspsim.core.CPUMonitor.MEMORY_WRITE) return
          v.update(updateFun())
        }
      })
      
      v
    }
    
    def intVar(name: String): Signal[Int] = memVar[Int](name, () => memory.getIntValueOf(name))
    def byteVar(name: String): Signal[Byte] = memVar[Byte](name, () => memory.getByteValueOf(  name))
    //def array(name: String, length: Int) = 
  }
}
