/*
 * Copyright (c) 2011, Florian Lukas
 * All rights reserved.
 * 
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions are met:
 * 1. Redistributions of source code must retain the above copyright notice,
 * this list of conditions and the following disclaimer. 2. Redistributions in
 * binary form must reproduce the above copyright notice, this list of
 * conditions and the following disclaimer in the documentation and/or other
 * materials provided with the distribution.
 *
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" 
 * AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE 
 * IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE 
 * ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE 
 * LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
 * CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF
 * SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS 
 * INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN 
 * CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) 
 * ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
 * POSSIBILITY OF SUCH DAMAGE.
 */

package de.fau.cooja.plugins.coojatrace



import reactive._

import org.contikios.cooja._
import de.fau.cooja.plugins.coojatrace.wrappers._
import de.fau.cooja.plugins.coojatrace.memorywrappers._


import org.contikios.cooja.mspmote._ 
import java.io.File

import se.sics.mspsim.core.MemoryMonitor
import se.sics.mspsim.core.Memory
import se.sics.mspsim.core.RegisterMonitor



// speed up compilation
class MspWrappers



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
    val file = mote.getSimulation.getCooja.restorePortablePath(new java.io.File(filename))

    // calculate executable address from file and line
    val addr = mote.getExecutableAddressOf(file, line)

    // generate name if not set
    val breakname = if(name != null) null else filename + ":" + line

    // add breakpoint which does not stop simulation
    val bp = mote.addBreakpoint(file, line, addr)
    bp.setStopsSimulation(false)

    // create result eventstream
    val es = new EventSource[String]

    // create watchpoint listener
    val monitor = new MemoryMonitor.Adapter {

      override
      def  notifyReadAfter(addr: Int,  mode: Memory.AccessMode , typ: Memory.AccessType){
        es fire name
      }
      
      
    }

    // add watchpoint listener
    val ffile = new File(filename)
    mote.getCPU.addWatchPoint(mote.getExecutableAddressOf(ffile, line), monitor)

    // remove watchpoint listener on plugin deactivation
    CoojaTracePlugin.forSim(mote.getSimulation).onCleanUp {
      mote.getCPU.removeWatchPoint(mote.getExecutableAddressOf(ffile, line), monitor)
    }

    // return eventstream
    es
  }

  /**
   * Signal of CPU (power) mode.
   * Possible modes: "active", "lpm0" .. "lpm4"
   *
   * @return Signal[String] of mode
   */
  def cpuMode = {
    // create result signal
    val signal = Var(se.sics.mspsim.core.MSP430Constants.MODE_NAMES(mote.getCPU.getMode))

    // create mode listener
    val listener = new se.sics.mspsim.core.OperatingModeListener {
      def modeChanged(source: se.sics.mspsim.core.Chip, newMode: Int) {
        signal.update(se.sics.mspsim.core.MSP430Constants.MODE_NAMES(newMode)) 
      }
    }

    // add mode listener
    mote.getCPU.addOperatingModeListener(listener)

    // remove watchpoint listener on plugin deactivation
    CoojaTracePlugin.forSim(mote.getSimulation).onCleanUp {
      mote.getCPU.removeOperatingModeListener(listener)
    }

    // return signal
    signal
  }

  
   def mspMote: MspMote = mote
  
  /**
   * Return a stacktrace for this mote.
   *
   * @return stacktrace as output from MSP CLI
   */ 
  def stackTrace = mote.getExecutionDetails
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
    val rwm = new se.sics.mspsim.core.RegisterMonitor.Adapter() {
      override
      def notifyWriteAfter(reg: Int, data: Int, mode: Memory.AccessMode) {
        v.update(data)
      }
    }
    
    mote.getCPU.addRegisterWriteMonitor(reg, rwm)

    // remove monitor on plugin deactivation
    CoojaTracePlugin.forSim(mote.getSimulation).onCleanUp {
      mote.getCPU.removeRegisterWriteMonitor(reg, rwm)
    }

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
   * @param size (optional) size of variable in bytes, neccessary to update on writes
   *  on parts of multi-byte variables
   * @return [[Signal]] of variable value
   * @tparam T type of variable / result type of updateFun
   */
  private def memVar[T](addr: Int, updateFun: Int => T, size: Int = 1): Signal[T] = {
    // create new signal, get inital value by evaluating updateFun
    val v = Var[T](updateFun(addr))
    
    // monitor for variable addresses
    val cpuMonitor = new MemoryMonitor.Adapter {

      override
      def  notifyWriteAfter(dstAddress: Int, data: Int, mode: Memory.AccessMode) {
        v.update(updateFun(addr))
      }
    }

    // add CPU breakpoint/monior for all addresses
    for(a <- addr until addr+size) mote.getCPU.addWatchPoint(a, cpuMonitor)

    // remove breakpoint on plugin deactivation
    CoojaTracePlugin.forSim(mote.getSimulation).onCleanUp {
      for(a <- addr until addr+size) mote.getCPU.removeWatchPoint(a, cpuMonitor)
      // TODO: remove remaining PC monitors? (very unlikely to be left)
    }
    
    // return signal
    v
  }


  override def byte(addr: Int) = memory.getMemorySegment(addr, 1)(0)

  override def int(addr: Int) = {
    val bytes = memory.getMemorySegment(addr, 2).map(_ & 0xFF)
    (bytes(1) << 8) + bytes(0)
  }

  override def array(addr: Int, length: Int) = {
    memory.getMemorySegment(addr, length)
  } 


  def addIntVar(addr: Int) = memVar(addr, int, 2)
  def addByteVar(addr: Int) = memVar(addr, byte, 1)
  def addPointerVar(addr: Int) = memVar(addr, pointer, 2)

  def addArrayVar(addr: Int, length: Int, const: Boolean) = if(const == true) {
    memVar(addr, _ => array(addr, length), 1)
  } else {
    memVar(addr, _ => array(addr, length), length)
  }
}

} // package mspwrappers
