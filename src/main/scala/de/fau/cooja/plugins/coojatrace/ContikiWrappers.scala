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

import org.contikios.cooja.contikimote._
import org.contikios.cooja.interfaces._



// speed up compilation
class ContikiWrappers



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

  override lazy val currentProcessDynamic = throw new Exception("Unsupported for ContikiMotes as process name strings are stored in .text section not accessibke to cooja plugins.")
}



/**
 * Helper interface to be added to contiki motes to observe memory changes.
 * 
 * @param mote contiki mote this memory interface belongs to
 */
@ClassDescription("Memory")
class MemoryInterface(mote: Mote) extends MoteInterface with PolledAfterActiveTicks {
  /**
   * WeakHashMap to store update function for variable signals.
   * Does not hold a strong reference to the signal and will remove update function
   * from list when (key) signal is garbage collected.
   */
  private val updates = collection.mutable.WeakHashMap[Var[_], () => Unit]()

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

    // store new signal in a weak reference to use in update function, otherwise
    // update function would prevent garbage collecting of signal!
    val weak = new ref.WeakReference(v)

    // WeakHashMap has synchronization issues with garbage collection...
    updates synchronized {
      // store update function, which calls updateFun and updates signal
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

    // after each tick, call every update function
    updates synchronized {
      val functions = updates.values.toList
      for(fun <- functions) fun() 
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

  /**
   * Mote memory.
   */
  lazy val memory = mote.getMemory.asInstanceOf[AddressMemory]


  def addIntVar(addr: Int) = memoryInterface.addVar(addr, int)
  def addByteVar(addr: Int) = memoryInterface.addVar(addr, byte)
  def addPointerVar(addr: Int) = memoryInterface.addVar(addr, pointer)
  def addArrayVar(addr: Int, length: Int, const: Boolean) = memoryInterface.addVar(addr, array(_, length))


  // return original memory addresses by subtracting offset of reference value
  override def pointer(name: String) = {
    val v = int(name)
    if(v == 0) v else v - int("referenceVar")
  }
  override def pointer(addr: Int) = {
    val v = int(addr)
    if(v == 0) v else v - int("referenceVar") 
  }
}

} // package contikiwrappers

