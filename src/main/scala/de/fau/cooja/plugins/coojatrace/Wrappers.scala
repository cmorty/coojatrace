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

import java.util.{Observable, Observer}

import org.contikios.cooja._
import interfaces._

import de.fau.cooja.plugins.coojatrace._
import interfacewrappers._
import memorywrappers._



// speed up compilation
class Wrappers



/**
 * Implicit conversions from original cooja simulation/mote/radiomedium objects 
 * to their rich wrappers.
 */
package object wrappers {
  implicit val simWrapper = RichSimulation.wrap
  implicit val moteWrapper = RichMote.wrap
  implicit def radioMediumWrapper(rm: RadioMedium)(implicit sim: Simulation) = 
    RichRadioMedium.wrap(rm)
}


package wrappers {
  
/**
 * Rich wrapper for a [[Simulation]].
 */
class RichSimulation(val simulation: Simulation) extends RichObservable {
  /**
   * All motes in the simulation in a map with their ID as keys.
   * @return map with (id -> mote) elements
   */
  def motes = simulation.getMotes.map(m => m.getID -> m).toMap

  /**
   * Stream of newly added simulation motes.
   * @return EventStream of  added motes as (id -> mote) tuples
   */
  lazy val newMotes = {
    val es = new EventSource[Mote]

    val listener = new SimEventCentral.MoteCountListener {
      def moteWasAdded(m: Mote) {
        es fire m
      }
      def moteWasRemoved(m: Mote) {}
    }

    simulation.getEventCentral.addMoteCountListener(listener)

    CoojaTracePlugin.forSim(simulation).onCleanUp {
      simulation.getEventCentral.removeMoteCountListener(listener)
    }

    es
  }

  /**
   * Utility function for using one for comprehension for both motes and newMotes.
   * '''Usage:''' `sim.allMotes.foreach {...}` or `for((id, mote) <- sim.allMotes) {...}`
   * @return object that can be used in a for comprehension or foreach call
   */
  def allMotes(implicit obs: Observing) = new {
    def foreach(f: Mote => Unit) {
      motes.values.foreach(f)
      newMotes.foreach(f)
    }
  }

  /**
   * Simulation radiomedium.
   * @return [[RadioMedium]] of this simulation
   */
  def radioMedium = simulation.getRadioMedium

  /**
   * Simulation mote relations.
   */
  lazy val moteRelations = new RichMoteRelations(simulation).relations

  /**
   * Simulation log.
   */
  lazy val log = new RichLog(simulation).messages

  /**
   * Current simulation time.
   * @return current simulation time in microseconds
   */ 
  def time = simulation.getSimulationTime

  /**
   * Current simulation time (in milliseconds) signal.
   */
  lazy val milliSeconds: Signal[Long] = observedSignal { time / 1000 }
  protected def addObserver(o: Observer) { simulation.addMillisecondObserver(o) }
  protected def removeObserver(o: Observer) { simulation.deleteMillisecondObserver(o) }
}

object RichSimulation {
  implicit val wrap: Simulation => RichSimulation = new RichSimulation(_)
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
   * Get the current contiki process as a signal. The process name is read statically from the 
   * map file.
   * @return [[Signal]] of currently running [[Process]]
   */
  lazy val currentProcess = {
    memory.variable("process_current", CPointer).map(
      addr => Process(mote.memory.varAddresses.getOrElse(addr, "") ,addr)
    )
  }

  /**
   * Get the current contiki process as a signal. The process name is read dynamically from the
   * process structure in mote memory. Does not work when compiled with PROCESS_CONF_NO_PROCESS_NAMES.
   * @return [[Signal]] of currently running [[Process]]
   */
  lazy val currentProcessDynamic = {
    val processPtr = memory.variable("process_current", CPointer).toPointer(CPointer)
    val namePtr = *(processPtr+1).toPointer(CArray(32, true)) // 32 char max! and const
    *(namePtr).map(array => 
      Process( array.takeWhile(_ != 0).map(_.toChar).mkString , processPtr.now )
    ) 
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

  implicit val wrap: Mote => RichMote = RichMote(_)

  /**
   * Clears mote conversion cache.
   */
  def clearCache() {
    conversions = Nil
    cache.clear()
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
 * Radio transmission wrapper.
 *
 * @param startTime start time of transmission in microseconds
 * @param endTime end time of transmission in microseconds
 * @param source source [[Radio]]
 * @param destinations set of destination [[Radio]]s
 * @param interfered set of [[Radio]]s interfered by this transmission (including destinations)
 * @param packet transmitted [[RadioPacket]]
 */
case class RadioTransmission(startTime: Long, endTime: Long, source: Radio,
  destinations: Set[Radio], interfered: Set[Radio], packet: RadioPacket) {
  /**
   * Source mote (if any).
   */
  lazy val sourceMote = source.getMote

  /**
   * Set of destination motes (if any).
   */
  lazy val destinationMotes = destinations.map(_.getMote)

  /**
   * Set of interferred motes (if any).
   */
  lazy val interferedMote = interfered.map(_.getMote)

  /**
   * Set of radios interfered by this transmission excluding destinations.
   */
  lazy val interferedNonDestinations = interfered -- destinations

  /**
   * Packet data bytes.
   */
  lazy val packetData = packet.getPacketData

  /**
   * Packet data in human readable form.
   */
  lazy val packetString = packetData.map(b => "%02X".format(b)).mkString(" ")
}

/**
 * Generic radio medium wrapper.
 */
class RichRadioMedium(val radioMedium: RadioMedium)(implicit val simulation: Simulation)
    extends RichObservable {
  /**
   * List of active radio connections as a signal.
   * @return [[Signal]] of a list of active radio connections
   */
  lazy val connections = SeqSignal( observedSignal {
    radioMedium.asInstanceOf[org.contikios.cooja.radiomediums.AbstractRadioMedium].getActiveConnections.toList
  })

  /**
   * Stream of all completed radio transmissions.
   * @return [[EventStream]] of [[RadioTransmission]]s.
   */
  lazy val transmissions = observedEvent {
    val conn = radioMedium.getLastConnection
    if(conn == null)
      null
    else {
      RadioTransmission(conn.getStartTime, simulation.getSimulationTime, conn.getSource,
                        conn.getDestinations.toSet, conn.getInterfered.toSet,
                        conn.getSource.getLastPacketTransmitted)
    }
  }.filter(_ != null)

  // uses different observer functions
  def addObserver(o: Observer) { radioMedium.addRadioMediumObserver(o) }
  def removeObserver(o: Observer) { radioMedium.deleteRadioMediumObserver(o) }
}

object RichRadioMedium {
  implicit def wrap(rm: RadioMedium)(implicit sim: Simulation): RichRadioMedium =
    new RichRadioMedium(rm)
}


/**
 * Simulation log message.
 *
 * @param mote mote which sent log message
 * @param message the log message
 */
case class LogMessage(mote: Mote, message: String)

/**
 * (GUI) Log wrapper.
 */
class RichLog(val simulation: Simulation) {
  /**
   * Get a eventsource which fires for log messages of '''all''' motes in simulation.
   *
   * @return [[EventSource]] of LogMessages
   */
  lazy val messages = {
    // create new eventsource
    val es = new EventSource[LogMessage]()

    // create observer which calls fun and fires result
    val l = new SimEventCentral.LogOutputListener() {
      def newLogOutput(loe: SimEventCentral.LogOutputEvent) {
        es fire LogMessage(loe.getMote, loe.getMessage)
      }
      def removedLogOutput(loe: SimEventCentral.LogOutputEvent) {}
      def moteWasAdded(mote: Mote) {}
      def moteWasRemoved(mote: Mote) {}
    }

    // add listener to simulation
    simulation.getEventCentral.addLogOutputListener(l)

    // remove observer when deactivating plugin
    CoojaTracePlugin.forSim(simulation).onCleanUp {
      simulation.getEventCentral.removeLogOutputListener(l)
    }

    // return eventsource
    es
  }
}



/**
 * Mote2Mote relation wrapper.
 */
class RichMoteRelations(val simulation: Simulation) extends RichObservable {
  /**
   * Get a list of all Mote2Mote relations in this simulation.
   * @return [[SeqSignal]] of a list of Mote2Mote relations
   */
  lazy val relations = SeqSignal( observedSignal { simulation.getCooja.getMoteRelations.toList } )

  // uses different observer functions
  def addObserver(o: Observer) { simulation.getCooja.addMoteRelationsObserver(o) }
  def removeObserver(o: Observer) { simulation.getCooja.deleteMoteRelationsObserver(o) }
}

} // package wrappers

