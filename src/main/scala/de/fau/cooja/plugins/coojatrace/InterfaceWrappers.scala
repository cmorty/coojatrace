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



import org.contikios.cooja._
import interfaces._

import de.fau.cooja.plugins.coojatrace.wrappers._

import scala.collection.JavaConverters._



// speed up compilation
class InterfaceWrappers



/**
 * Implicit conversions from original cooja mote interfaces to their rich wrappers.
 */
package object interfacewrappers {
  /**
   * Cast interface to specified subtype.
   *
   * '''Note:''' This function only serves to shorten the actual conversion functions below.
   *
   * @param i [[MoteInterface]] to be casted (must be supertype)
   * @return interface cast to specified subtype of [[MoteInterface]]
   * @tparam T interface type to cast to (subtype of [[MoteInterface]])
   */
  private def interface[T <: MoteInterface](i: MoteInterface): T = i.asInstanceOf[T]
  
  implicit def led2RichLED(i: LED)(implicit sim: Simulation) =
    new RichLED(i, sim)
  implicit def ledInterface = interface[LED] _

  implicit def radio2RichRadio(r: Radio)(implicit sim: Simulation) =
    new RichRadio(r, sim)
  implicit def radioInterface = interface[Radio] _

  implicit def position2RichPosition(p: Position)(implicit sim: Simulation) = 
    new RichPosition(p, sim)
  implicit def positionInterface = interface[Position] _

  implicit def log2RichLog(l: Log)(implicit sim: Simulation) =
    new RichLog(l, sim)
  implicit def logInterface = interface[Log] _

  implicit def ipAddress2RichIPAddress(ia: IPAddress)(implicit sim: Simulation) = 
    new RichIPAddress(ia, sim)
  implicit def ipAddressInterface = interface[IPAddress] _

  implicit def rimeAddress2RichRimeAddress(ra: RimeAddress)(implicit sim: Simulation) = 
    new RichRimeAddress(ra, sim)
  implicit def rimeAddressInterface = interface[RimeAddress] _

  implicit def moteID2RichMoteID(id: MoteID)(implicit sim: Simulation) =
    new RichMoteID(id, sim)
  implicit def moteIDInterface = interface[MoteID] _

  implicit def beeper2RichBeeper(b: Beeper)(implicit sim: Simulation) = 
    new RichBeeper(b, sim)
  implicit def beeperInterface = interface[Beeper] _

  implicit def button2RichButton(bt: Button)(implicit sim: Simulation) =
    new RichButton(bt, sim)
  implicit def buttonInterface = interface[Button] _

  implicit def moteAttributes2RichMoteAttributes(ma: MoteAttributes)(implicit sim: Simulation) =
    new RichMoteAttributes(ma, sim)
  implicit def moteAttributes = interface[MoteAttributes] _
}



package interfacewrappers {

/**
 * Accessor functions for mote interfaces (only to reduce typing).
 * 
 * '''Note:''' This trait is mixed into [[RichMote]].
 */
trait InterfaceAccessors { this: RichMote =>
  /**
   * Get map of mote interfaces with interface classnames as keys.
   * @return map of (clasnsname -> interface) elements
   */
  def interfaces: Map[String, MoteInterface] = 
    mote.getInterfaces.getInterfaces.asScala.map(i => i.getClass.getName.split("\\.").last -> i).toMap
  
  /**
   * Get interface of specified class.
   *
   * @param t class of interface type
   * @return interface cast to specified subtype of [[MoteInterface]]
   * @tparam T interface type to return (subtype of [[MoteInterface]])
   */
  def interface[T <: MoteInterface](t: Class[T]): T = mote.getInterfaces.getInterfaceOfType(t)
  
  def led = interface(classOf[LED])
  def radio = interface(classOf[Radio])
  def position = interface(classOf[Position])
  def log = interface(classOf[Log])
  def ipAddress = interface(classOf[IPAddress])
  def rimeAddress = interface(classOf[RimeAddress])
  def moteID = interface(classOf[MoteID])
  def beeper = interface(classOf[Beeper])
  def button = interface(classOf[Button])
  def clock = interface(classOf[Clock])
  def pir = interface(classOf[PIR])
  def moteAttributes = interface(classOf[MoteAttributes])
}



/**
 * Mote LED status.
 * 
 * @param redOn `true` if red LED is lit 
 * @param greenOn `true` if green LED is lit 
 * @param yellowOn `true` if yellow LED is lit 
 */
case class LEDStatus(redOn: Boolean, greenOn: Boolean, yellowOn: Boolean)

/**
 * Wrapper for mote LED interface.
 */
class RichLED(val interface: LED, val simulation: Simulation) extends RichInterface[LED] {
  /**
   * Get signal of LED status.
   * @return [[Signal]] of type [[LEDStatus]]
   */
  lazy val status = observedSignal {
    LEDStatus(interface.isRedOn, interface.isGreenOn, interface.isYellowOn) 
  }
}



/**
 * Mote position.
 * 
 * @param x X coordinate of mote 
 * @param y Y coordinate of mote
 * @param z Z coordinate of mote 
 */
case class MotePosition(x: Double, y: Double, z: Double)

/**
 * Wrapper for mote position (interface).
 */
class RichPosition(val interface: Position, val simulation: Simulation) extends RichInterface[Position] {
  /**
   * Get signal of mote position.
   * @return [[Signal]] of type [[MotePosition]]
   */
  lazy val position = observedSignal {
    MotePosition(interface.getXCoordinate, interface.getYCoordinate, interface.getZCoordinate)
  }
}



/**
 * Wrapper for mote radio interface.
 */
class RichRadio(val interface: Radio, sim: Simulation) extends RichInterface[Radio]  {
  implicit val simulation = sim

  /**
   * Get eventstream of mote radio interface events.
   * @return [[EventStream]] of radio events
   */
  lazy val events = observedEvent { interface.getLastEvent }

  /**
   * Get signal of mote radio interference status.
   * @return boolean [[Signal]] of interference status, `true` when being interfered
   */
  lazy val interfered = observedSignal { interface.isInterfered }
  
  
  /**
   * Get signal of mote radio receiver status.
   * @return boolean [[Signal]] of receiver status, `true` when receiver is on
   */
  lazy val radioOn = observedSignal { 
	  interface.isRadioOn 
    }
  
  
  /**
   * Get signal of mote radio receiver status.
   * @return boolean [[Signal]] of receiver status, `true` when receiver is on
   */
  lazy val receiverOn = observedSignal { 
	   (interface.isRadioOn() && !interface.isTransmitting())
    }

  /**
   * Get signal of mote radio reception status.
   * @return boolean [[Signal]] of reception status, `true` when receiving
   */
  lazy val receiving = observedSignal {interface.isReceiving }

  /**
   * Get signal of mote radio transmission status.
   * @return boolean [[Signal]] of transmission status, `true` when transmissing
   */
  lazy val transmitting = observedSignal { interface.isTransmitting }
  


  /**
   * Get signal of mote radio channel.
   * @return [[Signal]] of radio channel
   */
  lazy val channel = observedSignal { interface.getChannel }

  /**
   * Get signal of mote radio output power.
   * @return [[Signal]] of output power.
   */
  lazy val currentOutputPower = observedSignal { interface.getCurrentOutputPower }

  /**
   * Get signal of mote radio output power indicator.
   * @return [[Signal]] of output power indicator
   */
  lazy val currentOutputPowerIndicator = observedSignal { interface.getCurrentOutputPowerIndicator }

  /**
   * Get signal of mote radio signal strength.
   * @return [[Signal]] of current signal strength
   */
  lazy val currentSignalStrength = observedSignal { interface.getCurrentSignalStrength }



  /**
   * Get signal of mote radio position.
   * @return [[Signal]] of type [[MotePosition]]
   */
  lazy val position = observedSignal { interface.getPosition }

  /**
   * Get stream of transmitted radio packets.
   * @return [[EventStream]] of transmitted [[RadioPacket]]s
   */
  lazy val packetsTransmitted = observedEvent {
    interface.getLastPacketTransmitted
  }.filter(_ => interface.getLastEvent == Radio.RadioEvent.PACKET_TRANSMITTED)

  /**
   * Get stream of received radio packets.
   * @return [[EventStream]] of received [[RadioPacket]]s
   */
  lazy val packetsReceived = observedEvent {
    interface.getLastPacketReceived 
  }.filter(_ => interface.getLastEvent == Radio.RadioEvent.RECEPTION_FINISHED)

  /**
   * Radio transmissions started by this radio.
   */
  lazy val transmissions = simulation.radioMedium.transmissions.filter(_.source == interface)

  /**
   * Radio transmissions received by this radio. Includes interfered transmissions!
   */
  lazy val receptions = simulation.radioMedium.transmissions.filter(_.destinations.contains(interface))
}



/**
 * Wrapper for mote log (interface).
 */
class RichLog(val interface: Log, val simulation: Simulation) extends RichInterface[Log] {
  /**
   * Get eventstream of log messages.
   * @return [[EventStream]] of log messages
   */
  lazy val messages = observedEvent { interface.getLastLogMessage }
}



/**
 * Wrapper for mote beeper (interface).
 */
class RichBeeper(val interface: Beeper, val simulation: Simulation) extends RichInterface[Beeper] {
  /**
   * Get signal of mote beeper status.
   * @return boolean [[Signal]] of beeper status, `true` when beeping
   */
  lazy val beeping = observedSignal { interface.isBeeping }
}



/**
 * Wrapper for mote button (interface).
 */
class RichButton(val interface: Button, val simulation: Simulation) extends RichInterface[Button] {
  /**
   * Get signal of mote button status.
   *
   * '''Note:''' Some mote types will notify observers when clicked, but do not set
   * pressed status to `true`, in these cases signal will always be `false` but "changes"
   * are still propagated.
   *
   * @return boolean [[Signal]] of button status, `true` when pressed
   */
  lazy val pressed = observedSignal { interface.isPressed }
}



/**
 * Wrapper for mote IP address (interface).
 */
class RichIPAddress(val interface: IPAddress, val simulation: Simulation) extends RichInterface[IPAddress] {
  /**
   * Get signal of mote IP address.
   * @return [[Signal]] of mote IP address as string
   */
  lazy val ipAddress = observedSignal { interface.getIPString }
}



/**
 * Wrapper for mote rime address (interface).
 */
class RichRimeAddress(val interface: RimeAddress, val simulation: Simulation) extends RichInterface[RimeAddress] {
  /**
   * Get signal of mote rime address.
   * @return [[Signal]] of mote rime address as string
   */
  lazy val address = observedSignal { interface.getAddressString }
}



/**
 * Wrapper for mote ID (interface).
 */
class RichMoteID(val interface: MoteID, val simulation: Simulation) extends RichInterface[MoteID] {
  /**
   * Get signal of mote ID.
   * @return [[Signal]] of mote ID as integer
   */
  lazy val moteID = observedSignal { interface.getMoteID }
}



/**
 * Wrapper for mote attributes (interface).
 */
class RichMoteAttributes(val interface: MoteAttributes, val simulation: Simulation) extends RichInterface[MoteAttributes] {
  /**
   * Get attributes of mote.
   * @return [[Signal]] of a (attributeName -> value) Map
   */
  lazy val attributes = observedSignal {
    val attrs = for {
      pair <- interface.getText.split("\n")
      Array(attr, name) = pair.split("=")
    } yield (attr, name)
    attrs.toMap
  }
}

} // package interfacewrappers
