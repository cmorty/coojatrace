package se.sics.cooja.coojatest.interfacewrappers

import se.sics.cooja._
import se.sics.cooja.coojatest.wrappers._

import se.sics.cooja.interfaces._

import scala.collection.JavaConverters._



object Conversions {
  def interface[T <: MoteInterface](i: MoteInterface): T = i.asInstanceOf[T]
  
  implicit def led2RichLED(i: LED) = new RichLED(i)  
  implicit def ledInterface = interface[LED] _

  implicit def radio2RichRadio(r: Radio) = new RichRadio(r)
  implicit def radioInterface = interface[Radio] _

  implicit def position2RichPosition(p: Position) = new RichPosition(p)
  implicit def positionInterface = interface[Position] _

  implicit def log2RichLog(l: Log) = new RichLog(l)
  implicit def logInterface = interface[Log] _

  implicit def ipAddress2RichIPAddress(ia: IPAddress) = new RichIPAddress(ia)
  implicit def ipAddressInterface = interface[IPAddress] _

  implicit def rimeAddress2RichRimeAddress(ra: RimeAddress) = new RichRimeAddress(ra)
  implicit def rimeAddressInterface = interface[RimeAddress] _

  implicit def moteID2RichMoteID(id: MoteID) = new RichMoteID(id)
  implicit def moteIDInterface = interface[MoteID] _

  implicit def beeper2RichBeeper(b: Beeper) = new RichBeeper(b)
  implicit def beeperInterface = interface[Beeper] _

  implicit def button2RichButton(bt: Button) = new RichButton(bt)
  implicit def buttonInterface = interface[Button] _
}



trait InterfaceAccessors { this: RichMote =>
  def interfaces: Map[String, MoteInterface] = 
    mote.getInterfaces.getInterfaces.asScala.map(i => i.getClass.getName.split("\\.").last -> i).toMap
  def interface[T <: MoteInterface](t: Class[T]): T =
    mote.getInterfaces.getInterfaceOfType(t)
  
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
}


case class LEDStatus(redOn: Boolean, greenOn: Boolean, yellowOn: Boolean)
class RichLED(val interface: LED) extends RichInterface[LED] {
  lazy val status = observedSignal {
   LEDStatus(interface.isRedOn, interface.isGreenOn, interface.isYellowOn) 
  }
}



case class MotePosition(x: Double, y: Double, z: Double)
class RichPosition(val interface: Position) extends RichInterface[Position] {
  lazy val position = observedSignal {
   MotePosition(interface.getXCoordinate, interface.getYCoordinate, interface.getZCoordinate)
  }
}



class RichRadio(val interface: Radio) extends RichInterface[Radio]  {
  lazy val events = observedEvent { interface.getLastEvent }

  lazy val interfered = observedSignal { interface.isInterfered }
  lazy val receiverOn = observedSignal { interface.isReceiverOn }
  lazy val receiving = observedSignal { interface.isReceiving }
  lazy val transmitting = observedSignal { interface.isTransmitting }
  
  lazy val channel = observedSignal { interface.getChannel }
  lazy val currentOutputPower = observedSignal { interface.getCurrentOutputPower }
  lazy val currentOutputPowerIndicator = observedSignal { interface.getCurrentOutputPowerIndicator }
  lazy val currentSignalStrength = observedSignal { interface.getCurrentSignalStrength }
  lazy val position = observedSignal { interface.getPosition }

  // TODO: only fire at right event
  lazy val packetsTransmitted = observedSignal { interface.getLastPacketTransmitted }
  lazy val packetsReceived = observedSignal { interface.getLastPacketReceived } 
}



class RichLog(val interface: Log) extends RichInterface[Log] {
  lazy val messages = observedEvent { interface.getLastLogMessage }
}



class RichBeeper(val interface: Beeper) extends RichInterface[Beeper] {
  lazy val beeping = observedSignal { interface.isBeeping }
}



class RichButton(val interface: Button) extends RichInterface[Button] {
  lazy val pressed = observedSignal { interface.isPressed }
}



class RichIPAddress(val interface: IPAddress) extends RichInterface[IPAddress] {
  lazy val ipAddress = observedSignal { interface.getIPString }
}



class RichRimeAddress(val interface: RimeAddress) extends RichInterface[RimeAddress] {
  lazy val address = observedSignal { interface.getAddressString }
}



class RichMoteID(val interface: MoteID) extends RichInterface[MoteID] {
  lazy val moteID = observedSignal { interface.getMoteID }
}
