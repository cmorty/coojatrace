package se.sics.cooja.coojatest.interfacewrappers

import se.sics.cooja._
import se.sics.cooja.coojatest.wrappers._

import se.sics.cooja.interfaces._

import scala.collection.JavaConverters._



object Conversions {
  def interface[T <: MoteInterface](i: MoteInterface): T = i.asInstanceOf[T]
  
  implicit def ledToRichLED(i: LED) = new RichLED(i)  
  implicit def ledInterface = interface[LED] _

  implicit def radioToRichRadio(r: Radio) = new RichRadio(r)
  implicit def radioInterface = interface[Radio] _
}



trait InterfaceAccessors { this: RichMote =>
  def interfaces: Map[String, MoteInterface] = 
    mote.getInterfaces.getInterfaces.asScala.map(i => i.getClass.getName.split("\\.").last -> i).toMap
  def interface[T <: MoteInterface](t: Class[T]): T =
    mote.getInterfaces.getInterfaceOfType(t)
  
  def leds = interface(classOf[LED])
  def radio = interface(classOf[Radio])
}


case class LEDStatus(redOn: Boolean, greenOn: Boolean, yellowOn: Boolean)
class RichLED(val interface: LED) extends RichInterface[LED] {
  lazy val status = observedSignal {
   LEDStatus(interface.isRedOn, interface.isGreenOn, interface.isYellowOn) 
  }
}



class RichRadio(val interface: Radio) extends RichInterface[Radio]  {
  lazy val events = observedEvent{ interface.getLastEvent }
}
