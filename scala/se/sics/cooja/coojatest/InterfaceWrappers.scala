package se.sics.cooja.coojatest.interfacewrappers

import se.sics.cooja._
import se.sics.cooja.coojatest.wrappers._

import se.sics.cooja.interfaces._



class RichLED(val interface: LED) extends RichInterface[LED] {
  lazy val status = observedSignal{ (interface.isRedOn, interface.isGreenOn, interface.isYellowOn) }
}



class RichRadio(val interface: Radio) extends RichInterface[Radio]  {
  lazy val events = observedEvent{ interface.getLastEvent }
}
