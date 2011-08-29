package se.sics.cooja.coojatest.wrappers

import reactive._

import scala.collection.JavaConverters._

import java.util.{Observable, Observer}

import se.sics.cooja._
import se.sics.cooja.interfaces._



class RichSimulation(val sim: Simulation) {
  def motes = sim.getMotes().map(m => m.getID() -> m).toMap
  def time = sim.getSimulationTime
  def radioMedium = sim.getRadioMedium
}



trait RichCPU  {
  def register(name: String): Signal[Int]
  def stackptr: Signal[Int]
}



class RichMote(val mote: Mote) {
  def interfaces: Map[String, MoteInterface] = 
    mote.getInterfaces.getInterfaces.asScala.map(i => i.getClass.getName.split("\\.").last -> i).toMap
  def interface[T <: MoteInterface](t: Class[T]): T =
    mote.getInterfaces.getInterfaceOfType(t)
  
  def leds = interface(classOf[LED])
  def radio = interface(classOf[Radio])
  // ...
}



trait RichObservable {
  type addFunType = (Observer) => Unit

  def observedEvent[ET](fun: => ET)(implicit addFun: addFunType) = {
    val es = new EventSource[ET]()
    addFun(new Observer() {
      def update(obs: Observable, obj: Object) {
        es fire fun
      }
    })
    es
  }
  
  def observedSignal[ST](fun: => ST)(implicit addFun: addFunType) = {
    val signal = Var[ST](fun)
    addFun(new Observer() {
      def update(obs: Observable, obj: Object) {
        signal() = fun
      }
    })
    signal
  }
}



trait RichMoteMemory {
  def intVar(name: String): Signal[Int]
  def byteVar(name: String): Signal[Byte]
  //def array(name: String, length: Int): Signal[Array[Byte]]

  def memory: AddressMemory
  def byte(name: String) = memory.getByteValueOf(name)
  def byte_=(name: String, value: Byte) { memory.setByteValueOf(name, value) }
  def int(name: String) = memory.getIntValueOf(name)
  def int_=(name: String, value: Int) { memory.setIntValueOf(name, value) }

}



trait RichInterface[T <: MoteInterface] extends RichObservable {
  val interface: T
  
  implicit val defaultAddFun: addFunType = interface.addObserver
}



class RichRadioMedium(val radioMedium: RadioMedium) extends RichObservable {
  lazy val connections = observedSignal{ radioMedium.asInstanceOf[se.sics.cooja.radiomediums.AbstractRadioMedium].getActiveConnections }(radioMedium.addRadioMediumObserver)
}

