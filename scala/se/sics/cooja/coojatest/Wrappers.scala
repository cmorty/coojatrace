package se.sics.cooja.coojatest.wrappers

import reactive._

import java.util.{Observable, Observer}

import se.sics.cooja._
import se.sics.cooja.interfaces._

import se.sics.cooja.coojatest.interfacewrappers._



object Conversions {
  implicit def simToRichSim(s: Simulation) = new RichSimulation(s)
  implicit def moteToRichMote(m: Mote) = RichMote(m)
  implicit def radioMediumToRichRadioMedium(rm: RadioMedium) = new RichRadioMedium(rm)
}



class RichSimulation(val sim: Simulation) {
  def motes = sim.getMotes().map(m => m.getID() -> m).toMap
  def time = sim.getSimulationTime
  def radioMedium = sim.getRadioMedium
}



trait RichCPU  {
  def register(name: String): Signal[Int]
  def stackPointer: Signal[Int]
}



class RichMote(val mote: Mote) extends InterfaceAccessors {
  def memory: RichMoteMemory = throw new Exception("Unsupported for this mote type")
  def cpu: RichCPU = throw new Exception("Unsupported for this mote type")
  
  lazy val varAdresses = memory.memory.getVariableNames.map(name => (memory.memory.getVariableAddress(name), name)).toMap
  lazy val currentProcess = memory.intVar("process_current").map(addr => varAdresses(addr) + " (0x" + "%X".format(addr) + ")")
}
object RichMote {
  var conversions = List[PartialFunction[Mote, RichMote]]()
  val defaultConversion: PartialFunction[Mote, RichMote] = { case m: Mote => new RichMote(m) }

  val cache = collection.mutable.Map[Mote, RichMote]()

  def apply(mote: Mote): RichMote = cache.getOrElseUpdate(mote,
     conversions.find(_.isDefinedAt(mote)).getOrElse(defaultConversion).apply(mote)
   )
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

