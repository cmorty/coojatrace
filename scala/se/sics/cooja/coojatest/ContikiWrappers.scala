package se.sics.cooja.coojatest

import reactive._

import se.sics.cooja._
import se.sics.cooja.coojatest.wrappers._

import se.sics.cooja.contikimote._
import se.sics.cooja.interfaces._




package object contikiwrappers {
  implicit def contikiMote2RichMote(cm: ContikiMote) = new ContikiRichMote(cm)
  
  def register() {
    RichMote.conversions ::= { case cm: ContikiMote => contikiMote2RichMote(cm) }
  }
}

package contikiwrappers {

class ContikiRichMote(mote: ContikiMote) extends RichMote(mote) {
  override lazy val memory = new ContikiMoteRichMemory(mote)
}



@ClassDescription("Memory")
class MemoryInterface(mote: Mote) extends MoteInterface with PolledAfterActiveTicks {
  private var updates = List[() => Unit]()

  def addVar[T](name: String, updateFun: (String) => T): Signal[T] = {
    val v = Var[T](updateFun(name))
    updates ::= ( () => v.update(updateFun(name)) )
    v
  }

  def doActionsAfterTick() {
      setChanged()
      notifyObservers(this)
      for(u <- updates) u()
  }

  def getInterfaceVisualizer = null
  def releaseInterfaceVisualizer(panel: javax.swing.JPanel) {}
  def getConfigXML = null
  def setConfigXML(configXML: java.util.Collection[org.jdom.Element], visAvailable: Boolean) {}
}



class ContikiMoteRichMemory(val mote: ContikiMote) extends RichMoteMemory {
  lazy val memoryInterface = if(mote.getInterfaces.get("MemoryInterface") == null) {
    val mI = new MemoryInterface(mote)
    mote.getInterfaces.addInterface(mI)
    mI
  } else {
    mote.getInterfaces.get("MemoryInterface").asInstanceOf[MemoryInterface]
  }

  lazy val memory = mote.getMemory.asInstanceOf[AddressMemory]

  def addIntVar(name: String) = memoryInterface.addVar(name, memory.getIntValueOf)
  def addByteVar(name: String) = memoryInterface.addVar(name, memory.getByteValueOf)
  def addArrayVar(name: String, length: Int) =
    memoryInterface.addVar(name, memory.getByteArray(_, length))
}

}

