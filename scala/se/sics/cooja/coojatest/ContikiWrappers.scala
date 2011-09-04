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
  var variables = Map[String, Tuple2[Var[_], () => Unit]]()
  val memory = mote.getMemory.asInstanceOf[AddressMemory]
  
  def addIntVar(name: String): Signal[Int] = {
    val v = Var[Int](memory.getIntValueOf(name)) 
    variables += name -> (v, () => v.update(memory.getIntValueOf(name))) 
    v
  }
  
  def addByteVar(name: String): Signal[Byte] = {
    val v = Var[Byte](memory.getByteValueOf(name)) 
    variables += name -> (v, () => v.update(memory.getByteValueOf(name))) 
    v
  }
  
  def addArray(name: String, length: Int): Signal[Array[Byte]] = {
    val v = Var[Array[Byte]](memory.getByteArray(name, length)) 
    variables += name -> (v, () => v.update(memory.getByteArray(name, length))) 
    v
  }
  
  def doActionsAfterTick() {
      setChanged()
      notifyObservers(this)
      for((name, (signal, update)) <- variables) {
        update()
      }
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
  
  def intVar(name: String) = memoryInterface.addIntVar(name)
  def byteVar(name: String) = memoryInterface.addByteVar(name)
  def array(name: String, length: Int) = memoryInterface.addArray(name, length)
}

}

