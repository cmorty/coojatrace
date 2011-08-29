package se.sics.cooja.coojatest

import se.sics.cooja._

import java.util._
import java.io._

import javax.swing._
import java.awt._
import java.awt.event._

import magicsignals._
import operators._
import wrappers._
import interfacewrappers._
import contikiwrappers._
import mspwrappers._



object Conversions {
  import se.sics.cooja.interfaces._

  implicit def simToRichSim(s: Simulation) = new RichSimulation(s)
  implicit def moteToRichMote(m: Mote) = new RichMote(m)
  implicit def ledToRichLED(i: LED) = new RichLED(i)
  implicit def radioToRichRadio(r: Radio) = new RichRadio(r)
    
  implicit def radioMediumToRichRadioMedium(rm: RadioMedium) = new RichRadioMedium(rm)
  
  def interface[T <: MoteInterface](i: MoteInterface): T = i.asInstanceOf[T]
  implicit def ledInterface = interface[LED] _
  implicit def radioInterface = interface[Radio] _
}



@ClassDescription("CoojaTest")
@PluginType(PluginType.SIM_PLUGIN)
class CoojaTestPlugin(sim: Simulation, gui: GUI) extends VisPlugin("CoojaTest", gui, false) {
  val logger = org.apache.log4j.Logger.getLogger(this.getClass)

  // Aufteilung
  val grid = new GridLayout(0,2)
  setLayout(grid)
  
  // Beschriftung
  val scriptLabel = new JLabel("Script:")

  // Code Textfeld
  val scriptCode = new JTextArea()

  // Ergebnis Textfeld
  val scriptResult = new JTextArea()
  val pwriter = new PrintWriter(new Writer () { 
    def close {}
    def flush {}
    def write(cbuf: Array[Char],  off: Int, len: Int) {
      scriptResult.append(new String(cbuf, off, len));
    }
  })
  scriptResult.setEditable(false)
  
  // Scala Interpreter
  lazy val interpreter = {
    import scala.tools.nsc._
    
    val settings = new Settings()
    
    def jarPathOfClass(className: String) = {
      val resource = className.split('.').mkString("/", "/", ".class")
  	  val path = getClass.getResource(resource).getPath
  	  logger.debug("path of class " + className + " is " + path)
  	  val indexOfFile = path.indexOf("file:")
  	  val indexOfSeparator = path.lastIndexOf('!')
  	  path.substring(indexOfFile, indexOfSeparator)
    }
    
    // TODO: classPath von Cooja bekommen?
    val classes = scala.List(
      "scala.tools.nsc.Interpreter", "scala.ScalaObject" ,
      "org.apache.log4j.Logger", "org.jdom.Element",
      "reactive.EventSource",
      "se.sics.cooja.coojatest.CoojaTestPlugin",
      "se.sics.mspsim.Main", "se.sics.cooja.mspmote.MspMote"
    )
    
    settings.bootclasspath.value = classes.map(jarPathOfClass(_)).mkString(":")
    logger.debug("Scala Bootclasspath: " + settings.bootclasspath.value) // DEBUG
    
    new Interpreter(settings, pwriter)
  }
  
  // Flush Button
  val flushButton = new JButton("flush");
  flushButton.addActionListener(new ActionListener() {
    def actionPerformed(e: ActionEvent) {
      interpreter.interpret("logdestination.stream.flush()")
    } 
  })
      
  // Run Button
  val scriptButton = new JButton("run");
  scriptButton.addActionListener(new ActionListener() {
    def actionPerformed(e: ActionEvent) {
      pwriter.flush()
      interpreter.reset()
      Rules.assertions = Nil
      Rules.logrules = Nil
      

      interpreter.interpret("""
        import se.sics.cooja._
        import se.sics.cooja.interfaces._
        import reactive._
        import se.sics.cooja.coojatest.Conversions._
        import se.sics.cooja.coojatest.magicsignals.MagicSignals._
        import se.sics.cooja.coojatest.Rules._
        import se.sics.cooja.coojatest.operators.Operators._

        import se.sics.cooja.coojatest.contikiwrappers._
        import se.sics.cooja.coojatest.mspwrappers._
      """)
  
      interpreter.bind("sim", sim.getClass.getName, sim)
      interpreter.bind("pwriter", pwriter.getClass.getName, pwriter)
      
      interpreter.interpret("""
        implicit val theSimulation = sim
        implicit val logdestination = new se.sics.cooja.coojatest.LogDestination(pwriter)
        implicit val observing = new Observing {}
        implicit val dl = new se.sics.cooja.coojatest.magicsignals.DynDepLog
      """)
	
      scriptResult.setText("")
      
      val res = interpreter.interpret(scriptCode.getText())
      logger.debug(res)
      if(res == scala.tools.nsc.InterpreterResults.Success) {
        scriptResult.setForeground(Color.BLACK)
      }
      else {
        scriptResult.setForeground(Color.RED)
      }
        
    }
  })

  add(scriptLabel)
  add(scriptButton)
  add(flushButton)
  add(new JScrollPane(scriptCode))
  add(new JScrollPane(scriptResult))
  
  setSize(300,100)
  
  override def closePlugin() { 
    logger.debug("closePlugin() called")
  }  
}
