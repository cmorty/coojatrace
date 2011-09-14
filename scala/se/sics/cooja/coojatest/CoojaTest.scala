package se.sics.cooja.coojatest

import se.sics.cooja._

import java.util.{List => _, _}
import java.io._

import javax.swing._
import java.awt.{List => _, _}
import java.awt.event._

import scala.collection.JavaConversions._

import magicsignals._
import operators._
import wrappers._
import interfacewrappers._
import contikiwrappers._
import mspwrappers._



@ClassDescription("CoojaTest")
@PluginType(PluginType.SIM_PLUGIN)
class CoojaTestPlugin(sim: Simulation, gui: GUI) extends VisPlugin("CoojaTest", gui, false) {
  val logger = org.apache.log4j.Logger.getLogger(this.getClass)
  
  // Code Textfeld
  val scriptCode = new JTextArea()

  // Run Button
  val scriptButton = new JButton("Activate");

  if(GUI.isVisualized) createGUI()

  def createGUI() {
    // Aufteilung
    val grid = new GridLayout(0,2)
    setLayout(grid)
    
    // Beschriftung
    val scriptLabel = new JLabel("Script:")

    scriptButton.addActionListener(new ActionListener() {
      def actionPerformed(e: ActionEvent) {
        if(isActive)
          deactivate()  
        else
          activate()
      }
    })

    //add(scriptLabel)
    add(scriptButton)
    add(new JScrollPane(scriptCode))
    
    setSize(500,500)
  
    // Reload notwendig?
    if(sim.getMotes.exists { _.getClass.getClassLoader != this.getClass.getClassLoader } ) {
      val s1 = "Reload"
      val s2 = "Ignore"
      val options = Array[Object](s1, s2)
      val n = JOptionPane.showOptionDialog(
                GUI.getTopParentContainer(),
                "The CoojaTest plugin was loaded after other plugins. This can load to classloader inconsistencies.\nDo you want to reload the simulation?",
                "Reload simulation?", JOptionPane.YES_NO_OPTION,
                JOptionPane.QUESTION_MESSAGE, null, options, s1);
      if (n == JOptionPane.YES_OPTION) {
        sim.getGUI.reloadCurrentSimulation(false)
      }
    }
  }
  
  
  // Scala Interpreter
  def createInterpreter() = {
    import scala.tools.nsc._
    
    val settings = new Settings()
    
    def jarPathOfClass(className: String) = {
      val resource = className.split('.').mkString("/", "/", ".class")
      val path = getClass.getResource(resource).getPath
      val indexOfFile = path.indexOf("file:")
      val indexOfSeparator = path.lastIndexOf('!')
      path.substring(indexOfFile, indexOfSeparator)
    }
    
    val classes = scala.List(
      "org.apache.log4j.Logger", "org.jdom.Element"
    )
    
    val coojaLibs = classes.map(jarPathOfClass(_))
    val classloader = sim.getGUI.projectDirClassLoader.asInstanceOf[java.net.URLClassLoader]
    val dynamicLibs = classloader.getURLs.map(_.toString.replace("file:", "")).toList
    settings.bootclasspath.value = (coojaLibs ::: dynamicLibs).mkString(":")
    
    new Interpreter(settings, new PrintWriter(System.out, true))
  }

  private var interpreter: scala.tools.nsc.Interpreter = null

  private var active = false
  def isActive = active

  def activate() {
    interpreter = createInterpreter()

    interpreter.interpret("""
      import se.sics.cooja._
      import se.sics.cooja.interfaces._
      import reactive._
      import se.sics.cooja.coojatest.wrappers.Conversions._
      import se.sics.cooja.coojatest.interfacewrappers.Conversions._
      import se.sics.cooja.coojatest.magicsignals.MagicSignals._
      import se.sics.cooja.coojatest.rules._
      import se.sics.cooja.coojatest.operators._

      import se.sics.cooja.coojatest.contikiwrappers._
      se.sics.cooja.coojatest.contikiwrappers.register()
      import se.sics.cooja.coojatest.mspwrappers._
      se.sics.cooja.coojatest.mspwrappers.register()
    """)
    
    interpreter.bind("sim", sim.getClass.getName, sim)
    
    interpreter.interpret("""
      implicit val _simulation = sim
      implicit val _observing = new Observing {}
      implicit val _dyndeplog = new se.sics.cooja.coojatest.magicsignals.DynDepLog
    """)
    
    val res = interpreter.interpret(scriptCode.getText())
    logger.debug("Interpreting script: " + res)
    if(res == scala.tools.nsc.InterpreterResults.Success) {
      scriptButton.setText("Reset")
      active = true
    }
    else {
      //if(GUI.showErrorDialog(this, "Scala Compilation Error", new Exception(""), true)) activate()
    }
  }

  def deactivate() {
    interpreter.reset() // necessary?
    interpreter = null

    scriptButton.setText("Activate")

    active = false
  }

  override def getConfigXML(): java.util.Collection[org.jdom.Element] = {
    val script = new org.jdom.Element("script").setText(scriptCode.getText)
    val active = new org.jdom.Element("active").setText(isActive.toString)
    List(script, active)
  }

  override def setConfigXML(configXML: java.util.Collection[org.jdom.Element], visAvailable: Boolean): Boolean = {
    for(e <- configXML) {
      if(e.getName == "script" && !e.getText.isEmpty) scriptCode.setText(e.getText)
      if(e.getName == "active" && GUI.isVisualized) activate()
    }

    if(!GUI.isVisualized) {
      activate()
      //sim.setDelayTime(0)
      //sim.startSimulation()
    }

    true
  }
  
  override def closePlugin() { 
    logger.debug("closePlugin() called")
  }  
}
