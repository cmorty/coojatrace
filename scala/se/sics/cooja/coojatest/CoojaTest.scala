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



/**
 * 
 * The CoojaTest Cooja-Plugin.
 * 
 * Creates a scala interpreter and runs the test script to initialize rules, operators and signals
 * of the CoojaTest framework. Can be run without GUI.
 *
 * @author Florian Lukas [[mailto:florian.lukas@e-technik.stud.uni-erlangen.de]]
 */
@ClassDescription("CoojaTest")
@PluginType(PluginType.SIM_PLUGIN) // comment out this line to prevent scaladoc bug
class CoojaTestPlugin(sim: Simulation, gui: GUI) extends VisPlugin("CoojaTest", gui, false) {
  /**
   * Logger for our plugin.
   */
  val logger = org.apache.log4j.Logger.getLogger(this.getClass)
  
  /**
   * Textarea for script code.
   */
  val scriptCode = new JTextArea()

  /**
   * Button to run/deactivate test script.
   */
  val scriptButton = new JButton("Activate");

  /**
   * The scala interpreter.
   * Initialized by `createInterpreter`
   */
  private var interpreter: scala.tools.nsc.Interpreter = null

  /**
   * Status variable showing whether script has been run and is active.
   */
  private var active = false
  
  // Constructor:
  // create Swing elements if run with GUI
  if(GUI.isVisualized) createGUI()

  /**
   * Create Swing elements for plugin GUI.
   */
  private def createGUI() {
    // add run/reset button handler
    scriptButton.addActionListener(new ActionListener() {
      def actionPerformed(e: ActionEvent) {
        if(isActive)
          deactivate()  
        else
          activate()
      }
    })

    // add elements to window
    add(scriptButton, BorderLayout.PAGE_END)
    add(new JScrollPane(scriptCode), BorderLayout.CENTER)
    setSize(500,500)
  
    // Propose to reload simulation when different classloaders are found
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
  
  /**
   * Create new scala interpreter with classpath of all loaded plugin and cooja itself.
   * @return new Interpreter-object
   */
  private def createInterpreter() = {
    // import interpreter classes
    import scala.tools.nsc._
    
    val settings = new Settings()
    
    /**
     * Get JAR path for a given classname.
     * @param className fully qualified java classname
     * @return absolute path to jar file where class is included
     */
    def jarPathOfClass(className: String) = {
      val resource = className.split('.').mkString("/", "/", ".class")
      val path = getClass.getResource(resource).getPath
      val indexOfFile = path.indexOf("file:")
      val indexOfSeparator = path.lastIndexOf('!')
      path.substring(indexOfFile, indexOfSeparator)
    }
    
    // external classes not loaded by plugins
    val classes = scala.List(
      "org.apache.log4j.Logger", "org.jdom.Element"
    )
    
    // assemble JAR path
    val coojaLibs = classes.map(jarPathOfClass(_))
    val classloader = sim.getGUI.projectDirClassLoader.asInstanceOf[java.net.URLClassLoader]
    val dynamicLibs = classloader.getURLs.map(_.toString.replace("file:", "")).toList
    settings.bootclasspath.value = (coojaLibs ::: dynamicLibs).mkString(":")
    
    // create new scala interpreter with classpath and write output to System.out (autoflush)
    new Interpreter(settings, new PrintWriter(System.out, true))
  }

  /**
   * Returns plugin status.
   * @return `true` if script has been run and is active
   */
  def isActive = active

  /**
   * Activate plugin by running test script.
   */
  def activate() {
    // create interpreter
    interpreter = createInterpreter()

    // import cooja and coojatest classes, register motewrappers
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
    
    // make the simulation object available to test code
    interpreter.bind("sim", sim.getClass.getName, sim)
    
    // define implicit vals for simulation, observing context, and MagicSignal deplog
    interpreter.interpret("""
      implicit val _simulation = sim
      implicit val _observing = new Observing {}
      implicit val _dyndeplog = new se.sics.cooja.coojatest.magicsignals.DynDepLog
    """)
    
    // interpret test script
    val res = interpreter.interpret(scriptCode.getText())
    logger.debug("Interpreting script: " + res)

    if(res == scala.tools.nsc.InterpreterResults.Success) {
      // success, change 
      scriptButton.setText("Reset")
      active = true
    }
    else if(res == scala.tools.nsc.InterpreterResults.Incomplete) {
      // incomplete, show warning dialog
      // TODO
    }
    else {
      // error, show error dialog
      // TODO
      //if(GUI.showErrorDialog(this, "Scala Compilation Error", new Exception(""), true)) activate()
    }
  }

  /**
   * Deactivate plugin by resetting and deleting interpreter.
   */
  def deactivate() {
    // reset interpreter (clears all defined objects)
    interpreter.reset() // necessary?

    // delete interpreter instance
    interpreter = null

    // reset button text
    scriptButton.setText("Activate")

    // save new status
    active = false
  }

  /**
   * Return XML for simulation config to save script and plugin status.
   * @return [[org.jdom.Element]]-Collection of plugin XML configuration
   */
  override def getConfigXML(): java.util.Collection[org.jdom.Element] = {
    // save script text
    val script = new org.jdom.Element("script").setText(scriptCode.getText)

    // save plugin status
    val active = new org.jdom.Element("active").setText(isActive.toString)

    // return list of elements (implicitly converted to java collection)
    List(script, active)
  }

  /**
   * Load test code and plugin status from config XML. 
   * Activates plugin automatically when run without GUI.
   * @param configXML [[org.jdom.Element]]-Collection of plugin XML configuration
   * @param visAvailable `true` when run with GUI
   * @return always `true`
   */
  override def setConfigXML(configXML: java.util.Collection[org.jdom.Element], visAvailable: Boolean): Boolean = {
    // read all XML elements
    for(e <- configXML) {
      // load plugin text
      if(e.getName == "script" && !e.getText.isEmpty) scriptCode.setText(e.getText)

      // load plugin status
      if(e.getName == "active" && GUI.isVisualized) activate()
    }

    // auto-activate when run without GUI
    if(!GUI.isVisualized) {
      activate()
    }

    // we never fail
    true
  }
  
  /**
   * Clean up plugin when closed.
   */
  override def closePlugin() { 
    logger.debug("closePlugin() called")
    // TODO: call cleanup-callbacks
  }  
}
