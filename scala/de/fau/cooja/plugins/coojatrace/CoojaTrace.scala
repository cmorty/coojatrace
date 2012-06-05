/*
 * Copyright (c) 2011, Florian Lukas
 * All rights reserved.
 * 
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions are met:
 * 1. Redistributions of source code must retain the above copyright notice,
 * this list of conditions and the following disclaimer. 2. Redistributions in
 * binary form must reproduce the above copyright notice, this list of
 * conditions and the following disclaimer in the documentation and/or other
 * materials provided with the distribution.
 *
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" 
 * AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE 
 * IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE 
 * ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE 
 * LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
 * CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF
 * SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS 
 * INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN 
 * CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) 
 * ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
 * POSSIBILITY OF SUCH DAMAGE.
 */

package de.fau.cooja.plugins.coojatrace



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
import generator._



// speed up compilation
class CoojaTrace



/**
 * 
 * The CoojaTrace Cooja-Plugin.
 * 
 * Creates a scala interpreter and runs the test script to initialize rules, operators and signals
 * of the CoojaTrace framework. Can be run without GUI.
 *
 * @author Florian Lukas [[mailto:florian.lukas@e-technik.stud.uni-erlangen.de]]
 */
@ClassDescription("CoojaTrace")
@PluginType(PluginType.SIM_PLUGIN) // comment out this line to prevent scaladoc bug
class CoojaTracePlugin(val sim: Simulation, val gui: GUI) extends VisPlugin("CoojaTrace", gui, false) {
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
  val scriptButton = new JButton("Activate")

  /**
   * The scala interpreter.
   * Initialized by `createInterpreter`
   */
  private var interpreter: scala.tools.nsc.interpreter.IMain = null

  /**
   * Status variable showing whether script has been run and is active.
   */
  private var active = false

  /**
   * List of callback functions to call when deactivating plugin.
   */
  private var cleanUpCallBacks = List[() => Unit]()

  /**
   * PrintWriter for interpreter output, goes to System.out (with autoflush)
   */
  private var errorWriter = new StringWriter
  private var pwriter = new PrintWriter(errorWriter, true)
  
  // Constructor:
  // create Swing elements if run with GUI
  if(GUI.isVisualized) createGUI()

  /**
   * Window for API reference.
   */
  private lazy val referenceWindow = new JInternalFrame("CoojaTrace Reference", true, true, true, true) {
    val editorPane = new JEditorPane()
    editorPane.setEditable(false)

    val referenceURL = this.getClass.getResource("/CoojaTraceReference.html")
    editorPane.setPage(referenceURL)

    add(new JScrollPane(editorPane))
    setSize(600, 600)
    setDefaultCloseOperation(javax.swing.WindowConstants.HIDE_ON_CLOSE)
    sim.getGUI.getDesktopPane.add(this)
  }

  /**
   * Script code generator.
   */
  private lazy val generatorWindow = new GeneratorWindow(this)
  
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

    // add generator button
    val generatorButton = new JButton("Generator...")
    generatorButton.addActionListener(new ActionListener() {
      def actionPerformed(e: ActionEvent) {
        generatorWindow.show()
        try {
          generatorWindow.setSelected(true)
        } catch {
          case e: java.beans.PropertyVetoException => // ignore
        }
      }
    })

    // add reference button
    val referenceButton = new JButton("Reference...")
    referenceButton.addActionListener(new ActionListener() {
      def actionPerformed(e: ActionEvent) {
        referenceWindow.show()
        try {
          referenceWindow.setSelected(true)
        } catch {
          case e: java.beans.PropertyVetoException => // ignore
        }
      }
    })

    // add elements to window
    val buttonPanel = new JPanel()
    buttonPanel.add(scriptButton)
    buttonPanel.add(generatorButton)
    buttonPanel.add(referenceButton)
    add(buttonPanel, BorderLayout.PAGE_END)
    add(new JScrollPane(scriptCode), BorderLayout.CENTER)
    setSize(600,400)
  
    // Propose to reload simulation when another classloader besides our own and cooja's is found
    val problem = sim.getMotes.exists { mote =>
      (mote.getClass.getClassLoader != this.getClass.getClassLoader) &&
      (mote.getClass.getClassLoader != gui.getClass.getClassLoader)
    }
    if(problem == true) {
      val s1 = "Reload"
      val s2 = "Ignore"
      val options = Array[Object](s1, s2)
      val n = JOptionPane.showOptionDialog(
                GUI.getTopParentContainer(),
                "The CoojaTrace plugin was loaded after other plugins. This can lead to classloader inconsistencies.\nDo you want to reload the simulation to fix this?",
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
    import scala.tools.nsc.interpreter._
    
    GUI.setProgressMessage("Compiling Scala"); 
    
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
      "org.apache.log4j.Logger", "org.jdom.Element", "se.sics.cooja.GUI"
    )
    
    // assemble JAR path
    val coojaLibs = classes.map(jarPathOfClass)
    val classloader = sim.getGUI.projectDirClassLoader.asInstanceOf[java.net.URLClassLoader]
    val dynamicLibs = classloader.getURLs.map(_.toString.replace("file:", "")).toList
    val classPath = coojaLibs ::: (dynamicLibs.filter(! _.endsWith("scala-compiler.jar")))
    settings.bootclasspath.value = classPath.mkString(":")
    
    // create new scala interpreter with classpath and write output to System.out
    new IMain(settings, pwriter)
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

    // import cooja and coojatrace classes
    interpreter.interpret("""
      import reactive._

      import se.sics.cooja._
      import interfaces._
      
      import de.fau.cooja.plugins.coojatrace._
      import wrappers._
      import interfacewrappers._
      import magicsignals._
      import memorywrappers._
      import rules.assertions._
      import rules.logrules._
      import operators._
    """)
    
    // load and register contikimote wrappers if available
    try {
      Class.forName("se.sics.cooja.contikimote.ContikiMote", false, sim.getGUI.projectDirClassLoader)

      interpreter.interpret("""
        import contikiwrappers._
        contikiwrappers.register()
      """)
    } catch {
      case e: Exception => logger.warn("ContikiMote wrappers not loaded.")
    }

    // load and register mspmote wrappers if available
    try {
      Class.forName("se.sics.cooja.mspmote.MspMote", false, sim.getGUI.projectDirClassLoader)

      interpreter.interpret("""
        import mspwrappers._
        mspwrappers.register()
      """)
    } catch {
      case e: Exception => logger.warn("MspMote wrappers not loaded.")
    }

    // make the simulation object available to test code
    interpreter.bind("sim", sim.getClass.getName, sim)
    
    // define implicit vals for simulation, observing context, and MagicSignal deplog
    interpreter.interpret("""
      implicit val _simulation = sim
      implicit val _observing = new Observing {}
      implicit val _dyndeplog = new de.fau.cooja.plugins.coojatrace.magicsignals.DynamicDepLogger

      Console.setOut(System.out)
    """)

    // ignore output so far
    errorWriter.getBuffer.setLength(0)
    
    // interpret test script
    val res = interpreter.interpret(scriptCode.getText())

    if(res == scala.tools.nsc.interpreter.Results.Success) {
      // success, change status
      logger.info("Script active")
      scriptButton.setText("Reset")
      scriptCode.setEnabled(false)
      active = true
    }
    else if(res == scala.tools.nsc.interpreter.Results.Incomplete) {
      // incomplete, show warning dialog
      logger.error("Script incomplete!")

      if(GUI.isVisualized) JOptionPane.showMessageDialog(GUI.getTopParentContainer,
        "The test script is incomplete.\n\n" +
        "This is most likely caused by an unmatched open (, [ or \"",
        "Script incomplete", JOptionPane.WARNING_MESSAGE)
    }
    else {
      // error, show error dialog
      pwriter.flush()
      val msg = errorWriter.toString

      logger.error("Script error: " + msg)
      if(GUI.isVisualized) {
        val e = new Exception("Scala compilation error") {
          override def printStackTrace(stream: PrintStream) {
            stream.print(msg)
          }
        }
        GUI.showErrorDialog(this, "Scala compilation error", e, false)
        deactivate()
      }
    }
  }

  /**
   * Perform given function when deactivating plugin. 
   *
   * @param f action to be performed on plugin deactivation
   */
  def onCleanUp(f: => Unit) {
    cleanUpCallBacks ::= f _
  }

  /**
   * Deactivate plugin by resetting and deleting interpreter.
   */
  def deactivate() {
    // call cleanup-callbacks and clear them
    for(f <- cleanUpCallBacks) f()
    cleanUpCallBacks = Nil

    // clear mote conversion cache
    RichMote.clearCache()

    // reset interpreter (clears all defined objects)
    if(interpreter != null) interpreter.reset() // necessary?

    // delete interpreter instance
    interpreter = null

    // reset button text
    scriptButton.setText("Activate")

    // enable text field
    scriptCode.setEnabled(true)

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

    // check if script runner plugin would be initialized before this
    val startedPlugins = gui.getStartedPlugins
    val scriptRunner = gui.getPlugin("se.sics.cooja.plugins.ScriptRunner")
    if((scriptRunner != null) && (startedPlugins.indexOf(this) > startedPlugins.indexOf(scriptRunner))) {
      logger.warn("CoojaTrace plugin will be initialized after the cooja test (script runner) plugin!")
      JOptionPane.showMessageDialog(GUI.getTopParentContainer,
        "The CoojaTrace plugin will be initialized after the cooja test (script runner) plugin.\n\n" +
        "Running this simulation without GUI will start the simulation before the CoojaTrace plugin is initalized.\n" +
        "To fix this, close the cooja test (script runner) plugin, open it again and resave the simulation.\n" +
        "This will change plugin initialization order.",
        "Plugin order warning", JOptionPane.WARNING_MESSAGE)
    }

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
      if(e.getName == "active" && e.getText.toBoolean == true) activate()
    }

    // auto-activate when run without GUI
    if(!GUI.isVisualized) {
      activate()
    }

    // never fails
    true
  }
  
  /**
   * Clean up plugin when closed.
   */
  override def closePlugin() { 
    deactivate()
  }  
}

/**
 * Some utility methods for CoojaTracePlugin.
 */
object CoojaTracePlugin {
  /**
   * Get the CoojaTracePlugin instance for a given simulation.
   *
   * @param sim simulation whose plugin will be returned
   * @return instance of CoojaTracePlugin in sim
   */
  def forSim(sim: Simulation): CoojaTracePlugin =
    sim.getGUI.getPlugin("CoojaTracePlugin").asInstanceOf[CoojaTracePlugin] 
}
