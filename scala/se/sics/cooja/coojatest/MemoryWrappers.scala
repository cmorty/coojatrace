package se.sics.cooja.coojatest.memorywrappers



import reactive._

import java.util.{Observable, Observer}

import se.sics.cooja._
import se.sics.cooja.interfaces._

import se.sics.cooja.coojatest.wrappers._



/**
 * Generic memory variable type.
 *
 * @tparam T Scala type to use for this type
 */
trait MemVarType[+T] {
  /**
   * Name of type (usually as in C).
   */
  val name: String

  /**
   * Get the size of one variable of this type)
   * @param mem mote memory in which variable is found
   * @return size of one variable of this type in bytes
   */
  def size(mem: RichMoteMemory) : Int

  /**
   * Get signal for variable of this type.
   * @param addr address of variable in mote memory
   * @param mem mote memory in which variable is found
   * @return signal for memory variable
   */
  def get(addr: Int, mem: RichMoteMemory): Signal[T]
}



/**
 * Type for integer (int) variables.
 */
object IntVarType extends MemVarType[Int] {
  val name = "int"
  def size(mem: RichMoteMemory) = mem.memory.getIntegerLength
  def get(addr: Int, mem: RichMoteMemory) = {
    val logger = org.apache.log4j.Logger.getLogger(this.getClass) // DEBUG
    logger.debug("^^ getting int @ "+ addr) // DEBUG
    mem.addVariable(addr, this)
  }
}

// TODO
/**
 * Type for pointer (*void) variables.
 */
object PointerVarType extends MemVarType[Int] {
  val name = "*void"
  def size(mem: RichMoteMemory) = mem.memory.getIntegerLength
  def get(addr: Int, mem: RichMoteMemory) = {
    val logger = org.apache.log4j.Logger.getLogger(this.getClass) // DEBUG
    logger.debug("^^ getting ptr @ "+ addr) // DEBUG
    mem.addVariable(addr, this)
  }
}

/**
 * Type for byte (char) variables.
 */
object ByteVarType extends MemVarType[Byte] {
  val name = "char"
  def size(mem: RichMoteMemory) = 1
  def get(addr: Int, mem: RichMoteMemory) = {
    val logger = org.apache.log4j.Logger.getLogger(this.getClass) // DEBUG
    logger.debug("^^ getting byte @ "+ addr) // DEBUG
    mem.addVariable(addr, this)
  }
}


/**
 * Aliases for memory variable types to their C names.
 * Can be used after importing: `import CTypeNames._`
 */
object CTypeNames {
  val int = IntVarType
  val char = ByteVarType
}


/*
case class MemSignal[+T](addr: Int, sig: Signal[T]) extends Signal[T] {
  def now = sig.now
  lazy val change = sig.change
}
*/


/**
 * Memory variable. Can be used as a signal.
 *
 * @param addr [[Signal]] for address of variable. Changes to address update value instantly
 * @param typ [[se.sics.cooja.coojatest.memorywrappers.MemVarType]] of memory variable
 * @param mem mote memory in which variable is found
 * @tparam T scala type of memory variable
 */
case class MemVar[+T](addr: Signal[Int], typ: MemVarType[T], mem: RichMoteMemory)
    extends Signal[T] {
  /**
   * Signal of variable at '''current''' address.
   */
  lazy val varSig: Signal[T] = addr.distinct.flatMap(a => typ.get(a, mem))

  // behave like current variable signal
  def now = varSig.now
  lazy val change = varSig.change

  val logger = org.apache.log4j.Logger.getLogger(this.getClass) // DEBUG
  logger.debug("+++ created new MemVar: " + this) //DEBUG

  // TODO: DOC
  def toPointer[N](newTyp: MemVarType[N])(implicit evidence: T <:< Int): MemPointer[N] = {
   logger.debug(">>> creating pointer from " + this) // DEBUG
    MemPointer[N](varSig.asInstanceOf[Signal[Int]], newTyp, mem)
  }
   

  // useful output
  override def toString = super.toString + " (" + typ.name + " @ " + addr.now + " = " + now + ")"
}



/**
 * Pointer to memory variable. Can be used as a signal for variable address.
 *
 * @param addr [[Signal]] for address this pointer '''points at'''. '''Not address of pointer!'''
 * @param typ [[se.sics.cooja.coojatest.memorywrappers.MemVarType]] of target variable
 * @param mem mote memory in which target variable  is found
 * @tparam T scala type of target variable
 */
case class MemPointer[+T](addr: Signal[Int], typ: MemVarType[T], mem: RichMoteMemory)
    extends Signal[Int] {
  // behave like addr signal
  def now = addr.now
  lazy val change = addr.change

  val logger = org.apache.log4j.Logger.getLogger(this.getClass) // DEBUG
  logger.debug("+++ created new MemPointer: " + this) //DEBUG

  // TODO: DOC
  def dereference[N](t: MemVarType[N]): MemVar[N] = {
    logger.debug("<<< dereferencing " + this)// DEBUG
    MemVar[N](addr, t, mem)
  }

  /**
   * Add offset to pointer (C pointer arithmetic).
   * @param offset offset to add to pointer in multiples of variable type size, not bytes!
   * @return new [[MemPointer]] with modified address (signal)
   */
  def +(offset: Int) = MemPointer[T](addr.map(_ + offset*typ.size(mem)), typ, mem)
  
  /**
   * Subtract offset from pointer (C pointer arithmetic).
   * @param offset offset to subtract from pointer in multiples of variable type size, not bytes!
   * @return new [[MemPointer]] with modified address (signal)
   */
  def -(offset: Int) = MemPointer[T](addr.map(_ - offset*typ.size(mem)), typ, mem)

  // useful output
  override def toString = super.toString + "( *" + typ.name + " = " + addr.now + ")"
}

/**
 * Functions for referencing and dereferencing [[MemPointer]]s.
 */
object MemPointer {
  /**
   * Get pointer pointing at given variable.
   * @param v [[MemVar]] at which new pointer will point
   * @return new [[MemPointer]] pointing at given variable
   * @tparam T scala type of memory variable
   */
  def &[T](v: MemVar[T]): MemPointer[T] = MemPointer(v.addr, v.typ, v.mem) 

  /**
   * Get variable (signal) by dereferencing pointer. Variable type is explicitly given.
   * @param p [[MemPointer]] which points at address of variable to get
   * @param t [[MemVarType]] new type of created memory variable (cast)
   * @return new [[MemVar]] found at address given by pointer
   * @tparam T scala type of memory variable
   */
  def *[T](p: MemPointer[_], t: MemVarType[T]): MemVar[T] = p.dereference(t) 

  /**
   * Get variable (signal) by dereferencing pointer. Variable type is taken from pointer.
   * @param p [[MemPointer]] which points at address of variable to get
   * @return new [[MemVar]] found at address given by pointer
   * @tparam T scala type of memory variable
   */
  def *[T](p: MemPointer[T]): MemVar[T] = *(p, p.typ)

  // NEW AND DANGEROUS
  def ptr[T](v: MemVar[Int], typ: MemVarType[T]): MemPointer[T] = v.toPointer[T](typ)
}


/*
// TODO: DOC
case class MemArray(addr: Signal[Int], len: Int, mem: MoteMemory) extends Signal[Array[Byte]] {
  val typ = ByteVarType

  /**
   * Signal of array at '''current''' address.
   */
  val arrSig = addr.flatMap(a => mem.variable[](addr))

  // behave like current array signal
  def now = arrSig.now
  lazy val change = arrSig.change
  
  // TODO: DOC
  def apply(idx: Int) = MemVar(addr.map(_ + idx*typ.size), typ, mem)

  // useful output
  override def toString = typ.name + "["+ len + "] @ " + addr.now + " = " + now.mkString(", ")
}
*/

/**
 * Generic mote memory wrapper.
 */
trait RichMoteMemory {
  /**
   * the wrapped memory.
   */
  def memory: AddressMemory

  val logger = org.apache.log4j.Logger.getLogger(this.getClass)  // DEBUG

  protected val variables = collection.mutable.WeakHashMap[MemVar[_], Null]()

  protected[memorywrappers] def addVariable[T](addr: Int, typ: MemVarType[T]): Signal[T] =
    variables.keys.find { 
      k => (k.addr == Val(addr)) && (k.typ == typ)
    }.getOrElse {
      logger.warn("DID NOT FIND MemVar(Val(" + addr + ", " + typ + " IN: ")
      for(m @ MemVar(addr, typ, mem) <- variables.keys) logger.warn("-" + m + " (" + addr + ", "+ typ+")")
      val s = typ match { // DEBUG: other way round... addVar(addr, typ)
        case it: IntVarType.type => addIntVar(addr)
        case bt: ByteVarType.type => addByteVar(addr)
        case pt: PointerVarType.type => addPointerVar(addr)
      }
      var v = new MemVar(Val(addr), typ, this) {
        override lazy val varSig: Signal[T] = s.asInstanceOf[Signal[T]]
      }
      variables(v) = null
      logger.debug("** addVar: " + variables) // DEBUG
      v //s?
    }.asInstanceOf[Signal[T]]

  def variable[T](addr: Int, typ: MemVarType[T]): MemVar[T] = 
    variables.keys.find { 
      k => (k.addr == Val(addr)) && (k.typ == typ)
    }.getOrElse {
      val v = MemVar(Val(addr), typ, this)
      variables(v) = null
      logger.debug("** variable: " + variables) // DEBUG
      v
    }.asInstanceOf[MemVar[T]]


  /**
   * Get signal of memory variable.
   * @param name name of variable
   * @return [[Signal]] with value of variable
   * @tparam scala type of variable
   */
  def variable[T](name: String, typ: MemVarType[T]): MemVar[T] =
    variable[T](memory.getVariableAddress(name), typ)

  def intVar(name: String) = variable(name, IntVarType)
  def intVar(addr: Int) = variable(addr, IntVarType)
  def byteVar(name: String) = variable(name, ByteVarType)
  def byteVar(addr: Int) = variable(addr, ByteVarType)


  /**
   * Create signal of memory int variable.
   * @param addr address of int variable
   * @return [[Signal]] with value of int variable
   */
  protected[memorywrappers] def addIntVar(addr: Int): Signal[Int]

  /**
   * Create signal of memory byte variable.
   * @param addr address of byte variable
   * @return [[Signal]] with value of byte variable
   */
  protected[memorywrappers] def addByteVar(addr: Int): Signal[Byte]


  // TODO
  protected[memorywrappers] def addPointerVar(addr: Int): Signal[Int]


  /**
   * Get value of byte variable.
   * @param name name of variable
   * @return byte value of variable
   */
  def byte(name: String) = memory.getByteValueOf(name)

  /**
   * Get value of byte variable at address.
   *
   * '''Note:''' this works only for mote types using SectionMoteMemory, and will throw an
   * exception otherwise. This method should therefore be overriden in other mote type wrappers.
   * @param addr address of variable
   * @return byte value of variable
   */
  def byte(addr: Int) = memory.asInstanceOf[SectionMoteMemory].getMemorySegment(addr, 1)(0)

  /**
   * Get value of integer variable.
   * @param name name of variable
   * @return integer value of variable
   */
  def int(name: String) = memory.getIntValueOf(name)
  
  /**
   * Get value of integer variable at address.
   *
   * '''Note:''' this works only for mote types using SectionMoteMemory, and will throw an
   * exception otherwise. This method should therefore be overriden in other mote type wrappers.
   * @param addr address of variable
   * @return integer value of variable
   */
  def int(addr: Int) = {
    val bytes = memory.asInstanceOf[SectionMoteMemory].getMemorySegment(addr, 4).map(_ & 0xFF)
    val retVal = ((bytes(0) & 0xFF) << 24) + ((bytes(1) & 0xFF) << 16) +
                 ((bytes(2) & 0xFF) << 8) + (bytes(3) & 0xFF) // TODO: int length
    val r= Integer.reverseBytes(retVal)
    logger.debug("VV read int @ " + addr + " = " + r); r // DEBUG
  }

  // TODO
  def pointer(addr: Int) = int(addr)
  def pointer(name: String) = int(name)

  /**
   * Get byte array of specified length.
   * @param name name of array
   * @param length length of array in bytes
   * @return byte array from memory
   */
  def array(name: String, length: Int) = memory.getByteArray(name, length)

  // TODO: DOC
  def array(addr: Int, length: Int) = memory.asInstanceOf[SectionMoteMemory].getMemorySegment(addr, length)
}
