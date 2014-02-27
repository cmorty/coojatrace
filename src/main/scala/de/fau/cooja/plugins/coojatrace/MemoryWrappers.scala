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



import reactive._

import java.util.{Observable, Observer}

import org.contikios.cooja._
import org.contikios.cooja.interfaces._

import de.fau.cooja.plugins.coojatrace.wrappers._



// speed up compilation
class MemoryWrappers



/**
 * Wrappers for accessing mote memory variables.
 */
package object memorywrappers extends memorywrappers.COperators // import C-style operators



package memorywrappers {
  
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
object CInt extends MemVarType[Int] {
  val name = "int"
  def size(mem: RichMoteMemory) = mem.memory.getIntegerLength
  def get(addr: Int, mem: RichMoteMemory): Signal[Int] = mem.addVariable(addr, this, mem.addIntVar)
}

/**
 * Type for pointer (*void) variables.
 */
object CPointer extends MemVarType[Int] {
  val name = "*void"
  def size(mem: RichMoteMemory) = mem.memory.getIntegerLength
  def get(addr: Int, mem: RichMoteMemory): Signal[Int] = mem.addVariable(addr, this, mem.addPointerVar)
}

/**
 * Type for byte (char) variables.
 */
object CByte extends MemVarType[Byte] {
  val name = "char"
  def size(mem: RichMoteMemory) = 1
  def get(addr: Int, mem: RichMoteMemory): Signal[Byte] = mem.addVariable(addr, this, mem.addByteVar)
}

/**
 * Type for (byte) array variables.
 * 
 * @param length size of array in bytes
 * @param const (optional) when `true` array does not check for changes to its contents, only changes when
 *   address is changed (uses less ressurces)
 */
case class CArray(length: Int, const: Boolean = false) extends MemVarType[Array[Byte]] {
  val name = "char["+ length + "]"
  def size(mem: RichMoteMemory) = length
  def get(addr: Int, mem: RichMoteMemory): Signal[Array[Byte]] =
    mem.addVariable(addr, this, a => mem.addArrayVar(a, length, const))
}



/**
 * Memory variable. Can be used as a signal.
 *
 * @param addr [[Signal]] for address of variable. Changes to address update value instantly
 * @param typ [[de.fau.cooja.plugins.coojatrace.memorywrappers.MemVarType]] of memory variable
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

  /**
   * Convert (cast) this variable to a pointer of given type. This works for int variables only!
   * @param newTyp type of new pointer (type of target of pointer)
   * @return new [[MemPointer]] which points at address given by value of this variable
   * @tparam N scala type of pointer type
   */
  def toPointer[N](newTyp: MemVarType[N])(implicit evidence: T <:< Int): MemPointer[N] =
    MemPointer[N](varSig.asInstanceOf[Signal[Int]], newTyp, mem)

  // useful output
  override def toString = super.toString + " (" + typ.name + " @ " + addr.now + " = " + now + ")"
}



/**
 * Pointer to memory variable. Can be used as a signal for variable address.
 *
 * @param addr [[Signal]] for address this pointer '''points at'''. '''Not address of pointer!'''
 * @param typ [[de.fau.cooja.plugins.coojatrace.memorywrappers.MemVarType]] of target variable
 * @param mem mote memory in which target variable  is found
 * @tparam T scala type of target variable
 */
case class MemPointer[+T](addr: Signal[Int], typ: MemVarType[T], mem: RichMoteMemory)
    extends Signal[Int] {
  // behave like addr signal
  def now = addr.now
  lazy val change = addr.change

  /**
   * Dereferences a pointer and returns result variable.
   * @param t type of target variable
   * @return [[MemVar]] at address given by the value of this pointer
   * @tparam scala type of target variable
   */
  def dereference[N](t: MemVarType[N]): MemVar[N] = MemVar[N](addr, t, mem)

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
trait COperators {
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

  /**
   * Convenience function for casting a variable into a pointer.
   * @param v [[MemVar]][Int] containing an address
   * @param typ type of pointer (= type of pointer target variable)
   * @return [[MemPointer]] pointing at address given by value of v
   * @tparam T scala type of pointer
   */
  def ptr[T](v: MemVar[Int], typ: MemVarType[T]): MemPointer[T] = v.toPointer[T](typ)
}



/**
 * (Byte) Array in mote memory. Can be used as a signal.
 *
 * @param addr [[Signal]] for start address of this array.
 * @param len length of array in bytes
 * @param mem mote memory in which array is found
 */
case class MemArray(addr: Signal[Int], len: Int, mem: RichMoteMemory) extends Signal[Array[Byte]] {
  val typ = CArray(len)
  /**
   * Signal of variable at '''current''' address.
   */
  lazy val arrSig: Signal[Array[Byte]] = addr.distinct.flatMap(a => typ.get(a, mem))

  // behave like current array signal
  def now = arrSig.now
  lazy val change = arrSig.change
  
  /**
   * Return element at index.
   * @param idx index in array
   * @return [[MemVar]] of element in array
   */
  def apply(idx: Int) = MemVar(addr.map(_ + idx), typ, mem)

  // useful output
  override def toString = typ.name + " @ " + addr.now + " = " + now.mkString(", ")
}


/**
 * Generic mote memory wrapper.
 */
trait RichMoteMemory {
  /**
   * the wrapped memory.
   */
  def memory: AddressMemory

  /**
   * Get mote variable names and addresses.
   * @return map of (address -> variablename) elements
   */
  lazy val varAddresses = {
    memory.getVariableNames.map {
      name => (memory.getVariableAddress(name), name)
    }.toMap
  }

  /**
   * List of already created [[MemVar]]s. Implemented a WeakHashMap with the variables as (weak) keys
   * pointing at nothing, since WeakHashMap handles all WeakReferences for us (there is no WeakList or similar).
   */
  protected val variables = collection.mutable.WeakHashMap[MemVar[_], Null]()

  /**
   * Get a new [[Signal]] for a memory variable (create and cache if not found)
   * @param addr address in memory where variable is found
   * @param typ type of variable
   * @param addFun function which takes address and returns newly created signal
   * @return [[Signal]] with value of variable
   * @tparam scala type of variable   
   */
  protected[memorywrappers] def addVariable[T](address: Int, typ: MemVarType[T], addFun: Int => Signal[T]): MemVar[T] =
    // check "cache" if signal is alredy created (as a MemVar)
    variables.keys.find { 
      k => (k.typ == typ) && (k.addr == Val(address))
    }.getOrElse {
      // not found, create underlying signal by addFun
      val sig = addFun(address).asInstanceOf[Signal[T]]
      
      // create new memvar 
      var v = new MemVar(Val(address), typ, this) {
        override lazy val varSig: Signal[T] = sig
      }

      // store in cache and return
      variables(v) = null
      v
    }.asInstanceOf[MemVar[T]]

  
  /**
   * Get a memory variable of given type at address.
   * @param addr address in memory where variable is found
   * @param typ type of variable
   * @return [[MemVar]] with value of variable
   * @tparam scala type of variable
   */
  def variable[T](addr: Int, typ: MemVarType[T]): MemVar[T] = 
    addVariable[T](addr, typ, a => typ.get(a, this))

  /**
   * Get a memory variable of given type at address.
   * @param name name of variable
   * @param typ type of variable
   * @return [[MemVar]] with value of variable
   * @tparam scala type of variable
   */
  def variable[T](name: String, typ: MemVarType[T]): MemVar[T] =
    variable[T](memory.getVariableAddress(name), typ)


  /**
   * Create signal of a memory int variable.
   * @param addr address of int variable
   * @return [[Signal]] with value of int variable
   */
  protected[memorywrappers] def addIntVar(addr: Int): Signal[Int]

  /**
   * Create signal of a memory byte variable.
   * @param addr address of byte variable
   * @return [[Signal]] with value of byte variable
   */
  protected[memorywrappers] def addByteVar(addr: Int): Signal[Byte]

  /**
   * Create signal of a memory pointer.
   * @param addr address of pointer
   * @return [[Signal]] with value (target address) of pointer
   */
  protected[memorywrappers] def addPointerVar(addr: Int): Signal[Int]

  /**
   * Create signal of a memory array.
   * @param addr address of array
   * @param len length of array in bytes
   * @param const if true, do not check for element changes
   * @return [[Signal]] with value of array
   */
  protected[memorywrappers] def addArrayVar(addr: Int, len: Int, const: Boolean): Signal[Array[Byte]]


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
   * '''Note:''' this works only for mote types using SectionMoteMemory and 32-bit integers, and will throw an
   * exception otherwise. This method should therefore be overriden in other mote type wrappers.
   * @param addr address of variable
   * @return integer value of variable
   */
  def int(addr: Int): Int = {
    val bytes = memory.asInstanceOf[SectionMoteMemory].getMemorySegment(addr, 4)
    ((bytes(3) & 0xFF) << 24) + ((bytes(2) & 0xFF) << 16) + ((bytes(1) & 0xFF) << 8) + (bytes(0) & 0xFF)
  }

  /**
   * Get value of pointer.
   * @param name name of pointer
   * @return integer value (target address) of pointer
   */
  def pointer(name: String) = int(name)

  /**
   * Get value of pointer at address.
   *
   * '''Note:''' this works only for mote types using SectionMoteMemory, and will throw an
   * exception otherwise. This method should therefore be overriden in other mote type wrappers.
   * @param addr name of pointer
   * @return integer value (target address) of pointer
   */
  def pointer(addr: Int) = int(addr)

  /**
   * Get byte array of specified length.
   * @param name name of array
   * @param length length of array in bytes
   * @return byte array from memory
   */
  def array(name: String, length: Int) = memory.getByteArray(name, length)

  /**
   * Get byte array of specified length at address.
   * @param address address of array
   * @param length length of array in bytes
   * @return byte array from memory
   */
  def array(addr: Int, length: Int) =
    memory.asInstanceOf[SectionMoteMemory].getMemorySegment(addr, length)
}

} // package memorywrappers
