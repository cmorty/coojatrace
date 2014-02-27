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

package de.fau.cooja.plugins.coojatrace.rules

import reactive._

import org.contikios.cooja.Simulation

package object logrules {
  /**
   * Create new rule to log one or more signals.
   * @param to [[LogDestination]]-object where values shall be logged to
   * @param sig [[Signal]]s to be logged. If multiple signals are given, a change in any 
   * of the signals will generate a new log-line listing ´´´all´´´ current values
   * @param sim the current [[Simulation]]
   * @param obs the observing context implicit
   */
  def log(to: LogDestination, sig: Signal[_]*)(implicit sim: Simulation, obs: Observing) { 
    require(sig.size > 0)

    val signals = sig.reverse
    val stream = signals.tail.foldLeft(signals.head.map(v => List(v.toString))) {
      case (combined, signal) => signal.flatMap(s => combined.map(v => s.toString :: v))
    }.change
    
    //Only log if there is really a change
    val distinctstream = stream.distinct
    
    // call dest.log for every change with a list of current values (as long as to is active)
    distinctstream.takeWhile(_ => to.active).foreach(to.log)
  }

  /**
   * Create new rule to log an eventstream and any number of signals to sample with.
   * @param to [[LogDestination]]-object where values shall be logged to
   * @param es [[EventStream]] to be logged. Multiple columns can be logged by passing an event
   *   strean of 
   * @param sig (optional) [[Signal]]s which will be sampled at every new event from es. Signal
   *   values will '''not''' be logged as they change, but '''only''' when es fires!
   * @param sim the current [[Simulation]]
   * @param obs the observing context implicit
   * @tparan T type of event stream to log
   */
  def log[T](to: LogDestination, es: EventStream[T], sig: Signal[_]*)(implicit sim: Simulation, m: Manifest[T], obs: Observing) {
    // pass an EventStream[List[_]] directly, otherwise map it to one-element List
    val stream = if(m <:< manifest[List[_]]) // manifests against type erasure
      es.asInstanceOf[EventStream[List[_]]].map(_ ::: sig.toList.map(_.now))
    else
      es.map(_ :: sig.toList.map(_.now))
    
    // call dest.log for every change with a list of current values (as long as to is active)
    stream.takeWhile(_ => to.active).foreach(to.log)
  }  

  /**
   * Implicit CanForward to a LogDestination. Can be used to log to a destination using
   * `destination <<: signalOrStream`.
   * @tparam T type of signal/stream
   * @return CanForward to LogDestination for type T
   */
  implicit def forwardLog[T](implicit sim: Simulation, m: Manifest[T]) =
    new CanForward[LogDestination, T] {
      def forward(s: Forwardable[T], dest: => LogDestination)(implicit o: Observing) {
        s match { // match because of overloaded log(...)
          case sig: Signal[_] => log(dest, sig)
          case es: EventStream[_] => log(dest, es.asInstanceOf[EventStream[T]])
        }
      }
    }
}
