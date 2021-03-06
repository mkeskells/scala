/*
 * Scala (https://www.scala-lang.org)
 *
 * Copyright EPFL and Lightbend, Inc.
 *
 * Licensed under Apache License 2.0
 * (http://www.apache.org/licenses/LICENSE-2.0).
 *
 * See the NOTICE file distributed with this work for
 * additional information regarding copyright ownership.
 */

package scala
package collection
package generic

import scala.collection.parallel.Combiner

/**
 *  @since 2.8
 */
trait HasNewCombiner[+T, +Repr] {
  protected[this] def newCombiner: Combiner[T, Repr]
}

