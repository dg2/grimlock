// Copyright 2014 Commonwealth Bank of Australia
//
// Licensed under the Apache License, Version 2.0 (the "License");
// you may not use this file except in compliance with the License.
// You may obtain a copy of the License at
//
// http://www.apache.org/licenses/LICENSE-2.0
//
// Unless required by applicable law or agreed to in writing, software
// distributed under the License is distributed on an "AS IS" BASIS,
// WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
// See the License for the specific language governing permissions and
// limitations under the License.

package grimlock.partition

import com.twitter.scalding._
import com.twitter.scalding.TDsl._, Dsl._
import cascading.flow.FlowDef

import grimlock.contents._
import grimlock.Matrix._
import grimlock.position._

/** Defines partitioning operations. */
object Partitioner {
  /**
   * Type for partitioning a [[Matrix]].
   *
   * @note An `Option` is returned to allow partitioners to be selective in
   *       which partition a [[position.Position]] is assigned to. An `Either`
   *       is returned to allow a partitioner to assign a [[position.Position]]
   *       to more than one partition.
   */
  type Partition[T, P <: Position] = (P, Option[SliceMap]) => Option[Either[T, List[T]]]
}

/**
 * Rich wrapper around a `TypedPipe[(T, (`[[position.Position]]`,
 * `[[contents.Content]]`))]`.
 *
 * @param data `TypedPipe[(T, (`[[position.Position]]`,
 *             `[[contents.Content]]`))]`.
 */
class Partitions[T: Ordering, P <: Position](
  data: TypedPipe[(T, (P, Content))]) {
  // TODO: Add 'keys'/'hasKey'/'set'/'modify' methods?
  // TODO: Add 'foreach' method - to apply function to all data for each key

  /**
   * Return the data for the partition `key`.
   *
   * @param key The partition for which to get the data.
   *
   * @return A `TypedPipe[(`[[position.Position]]`, `[[contents.Content]]`)]`.
   */
  def get(key: T): TypedPipe[(P, Content)] = {
    data.collect { case (t, pc) if (key == t) => pc }
  }

  /**
   * Persist a [[Partitions]] to disk.
   *
   * @param file        Name of the output file.
   * @param separator   Separator to use between `T`, [[position.Position]]
   *                    and [[contents.Content]].
   * @param descriptive Indicates if the output should be descriptive.
   *
   * @return A Scalding `TypedPipe[(T, (P, `[[contents.Content]]`))]` which
   *         is this [[Partitions]].
   */
  def persist(file: String, separator: String = "|",
    descriptive: Boolean = false)(implicit flow: FlowDef,
      mode: Mode): TypedPipe[(T, (P, Content))] = {
    data
      .map {
        case (t, (p, c)) => descriptive match {
          case true =>
            t.toString + separator + p.toString + separator + c.toString
          case false =>
            t.toString + separator + p.toShortString(separator) + separator +
              c.toShortString(separator)
        }
      }
      .toPipe('line)
      .write(TextLine(file))

    data
  }
}

object Partitions {
  /**
   * Conversion from `TypedPipe[(T, (`[[position.Position]]`,
   * `[[contents.Content]]`))]` to a [[Partitions]].
   */
  implicit def typedPipeTPositionContent[T: Ordering, P <: Position](
    data: TypedPipe[(T, (P, Content))]): Partitions[T, P] = {
    new Partitions(data)
  }
}

