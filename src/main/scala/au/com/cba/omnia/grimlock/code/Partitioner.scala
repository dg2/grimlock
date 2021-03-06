// Copyright 2014-2015 Commonwealth Bank of Australia
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

package au.com.cba.omnia.grimlock.partition

import au.com.cba.omnia.grimlock._
import au.com.cba.omnia.grimlock.content._
import au.com.cba.omnia.grimlock.position._
import au.com.cba.omnia.grimlock.utility._

import com.twitter.scalding._
import com.twitter.scalding.typed.Grouped

/** Base trait for partitioning operations. */
trait Partitioner {
  /** Type of the partition assignments. */
  type T
}

/** Base trait for partitioners. */
trait Assign extends AssignWithValue { self: Partitioner =>
  type V = Any

  def assign[P <: Position](pos: P, ext: V): Collection[T] = assign(pos)

  /**
   * Assign the cell to a partition.
   *
   * @param pos The position of the content.
   *
   * @return Optional of either a `T` or a `List[T]`, where the instances of `T` identify the partitions.
   */
  def assign[P <: Position](pos: P): Collection[T]
}

/** Base trait for partitioners that use a user supplied value. */
trait AssignWithValue { self: Partitioner =>
  /** Type of the external value. */
  type V

  /**
   * Assign the cell to a partition using a user supplied value.
   *
   * @param pos The position of the content.
   * @param ext The user supplied value.
   *
   * @return Optional of either a `T` or a `List[T]`, where the instances of `T` identify the partitions.
   */
  def assign[P <: Position](pos: P, ext: V): Collection[T]
}

/**
 * Rich wrapper around a `TypedPipe[(T, (Position, Content))]`.
 *
 * @param data The `TypedPipe[(T, (Position, Content))]`.
 */
class Partitions[T: Ordering, P <: Position](
  protected val data: TypedPipe[(T, Cell[P])]) extends Persist[(T, Cell[P])] {
  /** Return the partition identifiers. */
  def keys(): TypedPipe[T] = Grouped(data).keys

  /**
   * Return the data for the partition `key`.
   *
   * @param key The partition for which to get the data.
   *
   * @return A `TypedPipe[Cell[P]]`; that is a matrix.
   */
  def get(key: T): TypedPipe[Cell[P]] = data.collect { case (t, pc) if (key == t) => pc }

  /**
   * Add a partition.
   *
   * @param key       The partition identifier.
   * @param partition The partition to add.
   *
   * @return A `TypedPipe[(T, Cell[P])]` containing existing and new paritions.
   */
  def add(key: T, partition: TypedPipe[Cell[P]]): TypedPipe[(T, Cell[P])] = {
    data ++ (partition.map { case c => (key, c) })
  }

  /**
   * Remove a partition.
   *
   * @param key The identifier for the partition to remove.
   *
   * @return A `TypedPipe[(T, Cell[P])]` with the selected parition removed.
   */
  def remove(key: T): TypedPipe[(T, Cell[P])] = data.filter { case (t, c) => t != key }

  /**
   * Apply function `fn` to each partition in `keys`.
   *
   * @param keys The list of partitions to apply `fn` to.
   * @param fn   The function to apply to each partition.
   *
   * @return A `TypedPipe[(T, Cell[Q])]` containing the paritions in `keys` with `fn` applied to them.
   */
  def foreach[Q <: Position](keys: List[T],
    fn: (T, TypedPipe[Cell[P]]) => TypedPipe[Cell[Q]]): TypedPipe[(T, Cell[Q])] = {
    import Partitions._

    // TODO: This reads the data keys.length times. Is there a way to read it only once?
    //       Perhaps with Grouped.mapGroup and Execution[T]?
    keys
      .map { case k => fn(k, data.get(k)).map { case c => (k, c) } }
      .reduce[TypedPipe[(T, Cell[Q])]]((x, y) => x ++ y)
  }

  protected def toString(t: (T, Cell[P]), separator: String, descriptive: Boolean): String = {
    t._1.toString + separator + t._2.toString(separator, descriptive)
  }
}

object Partitions {
  /** Conversion from `TypedPipe[(T, (Position, Content))]` to a `Partitions`. */
  implicit def TPTPC2P[T: Ordering, P <: Position](data: TypedPipe[(T, Cell[P])]): Partitions[T, P] = {
    new Partitions(data)
  }
}

