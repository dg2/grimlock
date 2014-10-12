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

package grimlock

import scala.util.matching.Regex

import com.twitter.scalding._
import com.twitter.scalding.TDsl._, Dsl._
import cascading.flow.FlowDef

import grimlock.position._

/**
 * Rich wrapper around a `TypedPipe[(`[[position.Position]]`, Long)]`.
 *
 * @param data `TypedPipe[(`[[position.Position]]`, Long)]`.
 *
 * @note This class represents the names along the dimensions of a [[Matrix]].
 */
class Names[P <: Position](data: TypedPipe[(P, Long)]) {
  /** Renumber the names. */
  def renumber(): TypedPipe[(P, Long)] = {
    Names.number(data.map { case (p, i) => p })
  }

  private def slice(keep: Boolean, f: P => Boolean): TypedPipe[(P, Long)] = {
    Names.number(data.collect { case (p, i) if !keep ^ f(p) => p })
  }

  /**
   * Slice the names using a regular expression.
   *
   * @param regex     The regular expression to match on.
   * @param keep      Indicator if the matched names should be kept or removed.
   * @param spearator Separator used to convert each [[position.Position]] to
   *                  string.
   *
   * @return A `TypedPipe[(P, Long)]` with only the names of interest.
   *
   * @note The matching is done by convertion each [[position.Position]] to its
   *       short string reprensentation and then applying the regular
   *       expression.
   */
  def slice(regex: Regex, keep: Boolean,
    separator: String): TypedPipe[(P, Long)] = {
    slice(keep, p => regex.pattern.matcher(p.toShortString(separator)).matches)
  }

  /**
   * Slice the names using `positions`.
   *
   * @param positions The positions to slice on.
   * @param keep      Indicator if the matched names should be kept or removed.
   *
   * @return A `TypedPipe[(P, Long)]` with only the names of interest.
   *
   * @see [[position.PositionListable]]
   */
  def slice[T](positions: T, keep: Boolean)(
    implicit ev: PositionListable[T, P]): TypedPipe[(P, Long)] = {
    slice(keep, p => ev.convert(positions).contains(p))
  }

  /**
   * Set the index of a single [[position.Position]].
   *
   * @param position The position whose index is to be set.
   * @param index    The new index.
   *
   * @return A `TypedPipe[(P, Long)]` with only `position`'s index updated.
   *
   * @see [[position.Positionable]]
   */
  def set[T](position: T, index: Long)(
    implicit ev: Positionable[T, P]): TypedPipe[(P, Long)] = {
    set(Map(position -> index))
  }

  /**
   * Set the index of multiple [[position.Position]].
   *
   * @param positions A `Map` of positions (together with their new inxed)
   *        whose index is to be set.
   *
   * @return A `TypedPipe[(P, Long)]` with only `positions`' index updated.
   *
   * @see [[position.Positionable]]
   */
  def set[T](positions: Map[T, Long])(
    implicit ev: Positionable[T, P]): TypedPipe[(P, Long)] = {
    val converted = positions.map { case (k, v) => ev.convert(k) -> v }

    data.map { case (p, i) => (p, converted.getOrElse(p, i)) }
  }

  /**
   * Renumber the indices such that `position` is first.
   *
   * @param position The position to move to the front.
   *
   * @return A `TypedPipe[(P, Long)]` with `position` at index 0 and all
   *         others renumbered but preserving their relative ordering.
   *
   * @see [[position.Positionable]]
   */
  def moveToFront[T](position: T)(
    implicit ev: Positionable[T, P]): TypedPipe[(P, Long)] = {
    data.map {
      case (p, i) => (p, if (p == ev.convert(position)) 0 else i + 1)
    }
  }

  /**
   * Renumber the indices such that `position` is last.
   *
   * @param position The position to move to the back.
   *
   * @return A `TypedPipe[(P, Long)]` with `position` at the greates index and
   *         all others renumbered but preserving their relative ordering.
   *
   * @see [[position.Positionable]]
   */
  def moveToBack[T](position: T)(
    implicit ev: Positionable[T, P]): TypedPipe[(P, Long)] = {
    val state = data
      .map { case (p, i) => Map(p -> i) }
      .sum
      .map {
        case m => Map("max" -> m.values.max, "curr" -> m(ev.convert(position)))
      }

    data
      .flatMapWithValue(state) {
        case ((p, i), so) => so.map {
          case s => (p,
            if (s("curr") < i) i - 1
            else if (p == ev.convert(position)) s("max")
            else i)
        }
      }
  }

  /**
   * Persist [[Names]] to disk.
   *
   * @param file        Name of the output file.
   * @param separator   Separator to use between [[position.Position]] and
   *                    index.
   * @param descriptive Indicates if the output should be descriptive.
   *
   * @return A Scalding `TypedPipe[(P, Long)]` which is this [[Names]].
   */
  def persist(file: String, separator: String = "|",
    descriptive: Boolean = false)(implicit flow: FlowDef,
      mode: Mode): TypedPipe[(P, Long)] = {
    data
      .map {
        case (p, i) => descriptive match {
          case true => p.toString + separator + i.toString
          case false => p.toShortString(separator) + separator + i.toString
        }
      }
      .toPipe('line)
      .write(TextLine(file))

    data
  }
}

object Names {
  /**
   * Conversion from `TypedPipe[(`[[position.Position]]`, Long)]` to a
   * [[Names]].
   */
  implicit def typedPipePositionLong[P <: Position](
    data: TypedPipe[(P, Long)]): Names[P] = {
    new Names(data)
  }

  /**
   * Number of `TypedPipe[`[[position.Position]]`]`
   *
   * @param data `TypedPipe` of [[position.Position]] to number.
   *
   * @return A `TypedPipe[(`[[position.Position]]`, Long)]`.
   *
   * @note No ordering is defined on the indices for each
   *       [[position.Position]], but each index will be unique.
   */
  def number[P <: Position](data: TypedPipe[P]): TypedPipe[(P, Long)] = {
    data
      .groupAll
      .mapValueStream { _.zipWithIndex }
      .map { case ((), (p, i)) => (p, i) }
  }
}

/** Type class for transforming a type `T` into a `TypedPipe[(Q, Long)]`. */
trait Nameable[T, P <: Position, Q <: Position, D <: Dimension] {
  /**
   * Returns a `TypedPipe[(Q, Long)]` for type `T`.
   *
   * @param t Object that can be converted to a `TypedPipe[(Q, Long)]`.
   */
  def convert(m: Matrix[P], s: Slice[P, D], t: T): TypedPipe[(Q, Long)]
}

object Nameable {
  /**
   * Converts a `TypedPipe[(Q, Long)]` into a `TypedPipe[(Q, Long)]`; that is,
   * it is a pass through.
   */
  implicit def NamesNameable[P <: Position, Q <: Position, D <: Dimension]: Nameable[TypedPipe[(Q, Long)], P, Q, D] = {
    new Nameable[TypedPipe[(Q, Long)], P, Q, D] {
      def convert(m: Matrix[P], s: Slice[P, D],
        t: TypedPipe[(Q, Long)]): TypedPipe[(Q, Long)] = t
    }
  }
  /** Converts a `TypedPipe[Q]` into a `TypedPipe[(Q, Long)]`. */
  implicit def PositionPipeNameable[P <: Position, Q <: Position, D <: Dimension]: Nameable[TypedPipe[Q], P, Q, D] = {
    new Nameable[TypedPipe[Q], P, Q, D] {
      def convert(m: Matrix[P], s: Slice[P, D],
        t: TypedPipe[Q]): TypedPipe[(Q, Long)] = Names.number(t)
    }
  }
  /** Converts a [[position.PositionListable]] into a `TypedPipe[(Q, Long)]`. */
  implicit def PositionListableNameable[T, P <: Position, Q <: Position, D <: Dimension](implicit ev1: PositionListable[T, Q],
    ev2: PosDimDep[P, D]): Nameable[T, P, Q, D] = {
    new Nameable[T, P, Q, D] {
      def convert(m: Matrix[P], s: Slice[P, D], t: T): TypedPipe[(Q, Long)] = {
        // TODO: Is there a way not to use asInstanceOf?
        new Names(m.names(s).asInstanceOf[TypedPipe[(Q, Long)]]).slice(t, true)
      }
    }
  }
}

