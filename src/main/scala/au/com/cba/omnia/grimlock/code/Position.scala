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

package au.com.cba.omnia.grimlock.position

import au.com.cba.omnia.grimlock._
import au.com.cba.omnia.grimlock.contents._
import au.com.cba.omnia.grimlock.contents.encoding._
import au.com.cba.omnia.grimlock.position.coordinate._

import cascading.flow.FlowDef
import com.twitter.scalding._
import com.twitter.scalding.TDsl._, Dsl._
import com.twitter.scalding.typed.IterablePipe

/** Base trait for dealing with positions. */
trait Position {
  /** Type for positions of same (S) number of dimensions. */
  type S <: Position

  /** List of [[coordinate.Coordinate]] of the position. */
  val coordinates: List[Coordinate]

  /**
   * Return the [[coordinate.Coordinate]] at dimension (index) `dim`.
   *
   * @param dim [[Dimension]] of the [[coordinate.Coordinate]] to get.
   */
  def get(dim: Dimension): Coordinate = {
    coordinates(dim.index)
  }

  /**
   * Converts the position to a consise (terse) string.
   *
   * @param separator The separator to use between the coordinates.
   *
   * @return Short string representation.
   */
  def toShortString(separator: String): String = {
    coordinates.map(_.toShortString).mkString(separator)
  }

  /**
   * Compare this object with another [[Position]].
   *
   * @param that [[Position]] to compare against.
   *
   * @return x < 0 iff this < that,
   *         x = 0 iff this = that,
   *         x > 0 iff this > that.
   *
   * @note If the comparison is between two [[Position]]s with different
   *       dimensions, then a comparison on the number of dimensions is
   *       performed.
   */
  def compare(that: Position): Int = {
    val mine = coordinates
    val theirs = that.coordinates

    (mine.length == theirs.length) match {
      case true =>
        val cmp = mine.zip(theirs).map { case (m, t) => m.compare(t) }

        cmp.indexWhere(_ != 0) match {
          case idx if (idx < 0) => 0
          case idx => cmp(idx)
        }
      case false => mine.length.compare(theirs.length)
    }
  }
}

/**
 * Trait for operations that modify a position (but keep the number of
 * dimensions the same).
 */
trait ModifyablePosition { self: Position =>
  /**
   * Update the [[coordinate.Coordinate]] at `dim` with `t`.
   *
   * @param dim The [[Dimension]] to set.
   * @param t   The [[coordinate.Coordinate]] to set.
   *
   * @return A [[Position]] of the same size as `this` but with `t` set
   *         at index `dim`.
   *
   * @see [[coordinate.Coordinateable]]
   */
  def set[T: Coordinateable](dim: Dimension, t: T): S = {
    same(coordinates.updated(dim.index,
      implicitly[Coordinateable[T]].convert(t)))
  }

  /**
   * Permute the order of [[coordinate.Coordinate]]s.
   *
   * @param order The new ordering of the [[coordinate.Coordinate]]s.
   *
   * @return A [[Position]] of the same size as `this` but with the
   *         [[coordinate.Coordinate]]s ordered according to `order`.
   *
   * @note The ordering must contain each dimension (1..N) exactly once.
   */
  def permute(order: List[Dimension]): S = {
    same(order.map { case d => coordinates(d.index) })
  }

  def merge(that: Position, pattern: String = "%sx%s"): S = {
    same(coordinates
      .zip(that.coordinates)
      .map {
        case (l, r) if (l != r) =>
          StringCoordinate(pattern.format(l.toShortString, r.toShortString),
            StringCodex)
        case (l, r) => l
      })
  }

  protected def same(cl: List[Coordinate]): S
}

/** Trait for operations that reduce a position by one dimension. */
trait ReduceablePosition { self: Position =>
  /** Type for positions of one less (L) number of dimensions. */
  type L <: Position with ExpandablePosition

  /**
   * Remove the [[coordinate.Coordinate]] at dimension `dim`.
   *
   * @param dim The [[Dimension]] to remove.
   *
   * @return A new [[Position]] with dimension `dim` removed.
   */
  def remove(dim: Dimension): L = {
    val (h, t) = coordinates.splitAt(dim.index)

    less(h ::: t.drop(1))
  }

  type O
  type A
  def toOverMapValue(r: L, c: Content): O
  def toAlongMapValue(r: Position1D, c: Content): A
  def combineOverMapValues(x: Option[O], y: O): O
  def combineAlongMapValues(x: Option[A], y: A): A

  /**
   * Melt dimension `dim` into `into`.
   *
   * @param dim       The [[Dimension]] to remove.
   * @param into      The [[Dimension]] into which to melt.
   * @param separator The separator to use in the new
   *                  [[coordinate.Coordinate]] name.
   *
   * @return A new [[Position]] with dimension `dim` removed.
   *         The [[coordinate.Coordinate]] at `unto` will be
   *         a [[coordinate.StringCoordinate]] consisting of
   *         the string representations of the
   *         [[coordinate.Coordinate]] `dim` and `unto`
   *         separated by `separator`.
   *
   * @note `dim` and `into` must not be the same.
   */
  def melt(dim: Dimension, into: Dimension, separator: String): L = {
    val (h, t) = coordinates
      .updated(into.index,
        StringCoordinate(coordinates(into.index).toShortString + separator +
          coordinates(dim.index).toShortString, StringCodex))
      .splitAt(dim.index)

    less(h ::: t.drop(1))
  }

  protected def less(cl: List[Coordinate]): L
}

/** Trait for operations that expand a position by one dimension. */
trait ExpandablePosition { self: Position =>
  /** Type for positions of one more (M) number of dimensions. */
  type M <: Position with ReduceablePosition

  /**
   * Append a [[coordinate.Coordinate]] to the position.
   *
   * @param t The [[coordinate.Coordinate]] to append.
   *
   * @return A new [[Position]] with the [[coordinate.Coordinate]] `t` appended.
   *
   * @see [[coordinate.Coordinateable]]
   */
  def append[T: Coordinateable](t: T): M = {
    more(coordinates :+ implicitly[Coordinateable[T]].convert(t))
  }

  protected def more(cl: List[Coordinate]): M
}

/**
 * Position for zero dimensions.
 *
 * @note Position0D exists so things like names(Over(1)) work.
 */
case class Position0D() extends Position with ExpandablePosition {
  type S = Position0D
  type M = Position1D

  val coordinates = List()

  def more(cl: List[Coordinate]): M = Position1D(cl(0))
}

/**
 * Position for 1 dimensional data.
 *
 * @param first [[coordinate.Coordinate]] for the first dimension.
 */
case class Position1D(first: Coordinate) extends Position
  with ModifyablePosition with ReduceablePosition with ExpandablePosition {
  type L = Position0D
  type S = Position1D
  type M = Position2D

  type O = Content
  type A = Content
  def toOverMapValue(r: L, c: Content): O = c
  def toAlongMapValue(r: Position1D, c: Content): A = c
  def combineOverMapValues(x: Option[O], y: O): O = combineMapValues(x, y)
  def combineAlongMapValues(x: Option[A], y: A): A = combineMapValues(x, y)
  private def combineMapValues(l: Option[Content], r: Content): Content = r

  val coordinates = List(first)

  protected def less(cl: List[Coordinate]): L = Position0D()
  protected def same(cl: List[Coordinate]): S = Position1D(cl(0))
  protected def more(cl: List[Coordinate]): M = Position2D(cl(0), cl(1))
}

/** Companion object to [[Position1D]]. */
object Position1D {
  /**
   * Construct a [[Position1D]] from a type `S` for which there exist a
   * [[coordinate.Coordinateable]].
   *
   * @param s The coordinate value from which to create a [[Position1D]].
   *
   * @see [[coordinate.Coordinateable]]
   */
  def apply[S: Coordinateable](s: S): Position1D = {
    Position1D(implicitly[Coordinateable[S]].convert(s))
  }
}

/**
 * Position for 2 dimensional data.
 *
 * @param first  [[coordinate.Coordinate]] for the first dimension.
 * @param second [[coordinate.Coordinate]] for the second dimension.
 */
case class Position2D(first: Coordinate, second: Coordinate) extends Position
  with ModifyablePosition with ReduceablePosition with ExpandablePosition {
  type L = Position1D
  type S = Position2D
  type M = Position3D

  type O = Map[Position1D, Content]
  type A = Map[Position1D, Content]
  def toOverMapValue(r: L, c: Content): O = Map(r -> c)
  def toAlongMapValue(r: Position1D, c: Content): A = Map(r -> c)
  def combineOverMapValues(x: Option[O], y: O): O = combineMapValues(x, y)
  def combineAlongMapValues(x: Option[A], y: A): A = combineMapValues(x, y)
  private def combineMapValues(l: Option[Map[Position1D, Content]],
    r: Map[Position1D, Content]): Map[Position1D, Content] = {
    l match {
      case Some(x) => x ++ r
      case None => r
    }
  }

  val coordinates = List(first, second)

  protected def less(cl: List[Coordinate]): L = Position1D(cl(0))
  protected def same(cl: List[Coordinate]): S = Position2D(cl(0), cl(1))
  protected def more(cl: List[Coordinate]): M = Position3D(cl(0), cl(1), cl(2))
}

/** Companion object to [[Position2D]]. */
object Position2D {
  /**
   * Construct a [[Position2D]] from types `S` and `T` for which there exist
   * [[coordinate.Coordinateable]]s.
   *
   * @param s The first coordinate value from which to create a [[Position2D]].
   * @param t The second coordinate value from which to create a [[Position2D]].
   *
   * @see [[coordinate.Coordinateable]]
   */
  def apply[S: Coordinateable, T: Coordinateable](s: S, t: T): Position2D = {
    Position2D(implicitly[Coordinateable[S]].convert(s),
      implicitly[Coordinateable[T]].convert(t))
  }
}

/**
 * Position for 3 dimensional data.
 *
 * @param first  [[coordinate.Coordinate]] for the first dimension.
 * @param second [[coordinate.Coordinate]] for the second dimension.
 * @param third  [[coordinate.Coordinate]] for the third dimension.
 */
case class Position3D(first: Coordinate, second: Coordinate,
  third: Coordinate) extends Position with ModifyablePosition
  with ReduceablePosition with ExpandablePosition {
  type L = Position2D
  type S = Position3D
  type M = Position4D

  type O = Map[Position2D, Content]
  type A = Map[Position1D, Content]
  def toOverMapValue(r: L, c: Content): O = Map(r -> c)
  def toAlongMapValue(r: Position1D, c: Content): A = Map(r -> c)
  def combineOverMapValues(x: Option[O], y: O): O = combineMapValues(x, y)
  def combineAlongMapValues(x: Option[A], y: A): A = combineMapValues(x, y)
  private def combineMapValues[P <: Position](l: Option[Map[P, Content]],
    r: Map[P, Content]): Map[P, Content] = {
    l match {
      case Some(x) => x ++ r
      case None => r
    }
  }

  val coordinates = List(first, second, third)

  protected def less(cl: List[Coordinate]): L = Position2D(cl(0), cl(1))
  protected def same(cl: List[Coordinate]): S = Position3D(cl(0), cl(1), cl(2))
  protected def more(cl: List[Coordinate]): M = {
    Position4D(cl(0), cl(1), cl(2), cl(3))
  }
}

/** Companion object to [[Position3D]]. */
object Position3D {
  /**
   * Construct a [[Position3D]] from types `S`, `T` and `U` for which there
   * exist [[coordinate.Coordinateable]]s.
   *
   * @param s The first coordinate value from which to create a [[Position3D]].
   * @param t The second coordinate value from which to create a [[Position3D]].
   * @param u The third coordinate value from which to create a [[Position3D]].
   *
   * @see [[coordinate.Coordinateable]]
   */
  def apply[S: Coordinateable, T: Coordinateable, U: Coordinateable](s: S,
    t: T, u: U): Position3D = {
    Position3D(implicitly[Coordinateable[S]].convert(s),
      implicitly[Coordinateable[T]].convert(t),
      implicitly[Coordinateable[U]].convert(u))
  }
}

/**
 * Position for 4 dimensional data.
 *
 * @param first  [[coordinate.Coordinate]] for the first dimension.
 * @param second [[coordinate.Coordinate]] for the second dimension.
 * @param third  [[coordinate.Coordinate]] for the third dimension.
 * @param fourth [[coordinate.Coordinate]] for the fourth dimension.
 */
case class Position4D(first: Coordinate, second: Coordinate, third: Coordinate,
  fourth: Coordinate) extends Position with ModifyablePosition
  with ReduceablePosition with ExpandablePosition {
  type L = Position3D
  type S = Position4D
  type M = Position5D

  type O = Map[Position3D, Content]
  type A = Map[Position1D, Content]
  def toOverMapValue(r: L, c: Content): O = Map(r -> c)
  def toAlongMapValue(r: Position1D, c: Content): A = Map(r -> c)
  def combineOverMapValues(x: Option[O], y: O): O = combineMapValues(x, y)
  def combineAlongMapValues(x: Option[A], y: A): A = combineMapValues(x, y)
  private def combineMapValues[P <: Position](l: Option[Map[P, Content]],
    r: Map[P, Content]): Map[P, Content] = {
    l match {
      case Some(x) => x ++ r
      case None => r
    }
  }

  val coordinates = List(first, second, third, fourth)

  protected def less(cl: List[Coordinate]): L = Position3D(cl(0), cl(1), cl(2))
  protected def same(cl: List[Coordinate]): S = {
    Position4D(cl(0), cl(1), cl(2), cl(3))
  }
  protected def more(cl: List[Coordinate]): M = {
    Position5D(cl(0), cl(1), cl(2), cl(3), cl(4))
  }
}

/** Companion object to [[Position4D]]. */
object Position4D {
  /**
   * Construct a [[Position4D]] from types `S`, `T`, `U` and `V` for which
   * there exist [[coordinate.Coordinateable]]s.
   *
   * @param s The first coordinate value from which to create a [[Position4D]].
   * @param t The second coordinate value from which to create a [[Position4D]].
   * @param u The third coordinate value from which to create a [[Position4D]].
   * @param v The fourth coordinate value from which to create a [[Position4D]].
   *
   * @see [[coordinate.Coordinateable]]
   */
  def apply[S: Coordinateable, T: Coordinateable, U: Coordinateable, V: Coordinateable](s: S, t: T, u: U, v: V): Position4D = {
    Position4D(implicitly[Coordinateable[S]].convert(s),
      implicitly[Coordinateable[T]].convert(t),
      implicitly[Coordinateable[U]].convert(u),
      implicitly[Coordinateable[V]].convert(v))
  }
}

/**
 * Position for 5 dimensional data.
 *
 * @param first  [[coordinate.Coordinate]] for the first dimension.
 * @param second [[coordinate.Coordinate]] for the second dimension.
 * @param third  [[coordinate.Coordinate]] for the third dimension.
 * @param fourth [[coordinate.Coordinate]] for the fourth dimension.
 * @param fifth  [[coordinate.Coordinate]] for the fifth dimension.
 */
case class Position5D(first: Coordinate, second: Coordinate, third: Coordinate,
  fourth: Coordinate, fifth: Coordinate) extends Position
  with ModifyablePosition with ReduceablePosition {
  type L = Position4D
  type S = Position5D

  type O = Map[Position4D, Content]
  type A = Map[Position1D, Content]
  def toOverMapValue(r: L, c: Content): O = Map(r -> c)
  def toAlongMapValue(r: Position1D, c: Content): A = Map(r -> c)
  def combineOverMapValues(x: Option[O], y: O): O = combineMapValues(x, y)
  def combineAlongMapValues(x: Option[A], y: A): A = combineMapValues(x, y)
  private def combineMapValues[P <: Position](l: Option[Map[P, Content]],
    r: Map[P, Content]): Map[P, Content] = {
    l match {
      case Some(x) => x ++ r
      case None => r
    }
  }

  val coordinates = List(first, second, third, fourth, fifth)

  protected def less(cl: List[Coordinate]): L = {
    Position4D(cl(0), cl(1), cl(2), cl(3))
  }
  protected def same(cl: List[Coordinate]): S = {
    Position5D(cl(0), cl(1), cl(2), cl(3), cl(4))
  }
}

/** Companion object to [[Position5D]]. */
object Position5D {
  /**
   * Construct a [[Position5D]] from types `S`, `T`, `U`, `V` and `W` for
   * which there exist [[coordinate.Coordinateable]]s.
   *
   * @param s The first coordinate value from which to create a [[Position5D]].
   * @param t The second coordinate value from which to create a [[Position5D]].
   * @param u The third coordinate value from which to create a [[Position5D]].
   * @param v The fourth coordinate value from which to create a [[Position5D]].
   * @param w The fifth coordinate value from which to create a [[Position5D]].
   *
   * @see [[coordinate.Coordinateable]]
   */
  def apply[S: Coordinateable, T: Coordinateable, U: Coordinateable, V: Coordinateable, W: Coordinateable](s: S, t: T, u: U, v: V, w: W): Position5D = {
    Position5D(implicitly[Coordinateable[S]].convert(s),
      implicitly[Coordinateable[T]].convert(t),
      implicitly[Coordinateable[U]].convert(u),
      implicitly[Coordinateable[V]].convert(v),
      implicitly[Coordinateable[W]].convert(w))
  }
}

/**
 * Rich wrapper around a `TypedPipe[`[[Position]]`]`.
 *
 * @param data The `TypedPipe[`[[Position]]`]`.
 */
class PositionPipe[P <: Position](data: TypedPipe[P]) {
  /**
   * Returns the distinct [[position.Position]](s) (or names) for a given
   * `slice`.
   *
   * @param slice Encapsulates the dimension(s) for which the names are to
   *              be returned.
   *
   * @return A Scalding `TypedPipe[(`[[Slice.S]]`, Long)]` of the distinct
   *         [[Position]](s) together with a unique index.
   *
   * @note The [[Position]](s) are returned with an index so the return value
   *       can be used in various `write` methods. The index itself is unique
   *       for each [[Position]] but no ordering is defined.
   *
   * @see [[Names]]
   */
  def names[D <: Dimension](slice: Slice[P, D])(
    implicit ev: PosDimDep[P, D]): TypedPipe[(slice.S, Long)] = {
    implicit def PositionOrdering[T <: Position] = {
      new Ordering[T] { def compare(l: T, r: T) = l.compare(r) }
    }

    Names.number(data.map { case p => slice.selected(p) }.distinct)
  }

  /**
   * Persist a `TypedPipe[`[[Position]]`]` to disk.
   *
   * @param file        Name of the output file.
   * @param separator   Separator to use between [[position.Position]] and
   *                    [[contents.Content]].
   * @param descriptive Indicates if the output should be descriptive.
   *
   * @return A Scalding `TypedPipe[`[[Position]]`]` which is this
   *         [[PositionPipe]].
   */
  def persist(file: String, separator: String = "|",
    descriptive: Boolean = false)(implicit flow: FlowDef,
      mode: Mode): TypedPipe[P] = {
    data
      .map {
        case p => descriptive match {
          case true => p.toString
          case false => p.toShortString(separator)
        }
      }
      .toPipe('line)
      .write(TextLine(file))

    data
  }
}

/** Companion object for the [[PositionPipe]] class. */
object PositionPipe {
  /** Converts a `TypedPipe[`[[Position]]`]` to a [[PositionPipe]]. */
  implicit def typedPipePosition[P <: Position](
    data: TypedPipe[P]): PositionPipe[P] = {
    new PositionPipe(data)
  }
}

/** Type class for transforming a type `T` into a [[Position]]. */
trait Positionable[T, P <: Position] {
  /**
   * Returns a [[Position]] for type `T`.
   *
   * @param t Object that can be converted to a [[Position]].
   */
  def convert(t: T): P
}

/** Companion object for the [[Positionable]] type class. */
object Positionable {
  /**
   * Converts a [[Position]] to a [[Position]]; that is, it's a pass through.
   */
  implicit def PositionPositionable[T <: Position]: Positionable[T, T] = {
    new Positionable[T, T] {
      def convert(t: T): T = t
    }
  }
  /** Converts a [[coordinate.Coordinateable]] to a [[Position]]. */
  implicit def CoordinateablePositionable[T: Coordinateable]: Positionable[T, Position1D] = {
    new Positionable[T, Position1D] {
      def convert(t: T): Position1D = {
        Position1D(implicitly[Coordinateable[T]].convert(t))
      }
    }
  }
}

/** Type class for transforming a type `T` into a `List[`[[Position]]`]`. */
trait PositionListable[T, P <: Position] {
  /**
   * Returns a `List[`[[Position]]`]` for type `T`.
   *
   * @param t Object that can be converted to a `List[`[[Position]]`]`.
   */
  def convert(t: T): List[P]
}

/** Companion object for the [[PositionListable]] type class. */
object PositionListable {
  /** Converts a `List[`[[Positionable]]`]` to a `List[`[[Position]]`]`. */
  implicit def ListPositionablePositionListable[T, P <: Position](
    implicit ev: Positionable[T, P]): PositionListable[List[T], P] = {
    new PositionListable[List[T], P] {
      def convert(t: List[T]): List[P] = t.map(ev.convert(_))
    }
  }
  /** Converts a [[Positionable]] to a `List[`[[Position]]`]`. */
  implicit def PositionablePositionListable[T, P <: Position](
    implicit ev: Positionable[T, P]): PositionListable[T, P] = {
    new PositionListable[T, P] {
      def convert(t: T): List[P] = List(ev.convert(t))
    }
  }
}

/**
 * Type class for transforming a type `T` into a `TypedPipe[`[[Position]]`]`.
 */
trait PositionPipeable[T, P <: Position] {
  /**
   * Returns a `TypedPipe[`[[Position]]`]` for type `T`.
   *
   * @param t Object that can be converted to a `TypedPipe[`[[Position]]`]`.
   */
  def convert(t: T): TypedPipe[P]
}

object PositionPipeable {
  /**
   * Converts a `TypedPipe[`[[Position]]`]` to a `TypedPipe[`[[Position]]`]`;
   * that is, it's a pass through.
   */
  implicit def PipePositionPositionPipeable[P <: Position]: PositionPipeable[TypedPipe[P], P] = {
    new PositionPipeable[TypedPipe[P], P] {
      def convert(t: TypedPipe[P]): TypedPipe[P] = t
    }
  }
  /** Converts a `List[`[[Positionable]]`]` to a `TypedPipe[`[[Position]]`]`. */
  implicit def ListPositionablePositionPipeable[T, P <: Position](
    implicit ev: Positionable[T, P], flow: FlowDef,
    mode: Mode): PositionPipeable[List[T], P] = {
    new PositionPipeable[List[T], P] {
      def convert(t: List[T]): TypedPipe[P] = {
        new IterablePipe(t.map(ev.convert(_)), flow, mode)
      }
    }
  }
  /** Converts a [[Positionable]] to a `TypedPipe[`[[Position]]`]`. */
  implicit def PositionablePositionPipeable[T, P <: Position](
    implicit ev: Positionable[T, P], flow: FlowDef,
    mode: Mode): PositionPipeable[T, P] = {
    new PositionPipeable[T, P] {
      def convert(t: T): TypedPipe[P] = {
        new IterablePipe(List(ev.convert(t)), flow, mode)
      }
    }
  }
}

