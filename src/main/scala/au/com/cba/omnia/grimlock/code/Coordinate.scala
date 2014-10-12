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

package au.com.cba.omnia.grimlock.position.coordinate

import au.com.cba.omnia.grimlock.contents.encoding._
import au.com.cba.omnia.grimlock.utilities._

import java.util.Date

import scala.util.matching.Regex

// TODO: Can Coordinate be replaced by Value?

/** Base trait for coordinates in a [[Position]]. */
trait Coordinate {
  /**
   * The [[contents.encoding.Codex]] used to encode/decode this
   * [[Coordinate]].
   */
  val codex: Codex with CoordinateCodex

  /**
   * Check for equality with `that`.
   *
   * @param that Coordinate to compare against.
   *
   * @see [[Coordinateable]]
   */
  def equ[T: Coordinateable](that: T): Boolean = eval(that, Equal)

  /**
   * Check for in-equality with `that`.
   *
   * @param that Coordinate to compare against.
   *
   * @see [[Coordinateable]]
   */
  def neq[T: Coordinateable](that: T): Boolean = !(this equ that)

  /**
   * Check for for match with `that` regular expression.
   *
   * @param that Regular expression to match against.
   *
   * @note This always applies [[Coordinate.toShortString]] before matching.
   */
  def like(that: Regex): Boolean = {
    that.pattern.matcher(this.toShortString).matches
  }

  // Note: These next 4 methods implement comparison in a non-standard
  //       way when comparing two objects that can't be compared. In
  //       such cases the result is always false. This is the desired
  //       behaviour for the 'which' method (i.e. a filter).

  /**
   * Check if `this` is less than `that`.
   *
   * @param that Coordinate to compare against.
   *
   * @note If `that` is of a different type than `this`, then the result is
   *       always `false`.
   *
   * @see [[Coordinateable]]
   */
  def lss[T: Coordinateable](that: T): Boolean = eval(that, Less)

  /**
   * Check if `this` is less or equal to `that`.
   *
   * @param that Coordinate to compare against.
   *
   * @note If `that` is of a different type than `this`, then the result is
   *       always `false`.
   *
   * @see [[Coordinateable]]
   */
  def leq[T: Coordinateable](that: T): Boolean = eval(that, LessEqual)

  /**
   * Check if `this` is greater than `that`.
   *
   * @param that Coordinate to compare against.
   *
   * @note If `that` is of a different type than `this`, then the result is
   *       always `false`.
   *
   * @see [[Coordinateable]]
   */
  def gtr[T: Coordinateable](that: T): Boolean = eval(that, Greater)

  /**
   * Check if `this` is greater or equal to `that`.
   *
   * @param that Coordinate to compare against.
   *
   * @note If `that` is of a different type than `this`, then the result is
   *       always `false`.
   *
   * @see [[Coordinateable]]
   */
  def geq[T: Coordinateable](that: T): Boolean = eval(that, GreaterEqual)

  /**
   * Compare this object with another [[Coordinate]].
   *
   * @param that [[Coordinate]] to compare against.
   *
   * @return x < 0 iff this < that,
   *         x = 0 iff this = that,
   *         x > 0 iff this > that.
   *
   * @note If the comparison is between two [[Coordinate]]s with different
   *       underlying types, then a string comparison is performed.
   */
  def compare(that: Coordinate): Int = {
    codex.compare(this, that) match {
      case Some(0) => 0
      case Some(x) if (x > 0) => 1
      case Some(x) if (x < 0) => -1
      case _ =>
        throw new Exception("unable to compare different coordinate types.")
    }
  }

  /** Return value as `java.util.Date`. */
  def asDate: Option[Date] = None
  /** Return value as `String`. */
  def asString: Option[String] = None
  /** Return value as `Long`. */
  def asLong: Option[Long] = None

  /** Return a consise (terse) string representation of a [[Coordinate]]. */
  def toShortString: String = codex.toShortString(this)

  private def eval[T: Coordinateable](that: T, op: CompareResult): Boolean = {
    CompareResult.evaluate(codex.compare(this,
      implicitly[Coordinateable[T]].convert(that)), op)
  }
}

object Coordinate {
  /** Define an ordering between 2 [[Coordinate]]s. */
  implicit val Ordering: Ordering[Coordinate] = new Ordering[Coordinate] {
    def compare(x: Coordinate, y: Coordinate): Int = x.compare(y)
  }
}

/**
 * [[Coordinate]] for `java.util.Date` values.
 *
 * @param value `java.util.Date` value of the [[Coordinate]].
 * @param codex The [[contents.encoding.Codex]] used for encoding/decoding
 *              `value`.
 */
case class DateCoordinate(value: Date, codex: Codex with CoordinateCodex)
  extends Coordinate {
  override def asDate = Some(value)
}

/**
 * [[Coordinate]] for `String` values.
 *
 * @param value `String` value of the [[Coordinate]].
 * @param codex The [[contents.encoding.Codex]] used for encoding/decoding
 *        `value`.
 */
case class StringCoordinate(value: String, codex: Codex with CoordinateCodex)
  extends Coordinate {
  override def asString = Some(value)
}

/**
 * [[Coordinate]] for `Long` values.
 *
 * @param value `Long` value of the [[Coordinate]].
 * @param codex The [[contents.encoding.Codex]] used for encoding/decoding
 *              `value`.
 */
case class LongCoordinate(value: Long, codex: Codex with CoordinateCodex)
  extends Coordinate {
  override def asLong = Some(value)
}

/** Type class for transforming a type `T` into a [[Coordinate]]. */
trait Coordinateable[T] {
  /**
   * Returns a [[Coordinate]] for type `T`.
   *
   * @param t Object that can be converted to a [[Coordinate]].
   */
  def convert(t: T): Coordinate
}

/** Companion object for the [[Coordinateable]] type class. */
object Coordinateable {
  /**
   * Converts a [[Coordinate]] to a [[Coordinate]]; that is, it's a pass
   * through.
   */
  implicit def CoordinateCoordinateable[T <: Coordinate]: Coordinateable[T] = {
    new Coordinateable[T] {
      def convert(t: T): Coordinate = t
    }
  }
  /** Converts a `String` to a [[Coordinate]]. */
  implicit def StringCoordinateable: Coordinateable[String] = {
    new Coordinateable[String] {
      def convert(t: String): Coordinate = StringCoordinate(t, StringCodex)
    }
  }
  /** Converts a `Long` to a [[Coordinate]]. */
  implicit def LongCoordinateable: Coordinateable[Long] = {
    new Coordinateable[Long] {
      def convert(t: Long): Coordinate = LongCoordinate(t, LongCodex)
    }
  }
}

