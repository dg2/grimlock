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

package grimlock.contents.variable

import grimlock.contents.encoding._
import grimlock.contents.encoding.Codex._
import grimlock.contents.events._
import grimlock.utilities._

import java.util.Date

import scala.util.matching.Regex

/**
 * Base class for variable types.
 *
 * @param base Optional parent variable type.
 * @param name Name of the variable type.
 */
case class Type(base: Option[Type], name: String) {
  /**
   * Check if `this` is a specialisation of `that`.
   *
   * @param that Variable type to check against.
   */
  def isSpecialisationOf(that: Type): Boolean = {
    base match {
      case Some(vt) => (this == that) || vt.isSpecialisationOf(that)
      case None => (this == that)
    }
  }

  /** Returns the most general super type of `this`. */
  def getGeneralisation(): Type = {
    base match {
      case Some(vt) => vt.getGeneralisation()
      case None => this
    }
  }

  /** Returns the name of `this` type. */
  override def toString = name.capitalize

  /** Converts the type to a consise (terse) string. */
  def toShortString = name
}

/** Compantion object to [[Type]] class. */
object Type {
  /** Type for when the type is mixed. */
  val Mixed = Type(None, "mixed")
  /** Type for numeric types. */
  val Numerical = Type(None, "numerical")
  /** Type for continuous types. */
  val Continuous = Type(Some(Numerical), "continuous")
  /** Type for discrete types. */
  val Discrete = Type(Some(Numerical), "discrete")
  /** Type for categorical types. */
  val Categorical = Type(None, "categorical")
  /** Type for nominal types. */
  val Nominal = Type(Some(Categorical), "nominal")
  /** Type for ordinal types. */
  val Ordinal = Type(Some(Categorical), "ordinal")
  /** Type for date types. */
  val Date = Type(None, "date")
  /** Type for event types. */
  val Event = Type(None, "event")
}

/** Base trait for variable values. */
trait Value {
  /** The [[contents.encoding.Codex]] used to encode/decode this [[Value]]. */
  val codex: Codex with ValueCodex

  /**
   * Check for equality with `that`.
   *
   * @param that Value to compare against.
   *
   * @see [[Valueable]]
   */
  def equ[T: Valueable](that: T): Boolean = eval(that, Equal)

  /**
   * Check for in-equality with `that`.
   *
   * @param that Value to compare against.
   *
   * @see [[Valueable]]
   */
  def neq[T: Valueable](that: T): Boolean = !(this equ that)

  /**
   * Check for for match with `that` regular expression.
   *
   * @param that Regular expression to match against.
   *
   * @note This always applies [[Value.toShortString]] before matching.
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
   * @param that Value to compare against.
   *
   * @note If `that` is of a different type than `this`, then the result
   *       is always `false`.
   *
   * @see [[Valueable]]
   */
  def lss[T: Valueable](that: T): Boolean = eval(that, Less)

  /**
   * Check if `this` is less or equal to `that`.
   *
   * @param that Value to compare against.
   *
   * @note If `that` is of a different type than `this`, then the result
   *       is always `false`.
   *
   * @see [[Valueable]]
   */
  def leq[T: Valueable](that: T): Boolean = eval(that, LessEqual)

  /**
   * Check if `this` is greater than `that`.
   *
   * @param that Value to compare against.
   *
   * @note If `that` is of a different type than `this`, then the result
   *       is always `false`.
   *
   * @see [[Valueable]]
   */
  def gtr[T: Valueable](that: T): Boolean = eval(that, Greater)

  /**
   * Check if `this` is greater or equal to `that`.
   *
   * @param that Value to compare against.
   *
   * @note If `that` is of a different type than `this`, then the result
   *       is always `false`.
   *
   * @see [[Valueable]]
   */
  def geq[T: Valueable](that: T): Boolean = eval(that, GreaterEqual)

  /** Return value as `java.util.Date`. */
  def asDate: Option[Date] = None
  /** Return value as `String`. */
  def asString: Option[String] = None
  /** Return value as `Double`. */
  def asDouble: Option[Double] = None
  /** Return value as `Long`. */
  def asLong: Option[Long] = None
  /** Return value as `Boolean`. */
  def asBoolean: Option[Boolean] = None
  /** Return value as [[contents.events.Event]]. */
  def asEvent: Option[Event] = None

  /** Return a consise (terse) string representation of a [[Value]]. */
  def toShortString: String = codex.toShortString(this)

  private def eval[T: Valueable](that: T, op: CompareResult): Boolean = {
    CompareResult.evaluate(codex.compare(this,
      implicitly[Valueable[T]].convert(that)), op)
  }
}

/**
 * Value for when the data is of type `java.util.Date`
 *
 * @param value A `java.util.Date`.
 * @param codex The [[contents.encoding.Codex]] used for encoding/decoding
 *              `value`.
 */
case class DateValue(value: Date, codex: DateAndTimeCodex) extends Value {
  override def asDate = Some(value)
}

/**
 * Value for when the data is of type `String`.
 *
 * @param value A `String`.
 * @param codex The [[contents.encoding.Codex]] used for encoding/decoding
 *              `value`.
 */
case class StringValue(value: String, codex: StringCodex) extends Value {
  override def asString = Some(value.toString)
}

/**
 * Value for when the data is of type `Double`.
 *
 * @param value A `Double`.
 * @param codex The [[contents.encoding.Codex]] used for encoding/decoding
 *              `value`.
 */
case class DoubleValue(value: Double, codex: DoubleCodex) extends Value {
  override def asDouble = Some(value)
}

/**
 * Value for when the data is of type `Long`.
 *
 * @param value A `Long`.
 * @param codex The [[contents.encoding.Codex]] used for encoding/decoding
 *              `value`.
 */
case class LongValue(value: Long, codex: LongCodex) extends Value {
  override def asDouble = Some(value)
  override def asLong = Some(value)
}

/**
 * Value for when the data is of type `Boolean`.
 *
 * @param value A `Boolean`.
 * @param codex The [[contents.encoding.Codex]] used for encoding/decoding
 *              `value`.
 */
case class BooleanValue(value: Boolean, codex: BooleanCodex) extends Value {
  override def asBoolean = Some(value)
}

/**
 * Value for when the data is of type [[contents.events.Event]].
 *
 * @param value A [[contents.events.Event]].
 * @param codex The [[contents.encoding.Codex]] used for encoding/decoding
 *              `value`.
 */
case class EventValue[T <: Event](value: T, codex: EventCodex) extends Value {
  override def asEvent = Some(value)
}

/** Type class for transforming a type `T` into a [[Value]]. */
trait Valueable[T] {
  /**
   * Returns a [[Value]] for type `T`.
   *
   * @param t Object that can be converted to a [[Value]].
   */
  def convert(t: T): Value
}

/** Companion object for the [[Valueable]] type class. */
object Valueable {
  /** Converts a [[Value]] to a [[Value]]; that is, it's a pass through. */
  implicit def ValueValueable[T <: Value]: Valueable[T] = new Valueable[T] {
    def convert(t: T): Value = t
  }
  /** Converts a `String` to a [[Value]]. */
  implicit def StringValueable: Valueable[String] = new Valueable[String] {
    def convert(t: String): Value = StringValue(t, StringCodex)
  }
  /** Converts a `Double` to a [[Value]]. */
  implicit def DoubleValueable: Valueable[Double] = new Valueable[Double] {
    def convert(t: Double): Value = DoubleValue(t, DoubleCodex)
  }
  /** Converts a `Long` to a [[Value]]. */
  implicit def LongValueable: Valueable[Long] = new Valueable[Long] {
    def convert(t: Long): Value = LongValue(t, LongCodex)
  }
  /** Converts a `Int` to a [[Value]]. */
  implicit def IntValueable: Valueable[Int] = new Valueable[Int] {
    def convert(t: Int): Value = LongValue(t, LongCodex)
  }
  /** Converts a `Boolean` to a [[Value]]. */
  implicit def BooleanValueable: Valueable[Boolean] = new Valueable[Boolean] {
    def convert(t: Boolean): Value = BooleanValue(t, BooleanCodex)
  }
}

