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

package grimlock.contents.encoding

import grimlock.contents.events._
import grimlock.contents.variable._
import grimlock.position.coordinate._

import java.text.SimpleDateFormat

/** Base trait for encoding/decoding basic data types. */
trait Codex {
  /** Name of the codex. */
  val name: String

  /** The data type. */
  type T

  /**
   * Parse string into the data type.
   *
   * @param t The string to parse.
   *
   * @return `Some(T)` if the string was successfull parse, `None` otherwise.
   */
  def parse(t: String): Option[T]

  /**
   * Converts the type to a consise (terse) string.
   *
   * @param t The data type to convert to string.
   *
   * @return Short string representation of `t`.
   */
  def toShortString(t: T): String

  /**
   * Compare two optional data types.
   *
   * @param x The first `Option` of `T` to compare.
   * @param y The second `Option` of `T` to compare.
   *
   * @return `Some[Int]` if `x` and `y` could be compare, `None` otherwise. If
   *         successful, then the returned value is < 0 iff x < y, 0 iff x = y,
   *         > 0 iff x > y.
   */
  def compare(x: Option[T], y: Option[T]): Option[Int]

  protected def parse(t: String, conv: (String) => T): Option[T] = {
    scala.util.Try(conv(t)).toOption
  }
}

/**
 * Base trait for encoding/decoding basic data types as
 * [[contents.variable.Value]].
 */
trait ValueCodex { self: Codex =>
  /** The specific type of [[contents.variable.Value]]. */
  type V <: Value

  /**
   * Convert a basic data type to a [[contents.variable.Value]].
   *
   * @param value Data type to wrap in a [[contents.variable.Value]].
   */
  def toValue(value: T): V

  /**
   * Extract a basic data type from a [[contents.variable.Value]].
   *
   * @param value [[contents.variable.Value]] from which to extract the data.
   */
  def fromValue(value: Value): T

  /**
   * Decode a basic data type into a [[contents.variable.Value]].
   *
   * @param value String to decode into a [[contents.variable.Value]].
   *
   * @return `Some(`[[contents.variable.Value]]`)` if the decode was
   *         successful, `None` otherwise.
   */
  def decode(value: String): Option[V] = {
    parse(value).map { case t => toValue(t) }
  }

  /**
   * Converts a [[contents.variable.Value]] to a consise (terse) string.
   *
   * @param t The [[contents.variable.Value]] to convert to string.
   *
   * @return Short string representation of the [[contents.variable.Value]].
   */
  def toShortString(value: Value): String = toShortString(fromValue(value))

  /**
   * Compare two [[contents.variable.Value]]s.
   *
   * @param x The first [[contents.variable.Value]] compare.
   * @param y The second [[contents.variable.Value]] compare.
   *
   * @return `Some[Int]` if `x` and `y` could be compare, `None` otherwise. If
   *         successful, then the returned value is < 0 iff x < y, 0 iff x = y,
   *         > 0 iff x > y.
   */
  def compare(x: Value, y: Value): Option[Int]
}

/**
 * Base trait for encoding/decoding basic data types as
 * [[position.coordinate.Coordinate]].
 */
trait CoordinateCodex { self: Codex =>
  /** The specific type of [[position.coordinate.Coordinate]]. */
  type C <: Coordinate

  /**
   * Convert a basic data type to a [[position.coordinate.Coordinate]].
   *
   * @param value Data type to wrap in a [[position.coordinate.Coordinate]].
   */
  def toCoordinate(value: T): C

  /**
   * Extract a basic data type from a [[position.coordinate.Coordinate]].
   *
   * @param value [[position.coordinate.Coordinate]] from which to extract
   *        the data.
   */
  def fromCoordinate(value: Coordinate): T

  /**
   * Read a basic data type into a [[position.coordinate.Coordinate]].
   *
   * @param value String to read into a [[position.coordinate.Coordinate]].
   *
   * @return `Some(`[[position.coordinate.Coordinate]]`)` if the decode was
   *         successful, `None` otherwise.
   */
  def read(value: String): Option[C] = {
    parse(value).map { case t => toCoordinate(t) }
  }

  /**
   * Converts a [[position.coordinate.Coordinate]] to a consise (terse) string.
   *
   * @param t The [[position.coordinate.Coordinate]] to convert to string.
   *
   * @return Short string representation of the
   *         [[position.coordinate.Coordinate]].
   */
  def toShortString(value: Coordinate): String = {
    toShortString(fromCoordinate(value))
  }

  /**
   * Compare two [[position.coordinate.Coordinate]]s.
   *
   * @param x The first [[position.coordinate.Coordinate]] compare.
   * @param y The second [[position.coordinate.Coordinate]] compare.
   *
   * @return `Some[Int]` if `x` and `y` could be compare, `None` otherwise. If
   *         successful, then the returned value is < 0 iff x < y, 0 iff x = y,
   *         > 0 iff x > y.
   */
  def compare(x: Coordinate, y: Coordinate): Option[Int]
}

object Codex {
  /**
   * Return a [[Codex]] from it's name.
   *
   * @param str The name of the [[Codex]].
   *
   * @return `Some(`[[Codex]]`)` if the name maps to a know [[Codex]],
   *         `None` otherwise.
   */
  def fromString[T](str: String): Option[Codex] = {
    str match {
      case DateCodex.name => Some(DateCodex)
      case DateTimeCodex.name => Some(DateTimeCodex)
      case StringCodex.name => Some(StringCodex)
      case DoubleCodex.name => Some(DoubleCodex)
      case LongCodex.name => Some(LongCodex)
      case BooleanCodex.name => Some(BooleanCodex)
      case _ => None
    }
  }

  /**
   * Shorthand for [[DateCodex]] type (for use with
   * [[contents.metadata.Schema]]).
   */
  type DateCodex = DateCodex.type
  /**
   * Shorthand for [[DateTimeCodex]] type (for use with
   * [[contents.metadata.Schema]]).
   */
  type DateTimeCodex = DateTimeCodex.type
  /**
   * Shorthand for [[StringCodex]] type (for use with
   * [[contents.metadata.Schema]]).
   */
  type StringCodex = StringCodex.type
  /**
   * Shorthand for [[DoubleCodex]] type (for use with
   * [[contents.metadata.Schema]]).
   */
  type DoubleCodex = DoubleCodex.type
  /**
   * Shorthand for [[LongCodex]] type (for use with
   * [[contents.metadata.Schema]]).
   */
  type LongCodex = LongCodex.type
  /**
   * Shorthand for [[BooleanCodex]] type (for use with
   * [[contents.metadata.Schema]]).
   */
  type BooleanCodex = BooleanCodex.type

  /**
   * Implicit value for [[DateCodex]] (for use with
   * [[contents.metadata.Schema]]).
   */
  implicit val tc: DateCodex = DateCodex
  /**
   * Implicit value for [[DateTimeCodex]] (for use with
   * [[contents.metadata.Schema]]).
   */
  implicit val dtc: DateTimeCodex = DateTimeCodex
  /**
   * Implicit value for [[StringCodex]] (for use with
   * [[contents.metadata.Schema]]).
   */
  implicit val sc: StringCodex = StringCodex
  /**
   * Implicit value for [[DoubleCodex]] (for use with
   * [[contents.metadata.Schema]]).
   */
  implicit val dc: DoubleCodex = DoubleCodex
  /**
   * Implicit value for [[LongCodex]] (for use with
   * [[contents.metadata.Schema]]).
   */
  implicit val lc: LongCodex = LongCodex
  /**
   * Implicit value for [[BooleanCodex]] (for use with
   * [[contents.metadata.Schema]]).
   */
  implicit val bc: BooleanCodex = BooleanCodex
}

/** Base trait for dealing with `java.util.Date`. */
trait DateAndTimeCodex extends Codex with ValueCodex with CoordinateCodex {
  /** The format string of the date. */
  val format: String

  type T = java.util.Date
  type V = DateValue
  type C = DateCoordinate

  def toValue(value: T): V = DateValue(value, this)
  def fromValue(value: Value): T = value.asInstanceOf[V].value
  def compare(x: Value, y: Value): Option[Int] = compare(x.asDate, y.asDate)

  def toCoordinate(value: T): C = DateCoordinate(value, this)
  def fromCoordinate(value: Coordinate): T = value.asInstanceOf[C].value
  def compare(x: Coordinate, y: Coordinate): Option[Int] = {
    compare(x.asDate, y.asDate)
  }

  def parse(value: String): Option[T] = {
    parse(value, (str: String) => (new SimpleDateFormat(format)).parse(str))
  }
  def toShortString(value: T): String = {
    (new SimpleDateFormat(format)).format(value)
  }
  def compare(x: Option[T], y: Option[T]): Option[Int] = {
    (x, y) match {
      case (Some(l), Some(r)) => Some(l.getTime().compare(r.getTime()))
      case _ => None
    }
  }
}

/** [[Codex]] for dealing with `java.util.Date` formatted as `yyyy-MM-dd`. */
case object DateCodex extends DateAndTimeCodex {
  val name = "date"
  val format = "yyyy-MM-dd"
}

/**
 * [[Codex]] for dealing with `java.util.Date` formatted as
 * `yyyy-MM-dd hh:mm:ss`.
 */
case object DateTimeCodex extends DateAndTimeCodex {
  val name = "date.time"
  val format = "yyyy-MM-dd hh:mm:ss"
}

/** [[Codex]] for dealing with `String`. */
case object StringCodex extends Codex with ValueCodex with CoordinateCodex {
  val name = "string"

  type T = String
  type V = StringValue
  type C = StringCoordinate

  def toValue(value: T): V = StringValue(value, this)
  def fromValue(value: Value): T = value.asInstanceOf[V].value
  def compare(x: Value, y: Value): Option[Int] = {
    compare(x.asString, y.asString)
  }

  def toCoordinate(value: T): C = StringCoordinate(value, this)
  def fromCoordinate(value: Coordinate): T = value.asInstanceOf[C].value
  def compare(x: Coordinate, y: Coordinate): Option[Int] = {
    compare(x.asString, y.asString)
  }

  def parse(value: String): Option[T] = Some(value)
  def toShortString(value: T): String = value
  def compare(x: Option[T], y: Option[T]): Option[Int] = {
    (x, y) match {
      case (Some(l), Some(r)) => Some(l.compare(r))
      case _ => None
    }
  }
}

/** [[Codex]] for dealing with `Double`. */
case object DoubleCodex extends Codex with ValueCodex {
  val name = "double"

  type T = Double
  type V = DoubleValue

  def toValue(value: T): V = DoubleValue(value, this)
  def fromValue(value: Value): T = value.asInstanceOf[V].value
  def compare(x: Value, y: Value): Option[Int] = {
    compare(x.asDouble, y.asDouble)
  }

  def parse(value: String): Option[T] = parse(value, _.toDouble)
  def toShortString(value: T): String = value.toString
  def compare(x: Option[T], y: Option[T]): Option[Int] = {
    (x, y) match {
      case (Some(l), Some(r)) => Some(l.compare(r))
      case _ => None
    }
  }
}

/** [[Codex]] for dealing with `Long`. */
case object LongCodex extends Codex with ValueCodex with CoordinateCodex {
  val name = "long"

  type T = Long
  type V = LongValue
  type C = LongCoordinate

  def toValue(value: T): V = LongValue(value, this)
  def fromValue(value: Value): T = value.asInstanceOf[V].value
  def compare(x: Value, y: Value): Option[Int] = {
    compare(x.asLong, y.asLong) match {
      case None => DoubleCodex.compare(x.asDouble, y.asDouble)
      case cmp => cmp
    }
  }

  def toCoordinate(value: T): C = LongCoordinate(value, this)
  def fromCoordinate(value: Coordinate): T = value.asInstanceOf[C].value
  def compare(x: Coordinate, y: Coordinate): Option[Int] = {
    compare(x.asLong, y.asLong)
  }

  def parse(value: String): Option[T] = parse(value, _.toLong)
  def toShortString(value: T): String = value.toString
  def compare(x: Option[T], y: Option[T]): Option[Int] = {
    (x, y) match {
      case (Some(l), Some(r)) => Some(l.compare(r))
      case _ => None
    }
  }
}

/** [[Codex]] for dealing with `Boolean`. */
case object BooleanCodex extends Codex with ValueCodex {
  val name = "boolean"

  type T = Boolean
  type V = BooleanValue

  def toValue(value: T): V = BooleanValue(value, this)
  def fromValue(value: Value): T = value.asInstanceOf[V].value
  def compare(x: Value, y: Value): Option[Int] = {
    compare(x.asBoolean, y.asBoolean)
  }

  def parse(value: String): Option[T] = parse(value, _.toBoolean)
  def toShortString(value: T): String = value.toString
  def compare(x: Option[T], y: Option[T]): Option[Int] = {
    (x, y) match {
      case (Some(l), Some(r)) => Some(l.compare(r))
      case _ => None
    }
  }
}

/** Base trait for dealing with [[contents.events.Event]]. */
trait EventCodex extends Codex with ValueCodex {
  type T <: Event
}

