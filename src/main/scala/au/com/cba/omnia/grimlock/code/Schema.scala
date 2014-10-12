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

package grimlock.contents.metadata

import grimlock.contents._
import grimlock.contents.encoding._
import grimlock.contents.variable._

/** Base trait for variable schemas. */
trait Schema {
  /** The type of variable. */
  val kind: Type
  /** The [[contents.encoding.Codex]] used to encode/decode a variable value. */
  val codex: Codex with ValueCodex

  /**
   * Check if a [[contents.variable.Value]] is valid according to this schema.
   *
   * @param value The [[contents.variable.Value]] to validate.
   */
  def isValid[V <: Value](value: V): Boolean

  /**
   * Decode a string into a [[contents.Content]].
   *
   * @param str The string to decode
   *
   * @return `Some(`[[contents.Content]]`)` if the string can successfull be
   *         decoded and is valid according to this schema, `None` otherwise.
   */
  def decode(str: String): Option[Content] = {
    codex.decode(str) match {
      case Some(v) if isValid(v) =>
        Some(new Content { val schema = Schema.this; val value = v })
      case _ => None
    }
  }

  /**
   * Return a consise (terse) string representation of a [[Schema]].
   *
   * @param separator Separator to use between various fields.
   */
  def toShortString(separator: String): String = {
    kind.name + separator + codex.name
  }
}

object Schema {
  /**
   * Convert a codex and type string to a [[Schema]].
   *
   * @param codex The string name of the [[contents.encoding.Codex]].
   * @param kind  The string name of the [[contents.variable.Type]].
   *
   * @return `Some(`[[Schema]]`)` if an approriate [[Schema]] is
   *         found, `None` otherwise.
   */
  def fromString(codex: String, kind: String): Option[Schema] = {
    (kind, codex) match {
      case (Type.Continuous.name, DoubleCodex.name) =>
        Some(ContinuousSchema[Codex.DoubleCodex]())
      case (Type.Continuous.name, LongCodex.name) =>
        Some(ContinuousSchema[Codex.LongCodex]())
      case (Type.Discrete.name, LongCodex.name) =>
        Some(DiscreteSchema[Codex.LongCodex]())
      case (Type.Nominal.name, StringCodex.name) =>
        Some(NominalSchema[Codex.StringCodex]())
      case (Type.Nominal.name, DoubleCodex.name) =>
        Some(NominalSchema[Codex.DoubleCodex]())
      case (Type.Nominal.name, LongCodex.name) =>
        Some(NominalSchema[Codex.LongCodex]())
      case (Type.Ordinal.name, StringCodex.name) =>
        Some(OrdinalSchema[Codex.StringCodex]())
      case (Type.Ordinal.name, DoubleCodex.name) =>
        Some(OrdinalSchema[Codex.DoubleCodex]())
      case (Type.Ordinal.name, LongCodex.name) =>
        Some(OrdinalSchema[Codex.LongCodex]())
      case (Type.Date.name, DateCodex.name) =>
        Some(DateSchema[Codex.DateCodex]())
      case _ => None
    }
  }
}

/** Base trait for schemas for numerical variables. */
trait NumericalSchema[T] extends Schema {
  /** Optional minimum value of the variable. */
  val minimum: Option[T]
  /** Optional maximum value of the variable. */
  val maximum: Option[T]
}

/**
 * Schema for continuous variables.
 *
 * @param minimum The optional minimum value.
 * @param maximum The optional maximum value.
 *
 * @note The constructor is private to ensure a clean interface as
 *       provided by the `apply` methods of the companion object.
 */
case class ContinuousSchema[C <: Codex with ValueCodex] private (
  minimum: Option[C#T], maximum: Option[C#T])(implicit val codex: C,
    num: Numeric[C#T]) extends NumericalSchema[C#T] {
  val kind = Type.Continuous

  def isValid[V <: Value](value: V): Boolean = {
    import num._

    (codex.fromValue(value), minimum, maximum) match {
      case (v, Some(l), Some(u)) => (v >= l && v <= u)
      case (v, None, None) => true
      case _ => false
    }
  }

  override def toString(): String = {
    val params = (minimum, maximum) match {
      case (Some(l), Some(u)) => l.toString + "," + u.toString
      case _ => ""
    }

    "ContinuousSchema[" + codex.toString + "](" + params + ")"
  }
}

/** Companion object to [[ContinuousSchema]]. */
object ContinuousSchema {
  /** Construct a [[ContinuousSchema]] with unbounded range. */
  def apply[C <: Codex with ValueCodex]()(implicit codex: C,
    num: Numeric[C#T]): ContinuousSchema[C] = {
    ContinuousSchema(None, None)
  }
  /**
   * Construct a [[ContinuousSchema]] with bounded range.
   *
   * @param minimum The lower bound (minimum value).
   * @param maximum The upper bound (maximum value).
   */
  def apply[C <: Codex with ValueCodex](minimum: C#T, maximum: C#T)(
    implicit codex: C, num: Numeric[C#T]): ContinuousSchema[C] = {
    ContinuousSchema(Some(minimum), Some(maximum))
  }
}

/**
 * Schema for discrete variables.
 *
 * @param minimum The optional minimum value.
 * @param maximum The optional maximum value.
 * @param step    The optional step size.
 *
 * @note The constructor is private to ensure a clean interface as
 *       provided by the `apply` methods of the companion object.
 */
case class DiscreteSchema[C <: Codex with ValueCodex] private (
  minimum: Option[C#T], maximum: Option[C#T], step: Option[C#T])(
    implicit val codex: C, int: Integral[C#T]) extends NumericalSchema[C#T] {
  val kind = Type.Discrete

  def isValid[V <: Value](value: V): Boolean = {
    import int._

    (codex.fromValue(value), minimum, maximum, step) match {
      case (v, Some(l), Some(u), Some(s)) => (v >= l && v <= u && (v % s) == 0)
      case (v, None, None, None) => true
      case _ => false
    }
  }

  override def toString(): String = {
    val params = (minimum, maximum, step) match {
      case (Some(l), Some(u), Some(s)) =>
        l.toString + "," + u.toString + "," + s.toString
      case _ => ""
    }

    "DiscreteSchema[" + codex.toString + "](" + params + ")"
  }
}

/** Companion object to [[DiscreteSchema]]. */
object DiscreteSchema {
  /** Construct a [[DiscreteSchema]] with unbounded range and step size 1. */
  def apply[C <: Codex with ValueCodex]()(implicit codex: C,
    int: Integral[C#T]): DiscreteSchema[C] = {
    DiscreteSchema(None, None, None)
  }
  /**
   * Construct a [[DiscreteSchema]] with bounded range and step size.
   *
   * @param minimum The lower bound (minimum value).
   * @param maximum The upper bound (maximum value).
   * @param step    The step size.
   */
  def apply[C <: Codex with ValueCodex](minimum: C#T, maximum: C#T,
    step: C#T)(implicit codex: C, int: Integral[C#T]): DiscreteSchema[C] = {
    DiscreteSchema(Some(minimum), Some(maximum), Some(step))
  }
}

/** Base trait for schemas for categorical variables. */
trait CategoricalSchema[T] extends Schema {
  /** Optional values the variable can take. */
  val domain: Option[List[T]]

  def isValid[V <: Value](value: V): Boolean = {
    (codex.fromValue(value), domain) match {
      case (v, Some(d)) => d.contains(v)
      case (v, None) => true
      case _ => false
    }
  }
}

/**
 * Schema for nominal variables.
 *
 * @param domain The optional values of the variable.
 *
 * @note The constructor is private to ensure a clean interface as
 *       provided by the `apply` methods of the companion object.
 */
case class NominalSchema[C <: Codex with ValueCodex] private (
  domain: Option[List[C#T]])(implicit val codex: C)
  extends CategoricalSchema[C#T] {
  val kind = Type.Nominal

  override def toString(): String = {
    val params = domain match {
      case Some(d) => d.toString
      case _ => ""
    }

    "NominalSchema[" + codex.toString + "](" + params + ")"
  }
}

/** Companion object to [[NominalSchema]]. */
object NominalSchema {
  /** Construct a [[NominalSchema]] that can take on any value. */
  def apply[C <: Codex with ValueCodex]()(
    implicit codex: C): NominalSchema[C] = NominalSchema(None)
  /**
   * Construct a [[NominalSchema]].
   *
   * @param domain A list of values the variable can take on.
   */
  def apply[C <: Codex with ValueCodex](domain: List[C#T])(
    implicit codex: C): NominalSchema[C] = NominalSchema(Some(domain))
}

/**
 * Schema for ordinal variables.
 *
 * @param domain The optional values of the variable.
 *
 * @note The constructor is private to ensure a clean interface as
 *       provided by the `apply` methods of the companion object.
 */
case class OrdinalSchema[C <: Codex with ValueCodex] private (
  domain: Option[List[C#T]])(implicit val codex: C)
  extends CategoricalSchema[C#T] {
  val kind = Type.Ordinal

  override def toString(): String = {
    val params = domain match {
      case Some(d) => d.toString
      case _ => ""
    }

    "OrdinalSchema[" + codex.toString + "](" + params + ")"
  }
}

/** Companion object to [[OrdinalSchema]]. */
object OrdinalSchema {
  /** Construct a [[OrdinalSchema]] that can take on any value. */
  def apply[C <: Codex with ValueCodex]()(
    implicit codex: C): OrdinalSchema[C] = OrdinalSchema(None)
  /**
   * Construct a [[OrdinalSchema]].
   *
   * @param domain A list of values the variable can take on.
   */
  def apply[C <: Codex with ValueCodex](domain: List[C#T])(
    implicit codex: C): OrdinalSchema[C] = OrdinalSchema(Some(domain))
}

/** Schema for date variables. */
case class DateSchema[C <: Codex with ValueCodex]()(
  implicit val codex: C) extends Schema {
  val kind = Type.Date

  def isValid[V <: Value](value: V): Boolean = {
    !value.asDate.isEmpty
  }

  override def toString(): String = {
    "DateSchema[" + codex.toString + "]()"
  }
}

