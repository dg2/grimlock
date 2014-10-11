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

package grimlock.contents

import com.twitter.scalding._
import com.twitter.scalding.TDsl._, Dsl._
import cascading.flow.FlowDef

import grimlock.contents.encoding._
import grimlock.contents.events._
import grimlock.contents.metadata._
import grimlock.contents.variable._

/** Contents of a cell in a [[Matrix]]. */
trait Content {
  /** Schema (description) of the value. */
  val schema: Schema
  /** The value of the variable. */
  val value: Value

  override def toString(): String = {
    "Content(" + schema.toString + "," + value.toString + ")"
  }

  /**
   * Converts the content to a consise (terse) string.
   *
   * @param separator The separator to use between the fields.
   *
   * @return Short string representation.
   */
  def toShortString(separator: String): String = {
    schema.toShortString(separator) + separator + value.toShortString
  }
}

object Content {
  /** Standard `unapply` method of pattern matching on [[Content]]. */
  def unapply(con: Content): Option[(Schema, Value)] = {
    Some((con.schema, con.value))
  }

  /**
   * Construct a [[Content]] using a [[metadata.ContinuousSchema]] and
   * `Numeric` value.
   *
   * @param schema Schema of the variable value.
   * @param value  `Numeric` value of the variable.
   */
  def apply[T](schema: ContinuousSchema[Codex.DoubleCodex],
    value: T)(implicit num: Numeric[T]): Content = {
    import num._
    ContentImpl(schema, DoubleValue(value.toDouble, schema.codex))
  }

  /**
   * Construct a [[Content]] using a [[metadata.ContinuousSchema]] and
   * `Integral` value.
   *
   * @param schema Schema of the variable value.
   * @param value  `Integral` value of the variable.
   */
  def apply[T](schema: ContinuousSchema[Codex.LongCodex],
    value: T)(implicit num: Integral[T]): Content = {
    import num._
    ContentImpl(schema, LongValue(value.toLong, schema.codex))
  }

  /**
   * Construct a [[Content]] using a [[metadata.DiscreteSchema]] and
   * `Numeric` value.
   *
   * @param schema Schema of the variable value.
   * @param value  `Numeric` value of the variable.
   */
  def apply[T](schema: DiscreteSchema[Codex.DoubleCodex],
    value: T)(implicit num: Numeric[T]): Content = {
    import num._
    ContentImpl(schema, DoubleValue(value.toDouble, schema.codex))
  }

  /**
   * Construct a [[Content]] using a [[metadata.DiscreteSchema]] and
   * `Integral` value.
   *
   * @param schema Schema of the variable value.
   * @param value  `Integral` value of the variable.
   */
  def apply[T](schema: DiscreteSchema[Codex.LongCodex],
    value: T)(implicit num: Integral[T]): Content = {
    import num._
    ContentImpl(schema, LongValue(value.toLong, schema.codex))
  }

  /**
   * Construct a [[Content]] using a [[metadata.NominalSchema]] and value.
   *
   * @param schema Schema of the variable value.
   * @param value  Value of the variable.
   *
   * @note The value is converted, and stored, as `String`.
   */
  def apply[T](schema: NominalSchema[Codex.StringCodex], value: T): Content = {
    ContentImpl(schema, StringValue(value.toString, schema.codex))
  }

  /**
   * Construct a [[Content]] using a [[metadata.NominalSchema]] and
   * `Numeric` value.
   *
   * @param schema Schema of the variable value.
   * @param value  `Numeric` value of the variable.
   */
  def apply[T](schema: NominalSchema[Codex.DoubleCodex],
    value: T)(implicit num: Numeric[T]): Content = {
    import num._
    ContentImpl(schema, DoubleValue(value.toDouble, schema.codex))
  }

  /**
   * Construct a [[Content]] using a [[metadata.NominalSchema]] and
   * `Integral` value.
   *
   * @param schema Schema of the variable value.
   * @param value  `Integral` value of the variable.
   */
  def apply[T](schema: NominalSchema[Codex.LongCodex],
    value: T)(implicit num: Integral[T]): Content = {
    import num._
    ContentImpl(schema, LongValue(value.toLong, schema.codex))
  }

  /**
   * Construct a [[Content]] using a [[metadata.OrdinalSchema]] and value.
   *
   * @param schema Schema of the variable value.
   * @param value  Value of the variable.
   *
   * @note The value is converted, and stored, as `String`.
   */
  def apply[T](schema: OrdinalSchema[Codex.StringCodex],
    value: T): Content = {
    ContentImpl(schema, StringValue(value.toString, schema.codex))
  }

  /**
   * Construct a [[Content]] using a [[metadata.OrdinalSchema]] and
   * `Numeric` value.
   *
   * @param schema Schema of the variable value.
   * @param value  `Numeric` value of the variable.
   */
  def apply[T](schema: OrdinalSchema[Codex.DoubleCodex],
    value: T)(implicit num: Numeric[T]): Content = {
    import num._
    ContentImpl(schema, DoubleValue(value.toDouble, schema.codex))
  }

  /**
   * Construct a [[Content]] using a [[metadata.OrdinalSchema]] and
   * `Integral` value.
   *
   * @param schema Schema of the variable value.
   * @param value  `Integral` value of the variable.
   */
  def apply[T](schema: OrdinalSchema[Codex.LongCodex],
    value: T)(implicit num: Integral[T]): Content = {
    import num._
    ContentImpl(schema, LongValue(value.toLong, schema.codex))
  }

  /**
   * Construct a [[Content]] using a [[metadata.DateSchema]] and
   * `java.util.Date` value.
   *
   * @param schema Schema of the variable value.
   * @param value  Date value of the variable.
   */
  def apply[T <: DateAndTimeCodex](schema: DateSchema[T],
    value: java.util.Date): Content = {
    ContentImpl(schema, DateValue(value, schema.codex))
  }

  private case class ContentImpl(schema: Schema, value: Value) extends Content
}

/**
 * Rich wrapper around a `TypedPipe[`[[Content]]`]`.
 *
 * @param data The `TypedPipe[`[[Content]]`]`.
 */
class ContentPipe(data: TypedPipe[Content]) {
  /**
   * Persist a [[ContentPipe]] to disk.
   *
   * @param file        Name of the output file.
   * @param separator   Separator to use between the fields of a [[Content]].
   * @param descriptive Indicates if the output should be descriptive.
   *
   * @return A Scalding `TypedPipe[`[[Content]]`]` which is this
   *         [[ContentPipe]].
   */
  def persist(file: String, separator: String = "|",
    descriptive: Boolean = false)(implicit flow: FlowDef,
      mode: Mode): TypedPipe[Content] = {
    data
      .map {
        case c => descriptive match {
          case true => c.toString
          case false => c.toShortString(separator)
        }
      }
      .toPipe('line)
      .write(TextLine(file))

    data
  }
}

/** Companion object for the [[ContentPipe]] class. */
object ContentPipe {
  /** Converts a `TypedPipe[`[[Content]]`]` to a [[ContentPipe]]. */
  implicit def typedPipeContent(data: TypedPipe[Content]): ContentPipe = {
    new ContentPipe(data)
  }
}

