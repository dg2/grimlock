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

package au.com.cba.omnia.grimlock.transform

import au.com.cba.omnia.grimlock.contents._
import au.com.cba.omnia.grimlock.contents.encoding._
import au.com.cba.omnia.grimlock.contents.metadata._
import au.com.cba.omnia.grimlock.contents.variable.Type._
import au.com.cba.omnia.grimlock.position._
import au.com.cba.omnia.grimlock.position.coordinate._

/** Convenience trait for transformations involving `Double` values. */
trait AsDouble {
  protected def getAsDouble[T: Coordinateable, U: Coordinateable](con: Content,
    ext: Map[Position1D, Map[Position1D, Content]], index1: T,
    index2: U): Option[Double] = {
    val key1 = Position1D(index1)
    val key2 = Position1D(index2)

    (con.schema.kind.isSpecialisationOf(Numerical) && ext.isDefinedAt(key1) &&
      ext(key1).isDefinedAt(key2)) match {
        case true => ext(key1)(key2).value.asDouble
        case false => None
      }
  }

  protected def returnSingle[P <: Position with ModifyablePosition](pos: P,
    dim: Dimension, name: String,
    value: Double): Option[Either[(P#S, Content), List[(P#S, Content)]]] = {
    Some(Left((pos.set(dim, name),
      Content(ContinuousSchema[Codex.DoubleCodex](), value))))
  }
}

/**
 * Convenience trait for [[Transformer]]s that present a `Double` value using a
 * user supplied value.
 */
trait PresentAsDoubleWithValue extends PresentWithValue with AsDouble {
  self: Transformer =>
  type V = Map[Position1D, Map[Position1D, Content]]
}

/**
 * Convenience trait for [[Transformer]]s that present a `Double` value with or
 * without using a user supplied value.
 */
trait PresentAsDoubleAndWithValue extends PresentAndWithValue with AsDouble {
  self: Transformer =>
}

/**
 * Convenience trait for [[Transformer]]s that append a `Double` value using a
 * user supplied value.
 */
trait PresentExpandedAsDoubleWithValue extends PresentExpandedWithValue
  with AsDouble { self: Transformer =>
  type V = Map[Position1D, Map[Position1D, Content]]
}

/**
 * Create indicator variables.
 *
 * @param dim    [[position.Dimension]] for which to create indicator variables.
 * @param suffix Suffix for the new name of the coordinate at `dim`.
 *
 * @note The returned [[position.Position]] will have a
 *       [[position.coordinate.StringCoordinate]] at index `dim`. The value of
 *       the [[position.coordinate.Coordinate]] is the string representation of
 *       the original [[position.coordinate.Coordinate]] at `dim` with the
 *       `suffix` appended.
 */
case class Indicator(dim: Dimension, suffix: String = ".ind")
  extends Transformer with PresentAndWithValue {
  def present[P <: Position with ModifyablePosition](pos: P, con: Content) = {
    Some(Left((pos.set(dim, pos.get(dim).toShortString + suffix),
      Content(DiscreteSchema[Codex.LongCodex](), 1))))
  }
}

/**
 * Binarise categorical variables.
 *
 * @param dim       [[position.Dimension]] for for which to create binarised
 *                  variables.
 * @param separator Sparator between [[position.coordinate.Coordinate]] and
 *                  [[contents.variable.Value]].
 *
 * @note Binarisation is only applied to [[contents.variable.Type.Categorical]]
 *       variables.
 * @note The returned [[position.Position]] will have a
 *       [[position.coordinate.StringCoordinate]] at index `dim`. The value of
 *       the [[position.coordinate.Coordinate]] is the string representation of
 *       the original [[position.coordinate.Coordinate]] at `dim` with
 *       [[contents.variable.Value]] appended (separated by `separator`).
 */
case class Binarise(dim: Dimension, separator: String = "=")
  extends Transformer with PresentAndWithValue {
  def present[P <: Position with ModifyablePosition](pos: P, con: Content) = {
    (con.schema.kind.isSpecialisationOf(Categorical)) match {
      case true =>
        Some(Left((pos.set(dim,
          pos.get(dim).toShortString + separator + con.value.toShortString),
          Content(DiscreteSchema[Codex.LongCodex](), 1))))
      case false => None
    }
  }
}

/**
 * Normalise numeric variables.
 *
 * @param dim    [[position.Dimension]] for for which to create normalised
 *               variables.
 * @param state  Name of the field in user supplied value which is used as the
 *               normalisation constant.
 * @param suffix Suffix for the new name of the coordinate at `dim`.
 *
 * @note Normalisation scales a variable in the range [-1, 1].
 * @note Normalisation is only applied to [[contents.variable.Type.Numerical]]
 *       variables.
 * @note The returned [[position.Position]] will have a
 *       [[position.coordinate.StringCoordinate]] at index `dim`. The value of
 *       the [[position.coordinate.Coordinate]] is the string representation of
 *       the original [[position.coordinate.Coordinate]] at `dim` with the
 *       `suffix` appended.
 */
case class Normalise(dim: Dimension, state: String = "max.abs",
  suffix: String = "") extends Transformer with PresentAsDoubleWithValue {
  def present[P <: Position with ModifyablePosition](pos: P, con: Content,
    ext: V) = {
    (con.value.asDouble, getAsDouble(con, ext, pos.get(dim), state)) match {
      case (Some(v), Some(m)) =>
        returnSingle(pos, dim, pos.get(dim).toShortString + suffix, v / m)
      case _ => None
    }
  }
}

/**
 * Standardise numeric variables.
 *
 * @param dim       [[position.Dimension]] for for which to create standardised
 *                  variables.
 * @param state     List of names of the fields in the user supplied value
 *                  which is used as the standardisation constants.
 * @param threshold Minimum standard deviation threshold. Values less than this
 *                  result in standardised value of zero.
 * @param suffix    Suffix for the new name of the coordinate at `dim`.
 *
 * @note Standardisation results in a variable with zero mean and variance
 *       of one.
 * @note Standardisation is only applied to
 *       [[contents.variable.Type.Numerical]] variables.
 * @note The returned [[position.Position]] will have a
 *       [[position.coordinate.StringCoordinate]] at index `dim`. The value of
 *       the [[position.coordinate.Coordinate]] is the string representation of
 *       the original [[position.coordinate.Coordinate]] at `dim` with the
 *       `suffix` appended.
 */
case class Standardise(dim: Dimension,
  state: List[String] = List("mean", "std"), threshold: Double = 1e-4,
  suffix: String = "") extends Transformer with PresentAsDoubleWithValue {
  def present[P <: Position with ModifyablePosition](pos: P, con: Content,
    ext: V) = {
    (con.value.asDouble, getAsDouble(con, ext, pos.get(dim), state(0)),
      getAsDouble(con, ext, pos.get(dim), state(1))) match {
        case (Some(v), Some(m), Some(s)) =>
          returnSingle(pos, dim, pos.get(dim).toShortString + suffix,
            standardise(v, m, s))
        case _ => None
      }
  }

  private def standardise(v: Double, m: Double, s: Double) = {
    if (math.abs(s) < threshold) 0.0 else (v - m) / s
  }
}

/**
 * Clamp numeric variables.
 *
 * @param dim       [[position.Dimension]] for for which to create clamped
 *                  variables.
 * @param state     List of names of the fields in user supplied value which
 *                  is used as the clamping constants.
 * @param suffix    Suffix for the new name of the coordinate at `dim`.
 * @param andThen   Optional transformation to apply after clamping.
 *
 * @note Clamping results in a variable not smaller (or greater) than the
 *       clamping constants.
 * @note Clamping is only applied to [[contents.variable.Type.Numerical]]
 *       variables.
 * @note The returned [[position.Position]] will have a
 *       [[position.coordinate.StringCoordinate]] at index `dim`. The value of
 *       the [[position.coordinate.Coordinate]] is the string representation of
 *       the original [[position.coordinate.Coordinate]] at `dim` with the
 *       `suffix` appended.
 */
case class Clamp[T <: Transformer with PresentWithValue](dim: Dimension,
  state: List[String] = List("min", "max"), suffix: String = "",
  andThen: Option[T] = None) extends Transformer with PresentAsDoubleWithValue {
  def present[P <: Position with ModifyablePosition](pos: P, con: Content,
    ext: V) = {
    (con.value.asDouble, getAsDouble(con, ext, pos.get(dim), state(0)),
      getAsDouble(con, ext, pos.get(dim), state(1))) match {
        case (Some(v), Some(l), Some(u)) =>
          val cell = (pos.set(dim, pos.get(dim).toShortString + suffix),
            Content(ContinuousSchema[Codex.DoubleCodex](), clamp(v, l, u)))

          andThen match {
            case Some(t) => t.present(cell._1.asInstanceOf[P], cell._2,
              ext.asInstanceOf[t.V])
            case None => Some(Left(cell))
          }
        case _ => None
      }
  }

  private def clamp(v: Double, l: Double, u: Double) = {
    if (v < l) l else if (v > u) u else v
  }
}

/**
 * Compute the inverse document frequency.
 *
 * @param from  [[position.Dimension]] of the documents.
 * @param state Name of the field in user supplied value which is used as the
 *              number of documents.
 * @param name  Name of the idf [[position.coordinate.Coordinate]].
 * @param log   Log function to use.
 * @param add   Amount to add to the idf numerator.
 * @param lower Lower bound filtering; terms appearing in fewer than this
 *              documents are removed.
 * @param upper Upper bound filtering; terms appearing in more than this
 *              documents are removed.
 *
 * @note Idf is only applied to [[contents.variable.Type.Numerical]] variables.
 * @note The returned [[position.Position]] will have an extra dimension
 *       appended.
 */
case class Idf(from: Dimension, state: String = "size", name: String = "idf",
  log: (Double) => Double = math.log, add: Int = 1,
  lower: Long = Long.MinValue, upper: Long = Long.MaxValue) extends Transformer
  with PresentExpandedAsDoubleWithValue {
  def present[P <: Position with ExpandablePosition](pos: P, con: Content,
    ext: V) = {
    (con.value.asDouble, getAsDouble(con, ext, from.toString, state)) match {
      case (Some(df), Some(n)) if ((df > lower) && (df < upper)) =>
        Some(Left((pos.append(name),
          Content(ContinuousSchema[Codex.DoubleCodex](),
            log(n / (add + df.toDouble))))))
      case _ => None
    }
  }
}

/**
 * Create boolean term frequencies; all term frequencies are binarised.
 *
 * @param dim    [[position.Dimension]] for which to create boolean term
 *               frequencies.
 * @param suffix Suffix for the new name of the coordinate at `dim`.
 *
 * @note Boolean tf is only applied to [[contents.variable.Type.Numerical]]
 *       variables.
 * @note The returned [[position.Position]] will have a
 *       [[position.coordinate.StringCoordinate]] at index `dim`. The value
 *       of the [[position.coordinate.Coordinate]] is the string representation
 *       of the original [[position.coordinate.Coordinate]] at `dim` with the
 *       `suffix` appended.
 */
case class BooleanTf(dim: Dimension, suffix: String = "") extends Transformer
  with PresentAsDoubleAndWithValue {
  def present[P <: Position with ModifyablePosition](pos: P, con: Content) = {
    (con.schema.kind.isSpecialisationOf(Numerical)) match {
      case true =>
        returnSingle(pos, dim, pos.get(dim).toShortString + suffix, 1)
      case false => None
    }
  }
}

/**
 * Create logarithmic term frequencies.
 *
 * @param dim    [[position.Dimension]] for which to create boolean term
 *               frequencies.
 * @param suffix Suffix for the new name of the coordinate at `dim`.
 * @param log    Log function to use.
 *
 * @note Logarithmic tf is only applied to [[contents.variable.Type.Numerical]]
 *       variables.
 * @note The returned [[position.Position]] will have a
 *       [[position.coordinate.StringCoordinate]] at index `dim`. The value of
 *       the [[position.coordinate.Coordinate]] is the string representation of
 *       the original [[position.coordinate.Coordinate]] at `dim` with the
 *       `suffix` appended.
 */
case class LogarithmicTf(dim: Dimension, suffix: String = "",
  log: (Double) => Double = math.log) extends Transformer
  with PresentAsDoubleAndWithValue {
  def present[P <: Position with ModifyablePosition](pos: P, con: Content) = {
    (con.schema.kind.isSpecialisationOf(Numerical), con.value.asDouble) match {
      case (true, Some(tf)) =>
        returnSingle(pos, dim, pos.get(dim).toShortString + suffix, log(tf + 1))
      case _ => None
    }
  }
}

/**
 * Create augmented term frequencies.
 *
 * @param dim    [[position.Dimension]] for which to create boolean term
 *               frequencies.
 * @param state  Name of the field in user supplied value which is used as the
 *               maximum term frequency.
 * @param suffix Suffix for the new name of the coordinate at `dim`.
 *
 * @note Augmented tf is only applied to [[contents.variable.Type.Numerical]]
 *       variables.
 * @note The returned [[position.Position]] will have a
 *       [[position.coordinate.StringCoordinate]] at index `dim`. The value of
 *       the [[position.coordinate.Coordinate]] is the string representation of
 *       the original [[position.coordinate.Coordinate]] at `dim` with the
 *       `suffix` appended.
 */
case class AugmentedTf(dim: Dimension, state: String = "max",
  suffix: String = "") extends Transformer with PresentAsDoubleWithValue {
  def present[P <: Position with ModifyablePosition](pos: P, con: Content,
    ext: V) = {
    (con.value.asDouble, getAsDouble(con, ext, pos.get(dim), state)) match {
      case (Some(tf), Some(m)) => returnSingle(pos, dim,
        pos.get(dim).toShortString + suffix, 0.5 + (0.5 * tf) / m)
      case _ => None
    }
  }
}

/**
 * Create tf-idf values.
 *
 * @param dim    [[position.Dimension]] for which to create boolean term
 *               frequencies.
 * @param state  Name of the field in user supplied value which is used as the
 *               inverse document frequency.
 * @param suffix Suffix for the new name of the coordinate at `dim`.
 *
 * @note Tf-idf is only applied to [[contents.variable.Type.Numerical]]
 *       variables.
 * @note The returned [[position.Position]] will have a
 *       [[position.coordinate.StringCoordinate]] at index `dim`. The value of
 *       the [[position.coordinate.Coordinate]] is the string representation of
 *       the original [[position.coordinate.Coordinate]] at `dim` with the
 *       `suffix` appended.
 */
case class TfIdf(dim: Dimension, state: String = "idf", suffix: String = "")
  extends Transformer with PresentAsDoubleWithValue {
  def present[P <: Position with ModifyablePosition](pos: P, con: Content,
    ext: V) = {
    (con.value.asDouble, getAsDouble(con, ext, pos.get(dim), state)) match {
      case (Some(tf), Some(idf)) =>
        returnSingle(pos, dim, pos.get(dim).toShortString + suffix, tf * idf)
      case _ => None
    }
  }
}

case class Subtract(dim: Dimension, state: String, suffix: String = "")
  extends Transformer with PresentAsDoubleWithValue {
  def present[P <: Position with ModifyablePosition](pos: P, con: Content,
    ext: V) = {
    (con.value.asDouble, getAsDouble(con, ext, pos.get(dim), state)) match {
      case (Some(l), Some(r)) =>
        returnSingle(pos, dim, pos.get(dim).toShortString + suffix, l - r)
      case _ => None
    }
  }
}

case class Square(dim: Dimension, suffix: String = "") extends Transformer
  with PresentAsDoubleAndWithValue {
  def present[P <: Position with ModifyablePosition](pos: P, con: Content) = {
    con.value.asDouble match {
      case Some(v) =>
        returnSingle(pos, dim, pos.get(dim).toShortString + suffix, v * v)
      case _ => None
    }
  }
}

case class SquareRoot(dim: Dimension, suffix: String = "") extends Transformer
  with PresentAsDoubleAndWithValue {
  def present[P <: Position with ModifyablePosition](pos: P, con: Content) = {
    con.value.asDouble match {
      case Some(v) => returnSingle(pos, dim,
        pos.get(dim).toShortString + suffix, math.sqrt(v))
      case _ => None
    }
  }
}

case class Divide(dim: Dimension, state: String, suffix: String = "",
  inverse: Boolean = false) extends Transformer with PresentAsDoubleWithValue {
  def present[P <: Position with ModifyablePosition](pos: P, con: Content,
    ext: V) = {
    (con.value.asDouble, getAsDouble(con, ext, pos.get(dim), state)) match {
      case (Some(l), Some(r)) => returnSingle(pos, dim,
        pos.get(dim).toShortString + suffix, if (inverse) r / l else l / r)
      case _ => None
    }
  }
}

case class Ratio(dim: Dimension, from: Dimension, state: String = "size",
  suffix: String = "", inverse: Boolean = false) extends Transformer
  with PresentAsDoubleWithValue {
  def present[P <: Position with ModifyablePosition](pos: P, con: Content,
    ext: V) = {
    (con.value.asDouble, getAsDouble(con, ext, from.toString, state)) match {
      case (Some(l), Some(r)) => returnSingle(pos, dim,
        pos.get(dim).toShortString + suffix, if (inverse) r / l else l / r)
      case _ => None
    }
  }
}

