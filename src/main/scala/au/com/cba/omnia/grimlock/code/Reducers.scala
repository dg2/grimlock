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

package grimlock.reduce

import grimlock._
import grimlock.contents._
import grimlock.contents.encoding._
import grimlock.contents.metadata._
import grimlock.contents.variable._
import grimlock.contents.variable.Type._
import grimlock.Matrix._
import grimlock.position._

/** Convenience trait for [[Reducer]]s that can prepare with or without a user supplied value. */
trait PrepareAndWithValue extends Prepare with PrepareWithValue { self: Reducer =>
  type V = Any

  def prepare[P <: Position, D <: Dimension](slc: Slice[P, D], pos: P, con: Content, ext: V): T = prepare(slc, pos, con)
}

/** Convenience trait for [[Reducer]]s that present a value both as [[PresentSingle]] and [[PresentMultiple]]. */
trait PresentSingleAndMultiple extends PresentSingle with PresentMultiple { self: Reducer =>
  /** [[position.coordinate.Coordinate]] name to use when presenting the value as [[PresentMultiple]]. */
  val name: String

  def presentSingle[P <: Position](pos: P, t: T): Option[(P, Content)] = content(t).map { case c => (pos, c) }
  def presentMultiple[P <: Position with ExpandablePosition](pos: P,
    t: T): Option[Either[(P#M, Content), List[(P#M, Content)]]] = content(t).map { case c => Left((pos.append(name), c)) }

  protected def content(t: T): Option[Content]
}

/**
 * Count reductions.
 *
 * @param name [[position.coordinate.Coordinate]] for the count values.
 *
 * @note `name` is only used when presenting [[PresentMultiple]].
 */
case class Count(name: String = "count") extends Reducer with PrepareAndWithValue with PresentSingleAndMultiple {
  type T = Long

  def prepare[P <: Position, D <: Dimension](slc: Slice[P, D], pos: P, con: Content): T = 1
  def reduce(lt: T, rt: T): T = lt + rt

  protected def content(t: T): Option[Content] = Some(Content(DiscreteSchema[Codex.LongCodex](), t))
}

/**
 * Moments of a distribution.
 *
 * @param strict Indicates if strict data handling is required. If so then any non-numeric
 *               value fails the reduction. If not then non-numeric values are silently
 *               ignored.
 * @param nan    Indicator if 'NaN' string should be output if the reduction failed (for
 *               example due to non-numeric data).
 * @param only   Subset of moments (1..4) to compute.
 * @param names  Names of the computed moments.
 *
 * @note `names` is only used when presenting [[PresentMultiple]].
 */
case class Moments(strict: Boolean = true, nan: Boolean = false, only: List[Int] = List(1, 2, 3, 4),
  names: List[String] = List("mean", "std", "skewness", "kurtosis")) extends Reducer with PrepareAndWithValue
  with PresentSingle with PresentMultiple {
  type T = com.twitter.algebird.Moments

  def prepare[P <: Position, D <: Dimension](slc: Slice[P, D], pos: P, con: Content): T = {
    com.twitter.algebird.Moments(con.value.asDouble.getOrElse(Double.NaN))
  }
  def reduce(lt: T, rt: T): T = {
    if (lt.mean.isNaN) { if (strict) { lt } else { rt } }
    else if (rt.mean.isNaN) { if (strict) { rt } else { lt } }
    else { com.twitter.algebird.Monoid.plus(lt, rt) }
  }
  def presentSingle[P <: Position](pos: P, t: T): Option[(P, Content)] = content(t).map { case cl => (pos, cl(only(0))) }
  def presentMultiple[P <: Position with ExpandablePosition](pos: P,
    t: T): Option[Either[(P#M, Content), List[(P#M, Content)]]] = {
    content(t).map { case cl => Right(only.map { case i => (pos.append(names(i - 1)), cl(i - 1)) }) }
  }

  protected def content(t: T): Option[List[Content]] = {
    if (t.mean.isNaN && !nan) {
      None
    } else if (t.mean.isNaN) {
      val con = Content(ContinuousSchema[Codex.DoubleCodex](), Double.NaN)

      Some(List(con, con, con, con))
    } else {
      Some(List(Content(ContinuousSchema[Codex.DoubleCodex](), t.mean),
        Content(ContinuousSchema[Codex.DoubleCodex](), t.stddev),
        Content(ContinuousSchema[Codex.DoubleCodex](), t.skewness),
        Content(ContinuousSchema[Codex.DoubleCodex](), t.kurtosis)))
    }
  }
}

/** Base trait for reducers that return a `Double` value. */
trait DoubleReducer extends Reducer with PrepareAndWithValue with PresentSingleAndMultiple {
  type T = Double

  /**
   * Indicates if strict data handling is required. If so then any non-numeric
   * value fails the reduction. If not then non-numeric values are silently
   * ignored.
   */
  val strict: Boolean

  def prepare[P <: Position, D <: Dimension](slc: Slice[P, D], pos: P, con: Content): T = {
    con.value.asDouble.getOrElse(Double.NaN)
  }
  def reduce(lt: T, rt: T): T = {
    if (lt.isNaN) { if (strict) { lt } else { rt } }
    else if (rt.isNaN) { if (strict) { rt } else { lt } }
    else { reduction(lt, rt) }
  }

  /**
   * Indicator if 'NaN' string should be output if the reduction failed
   * (for example due to non-numeric data).
   */
  val nan: Boolean

  protected def reduction(x: T, y: T): T
  protected def content(t: T): Option[Content] = {
    (t.isNaN && !nan) match {
      case true => None
      case false => Some(Content(ContinuousSchema[Codex.DoubleCodex](), t))
    }
  }
}

/** Minimum value reduction. */
case class Min(strict: Boolean = true, nan: Boolean = false, name: String = "min") extends DoubleReducer {
  def reduction(x: T, y: T): T = math.min(x, y)
}

/** Maximum value reduction. */
case class Max(strict: Boolean = true, nan: Boolean = false, name: String = "max") extends DoubleReducer {
  def reduction(x: T, y: T): T = math.max(x, y)
}

/** Maximum absolute value reduction. */
case class MaxAbs(strict: Boolean = true, nan: Boolean = false, name: String = "max.abs") extends DoubleReducer {
  def reduction(x: T, y: T): T = math.max(math.abs(x), math.abs(y))
}

/** Sum value reduction. */
case class Sum(strict: Boolean = true, nan: Boolean = false, name: String = "sum") extends DoubleReducer {
  def reduction(x: T, y: T): T = x + y
}

/**
 * Compute histogram.
 *
 * @param all       Indicator if histogram should apply to all data, or just [[contents.variable.Type.Categorical]].
 * @param meta      Return meta data statistics of the histogram (num categories, frequency ratio, entropy) also.
 * @param names     Names for the meta data statistics.
 * @param prefix    Prefix string for use on categories.
 * @param separator If a `prefix` is used, this is the separator used in [[position.Position.toShortString]].
 *
 * @note Usage of a `%s` in the prefix will be substituded with [[position.Position.toShortString]].
 */
// TODO: Add option to limit maximum number of categories
case class Histogram(all: Boolean = false, meta: Boolean = true, names: List[String] = List("num.cat", "entropy", "freq.ratio"),
  prefix: Option[String] = Some("%s="), separator: String = "") extends Reducer with PrepareAndWithValue with PresentMultiple {
  type T = Option[Map[String, Long]]

  def prepare[P <: Position, D <: Dimension](slc: Slice[P, D], pos: P, con: Content): T = {
    (con.schema.kind.isSpecialisationOf(Categorical) || all) match {
      case true => Some(Map(con.value.toShortString -> 1))
      case false => None
    }
  }

  def reduce(lt: T, rt: T): T = {
    (lt, rt) match {
      case (Some(lm), Some(rm)) => Some(lm ++ rm.map { case (k, v) => k -> (v + lm.getOrElse(k, 0L)) })
      case (Some(_), None) => lt
      case (None, Some(_)) => rt
      case _ => None
    }
  }

  def presentMultiple[P <: Position with ExpandablePosition](pos: P,
    t: T): Option[Either[(P#M, Content), List[(P#M, Content)]]] = {
    t.map {
      case m =>
        val counts = m.values.toList.sorted
        val stats = (pos.append(names(0)), Content(DiscreteSchema[Codex.LongCodex](), counts.size)) +:
          ((counts.size > 1) match {
            case true =>
              val ratio = counts.last.toDouble / counts(counts.length - 2)
              val entropy = -counts.map {
                case c => val f = c.toDouble / counts.sum; f * (math.log(f) / math.log(2))
              }.sum

              List((pos.append(names(1)), Content(ContinuousSchema[Codex.DoubleCodex](), entropy)),
                (pos.append(names(2)), Content(ContinuousSchema[Codex.DoubleCodex](), ratio)))
            case false => List()
          })
        val vals = (m.map {
          case (k, v) => prefix match {
            case Some(fmt) => (pos.append(fmt.format(pos.toShortString(separator)) + k),
              Content(DiscreteSchema[Codex.LongCodex](), v))
            case None => (pos.append(k), Content(DiscreteSchema[Codex.LongCodex](), v))
          }
        }).toList

        Right(if (meta) stats ++ vals else vals)
    }
  }
}

/**
 * Compute counts of values less-or-equal or greater than some `threshold`.
 *
 * @param strict    Indicates if strict data handling is required. If so then
 *                  any non-numeric value fails the reduction. If not then
 *                  non-numeric values are silently ignored.
 * @param na        The `String` to use if the reduction failed (for example
 *                  due to non-numeric data).
 * @param threshold The threshold value.
 * @param names     Names of the counts.
 */
// TODO: Test this
case class ThresholdCount(strict: Boolean = true, nan: Boolean = false, threshold: Double = 0,
  names: List[String] = List("leq.count", "gtr.count")) extends Reducer with PrepareAndWithValue with PresentMultiple {
  type T = (Long, Long) // (leq, gtr)

  def prepare[P <: Position, D <: Dimension](slc: Slice[P, D], pos: P, con: Content): T = {
    con.value.asDouble match {
      case Some(v) => if (v > threshold) (0, 1) else (1, 0)
      case _ => (-1, -1)
    }
  }

  def reduce(lt: T, rt: T): T = {
    if (lt._1 < 0) { if (strict) { lt } else { rt } }
    else if (rt._1 < 0) { if (strict) { rt } else { lt } }
    else { (lt._1 + rt._1, lt._2 + rt._2) }
  }

  def presentMultiple[P <: Position with ExpandablePosition](pos: P,
    t: T): Option[Either[(P#M, Content), List[(P#M, Content)]]] = {
    content(t).map { case cl => Right(names.zip(cl).map { case (n, c) => (pos.append(n), c) }) }
  }

  private def content(t: T): Option[List[Content]] = {
    if (t._1 < 0 && !nan) {
      None
    } else {
      Some(List(Content(DiscreteSchema[Codex.LongCodex](), t._1),
        Content(DiscreteSchema[Codex.LongCodex](), t._2)))
    }
  }
}

/**
 * Weighted sum reduction. This is particularly useful for scoring linear models.
 *
 * @param dim    Dimension for for which to create weigthed variables.
 * @param state  Name of the field in the user supplied value which is used as the weights.
 */
case class WeightedSum(dim: Dimension, state: String = "weight") extends Reducer with PrepareWithValue with PresentSingle {
  type T = Double
  type V = Map[Position1D, Map[Position1D, Content]]

  def prepare[P <: Position, D <: Dimension](slc: Slice[P, D], pos: P, con: Content, ext: V): T = {
    val key = Position1D(pos.get(dim))

    if (con.schema.kind.isSpecialisationOf(Numerical) && ext.isDefinedAt(key)) {
      (con.value.asDouble, ext(key)(Position1D(state)).value.asDouble) match {
        case (Some(v), Some(w)) => v * w
        case _ => throw new Exception("Unable to compute weighted sum with non-numeric value")
      }
    } else {
      0
    }
  }
  def reduce(lt: T, rt: T): T = lt + rt
  def presentSingle[P <: Position](pos: P, t: T): Option[(P, Content)] = content(t).map { case c => (pos, c) }

  private def content(t: T): Option[Content] = Some(Content(ContinuousSchema[Codex.DoubleCodex](), t))
}

/**
 * Distinct count reductions.
 *
 * @param name [[position.coordinate.Coordinate]] for the distinct count values.
 *
 * @note `name` is only used when presenting [[PresentMultiple]].
 */
// TODO: Test this
case class DistinctCount(name: String = "distinct.count") extends Reducer with PrepareAndWithValue with PresentSingleAndMultiple {
  type T = Set[String]

  def prepare[P <: Position, D <: Dimension](slc: Slice[P, D], pos: P, con: Content): T = Set(con.value.toShortString)
  def reduce(lt: T, rt: T): T = lt ++ rt

  protected def content(t: T): Option[Content] = Some(Content(DiscreteSchema[Codex.LongCodex](), t.size))
}

/**
 * Compute percentiles.
 *
 * @param percentiles Number of percentiles to compute.
 * @param name        Name format for the percentiles.
 *
 * @note Usage of a `%d` in the name will be substituded with percentile index.
 */
// TODO: Test this
case class Percentiles(percentiles: Int,
  name: Option[String] = Some("percentile.%d")) extends Reducer with PrepareAndWithValue with PresentMultiple {
  type T = Map[Double, Long]

  def prepare[P <: Position, D <: Dimension](slc: Slice[P, D], pos: P, con: Content): T = {
    val map = new scala.collection.immutable.TreeMap[Double, Long]()

    con.schema.kind.isSpecialisationOf(Numerical) match {
      case true => map + (con.value.asDouble.get -> 1)
      case false => map
    }
  }

  def reduce(lt: T, rt: T): T = {
    lt ++ rt.map { case (k, v) => k -> (v + lt.getOrElse(k, 0L)) }
  }

  def presentMultiple[P <: Position with ExpandablePosition](pos: P,
    t: T): Option[Either[(P#M, Content), List[(P#M, Content)]]] = {

    val keys = t.keys.toList
    val values = t.values

    val cumsum = values.tail.scanLeft(values.head)(_ + _).toList
    val N = cumsum.last.toDouble

    val boundaries = for (cnt <- (0.0 until N by (N / percentiles)).tail) yield keys(cumsum.indexWhere(_ >= cnt))

    Some(Right((boundaries.zipWithIndex.map {
      case (p, i) => name match {
        case Some(fmt) => (pos.append(fmt.format(i + 1)), Content(ContinuousSchema[Codex.DoubleCodex](), p))
        case None => (pos.append((i + 1).toString), Content(ContinuousSchema[Codex.DoubleCodex](), p))
      }
    }).toList))
  }
}

