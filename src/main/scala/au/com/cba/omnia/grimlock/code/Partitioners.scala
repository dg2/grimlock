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

package grimlock.partition

import grimlock.contents.encoding._
import grimlock.Matrix._
import grimlock.position._
import grimlock.position.coordinate._
import grimlock.utilities._

/** Implements default partitioners. */
object Partitioners {
  /**
   * Binary partition based on the hash code of a
   * [[position.coordinate.Coordinate]].
   *
   * @param dim   The [[position.Dimension]] to partition on.
   * @param ratio The binary split ratio (relative to `base`).
   * @param left  The identifier for the left partition.
   * @param right The identifier for the right partition.
   * @param base  The base for the ratio.
   *
   * @return A [[Partitioner.Partition]]
   *
   * @note The hash code modulo `base` is used for comparison with the ratio.
   * @note The [[position.Position]] is assigned to the left partition
   *       if it is less or equal to the `ratio` value.
   */
  def BinaryHashSplit[T, P <: Position](dim: Dimension, ratio: Int, left: T,
    right: T, base: Int = 100): Partitioner.Partition[T, P] =
    (pos: P, smo: Option[SliceMap]) => {
      Some(Left(if (math.abs(pos.get(dim).hashCode % base) <= ratio) left else right))
    }

  /**
   * Ternary partition based on the hash code of a
   * [[position.coordinate.Coordinate]].
   *
   * @param dim    The [[position.Dimension]] to partition on.
   * @param lower  The lower ternary split ratio (relative to `base`).
   * @param upper  The upper ternary split ratio (relative to `base`).
   * @param left   The identifier for the left partition.
   * @param middle The identifier for the middle partition.
   * @param right  The identifier for the right partition.
   * @param base   The base for the ratio.
   *
   * @return A [[Partitioner.Partition]]
   *
   * @note The hash code modulo `base` is used for comparison with lower/upper.
   * @note The [[position.Position]] is assigned to the partition `left` if
   *       it is less or equal to `lower`, `middle` if it is less of equal to
   *       `upper` or else to `right`.
   */
  // TODO: Test this
  def TernaryHashSplit[T, P <: Position](dim: Dimension, lower: Int,
    upper: Int, left: T, middle: T, right: T,
    base: Int = 100): Partitioner.Partition[T, P] =
    (pos: P, smo: Option[SliceMap]) => {
      val hash = math.abs(pos.get(dim).hashCode % base)

      Some(Left(if (hash <= lower) left else if (hash <= upper) middle else right))
    }

  /**
   * Partition based on the hash code of a [[position.coordinate.Coordinate]].
   *
   * @param dim    The [[position.Dimension]] to partition on.
   * @param ranges A `Map` holding the partitions and hash code ranges
   *               (relative to `base`) for each partition.
   * @param base   The base for hash code.
   *
   * @return A [[Partitioner.Partition]]
   *
   * @note The hash code modulo `base` is used for comparison with the range.
   * @note A [[position.Position]] falls in a range if it is (strictly) greater
   *       than the lower value (first value in tuple) and less or equal to the
   *       upper value (second value in tuple).
   */
  // TODO: Test this
  def HashSplit[T, P <: Position](dim: Dimension, ranges: Map[T, (Int, Int)],
    base: Int = 100): Partitioner.Partition[T, P] =
    (pos: P, smo: Option[SliceMap]) => {
      val hash = math.abs(pos.get(dim).hashCode % base)
      val parts = ranges.flatMap {
        case (k, (l, u)) if (hash > l && hash <= u) => Some(k)
        case _ => None
      }.toList

      Some(Right(parts))
    }

  /**
   * Binary partition based on the date of a [[position.coordinate.Coordinate]].
   *
   * @param dim   The [[position.Dimension]] to partition on.
   * @param date  The date around which to split.
   * @param left  The identifier for the left partition.
   * @param right The identifier for the right partition.
   *
   * @return A [[Partitioner.Partition]]
   *
   * @note The [[position.Position]] is assigned to the `left` partition
   *       if it is less or equal to the `date` value, to `right` otherwise.
   */
  // TODO: Test this
  def BinaryDateSplit[T, P <: Position](dim: Dimension, date: java.util.Date,
    left: T, right: T): Partitioner.Partition[T, P] =
    (pos: P, smo: Option[SliceMap]) => {
      val coord = pos.get(dim)
      val codex = coord.codex

      codex.compare(coord, DateCoordinate(date, codex)).map {
        case cmp => Left(if (cmp <= 0) left else right)
      }
    }

  /**
   * Ternary partition based on the date of a
   * [[position.coordinate.Coordinate]].
   *
   * @param dim    The [[position.Dimension]] to partition on.
   * @param lower  The lower date around which to split.
   * @param upper  The upper date around which to split.
   * @param left   The identifier for the left partition.
   * @param middle The identifier for the middle partition.
   * @param right  The identifier for the right partition.
   *
   * @return A [[Partitioner.Partition]]
   *
   * @note The [[position.Position]] is assigned to the partition `left` if
   *       it is less or equal to `lower`, `middle` if it is less of equal to
   *       `upper` or else to `right`.
   */
  // TODO: Test this
  def TernaryDateSplit[T, P <: Position](dim: Dimension, lower: java.util.Date,
    upper: java.util.Date, left: T, middle: T,
    right: T): Partitioner.Partition[T, P] =
    (pos: P, smo: Option[SliceMap]) => {
      val coord = pos.get(dim)
      val codex = coord.codex

      (codex.compare(coord, DateCoordinate(lower, codex)),
        codex.compare(coord, DateCoordinate(upper, codex))) match {
          case (Some(l), Some(u)) =>
            Some(Left(if (l <= 0) left else if (u <= 0) middle else right))
          case _ => None
        }
    }

  /**
   * Partition based on the date of a [[position.coordinate.Coordinate]].
   *
   * @param dim    The [[position.Dimension]] to partition on.
   * @param ranges A `Map` holding the partitions and date ranges for each
   *               partition.
   *
   * @return A [[Partitioner.Partition]]
   *
   * @note A [[position.Position]] falls in a range if it is (strictly)
   *       greater than the lower value (first value in tuple) and less or
   *       equal to the upper value (second value in tuple).
   */
  // TODO: Test this
  def DateSplit[T, P <: Position](dim: Dimension, ranges: Map[T, (java.util.Date, java.util.Date)]): Partitioner.Partition[T, P] =
    (pos: P, smo: Option[SliceMap]) => {
      val coord = pos.get(dim)
      val codex = coord.codex
      val parts = ranges.flatMap {
        case (k, (lower, upper)) =>
          (codex.compare(coord, DateCoordinate(lower, codex)),
            codex.compare(coord, DateCoordinate(upper, codex))) match {
              case (Some(l), Some(u)) if (l > 0 && u <= 0) => Some(k)
              case _ => None
            }
      }.toList

      Some(Right(parts))
    }
}

