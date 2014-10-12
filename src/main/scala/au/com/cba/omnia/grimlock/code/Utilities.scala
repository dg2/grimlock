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

package au.com.cba.omnia.grimlock.utilities

/** Hetrogeneous comparison results. */
trait CompareResult

/**
 * Invalid comparison, for example when comparing to values of different types.
 */
case object Invalid extends CompareResult
/** Result when x >= y. */
case object GreaterEqual extends CompareResult
/** Result when x > y. */
case object Greater extends CompareResult
/** Result when x = y. */
case object Equal extends CompareResult
/** Result when x < y. */
case object Less extends CompareResult
/** Result when x <= y. */
case object LessEqual extends CompareResult

object CompareResult {
  /**
   * Check if a standard comparison result matches a hetrogeneous comparison.
   *
   * @param cmp `Option` of standard comparison result.
   * @param op  The hetrogeneous comparison result to compare against.
   *
   * @return Indicator if the comparison results match or not.
   */
  def evaluate(cmp: Option[Int], op: CompareResult): Boolean = {
    cmp match {
      case Some(0) => (op == Equal) || (op == GreaterEqual) || (op == LessEqual)
      case Some(x) if (x > 0) => (op == Greater) || (op == GreaterEqual)
      case Some(x) if (x < 0) => (op == Less) || (op == LessEqual)
      case _ => false
    }
  }
}

/** Collection of simple utility functions. */
object Miscellaneous {
  private def flatten[T](tlo: Option[List[T]]): List[T] = {
    tlo match {
      case Some(tl) => tl
      case None => List()
    }
  }

  /** Transforms an `Option[Either[T, List[T]]]` to a `List[T]`. */
  def mapFlatten[T](toe: Option[Either[T, List[T]]]): List[T] = {
    flatten(toe.map {
      case Left(t) => List(t)
      case Right(tl) => tl
    })
  }

  /**
   * Transforms an `Option[Either[T, List[T]]]` together with a single `U`
   * to a `List[(T, U)]`.
   */
  def mapFlatten[T, U](toe: Option[Either[T, List[T]]], u: U): List[(T, U)] = {
    flatten(toe.map {
      case Left(t) => List((t, u))
      case Right(tl) => tl.map { case t => (t, u) }
    })
  }
}

