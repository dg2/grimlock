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
import grimlock.Matrix._
import grimlock.position._
import grimlock.utilities._

/**
 * Base trait for reductions.
 *
 * @note Aggregator/aggregate are already available on a TypedPipe. So, to
 *       prevent name clashes, Reducer/reduce are used instead. The net
 *       effect is still that Reducers aggregator over a [[Matrix]].
 */
trait Reducer {
  /** Type of the state being reduced (aggregated). */
  type T

  /**
   * Standard reduce method.
   *
   * @param lt Left state to reduce.
   * @param rt Right state to reduce.
   *
   * @return Reduced state
   */
  def reduce(lt: T, rt: T): T
}

/** Base trait for reduction preparation. */
trait Prepare { self: Reducer =>
  /**
   * Prepare for reduction.
   *
   * @param pos Original [[position.Position]] corresponding to the [[contents.Content]].
   *            That is, it's the position prior to [[Slice.selected]] being applied.
   * @param con [[contents.Content]] which is to be reduced.
   *
   * @return State to reduce.
   */
  def prepare[P <: Position, D <: Dimension](slc: Slice[P, D], pos: P, con: Content): T
}

/** Base trait for reduction preparation with a user supplied value. */
trait PrepareWithValue { self: Reducer =>
  type V

  /**
   * Prepare for reduction.
   *
   * @param pos Original [[position.Position]] corresponding to the [[contents.Content]].
   *            That is, it's the position prior to [[Slice.selected]] being applied.
   * @param con [[contents.Content]] which is to be reduced.
   * @param ext User provided data required for preparation.
   *
   * @return State to reduce.
   */
  def prepare[P <: Position, D <: Dimension](slc: Slice[P, D], pos: P, con: Content, ext: V): T
}

/** Base trait for reductions that return a single value. */
trait PresentSingle { self: Reducer =>
  /**
   * Present the reduced [[contents.Content]].
   *
   * @param pos The reduced [[position.Position]]. That is, the position returned by
   *            [[Slice.selected]].
   * @param t   The reduced state.
   *
   * @return Optional ([[position.Position]], [[contents.Content]]) tuple where the
   *         [[position.Position]] is `pos` and the [[contents.Content]] is derived
   *         from `t`.
   *
   * @note An `Option` is used in the return type to allow reducers to be selective
   *       in what [[contents.Content]] they apply to. For example, computing the
   *       mean is undefined for categorical variables. The reducer now has the
   *       option to return `None`. This in turn permits an external API, for simple
   *       cases, where the user need not know about the types of variables of their
   *       data.
   */
  def presentSingle[P <: Position](pos: P, t: T): Option[(P, Content)]
}

/** Base trait for reductions that return multiple values. */
trait PresentMultiple { self: Reducer =>
  /**
   * Present the reduced [[contents.Content]](s).
   *
   * @param pos The reduced [[position.Position]]. That is, the position returned by
   *            [[Slice.selected]].
   * @param t   The reduced state.
   *
   * @return Optional ([[position.ExpandablePosition.M]], [[contents.Content]]) tuple
   *         where the [[position.Position]] is creating by appending to `pos`
   *         ([[position.ExpandablePosition.append]]) and the [[contents.Content]](s)
   *         is derived from `t`.
   *
   * @note An `Option` is used in the return type to allow reducers to be selective
   *       in what [[contents.Content]] they apply to. For example, computing the
   *       mean is undefined for categorical variables. The reducer now has the
   *       option to return `None`. This in turn permits an external API, for simple
   *       cases, where the user need not know about the types of variables of their
   *       data.
   */
  def presentMultiple[P <: Position with ExpandablePosition](pos: P, t: T): Option[Either[(P#M, Content), List[(P#M, Content)]]]
}

/**
 * Reducer that is a combination of one or more [[Reducer]] with [[PresentMultiple]].
 *
 * @param reducers `List` of reducers that are combined together.
 *
 * @note This need not be called in an application. The
 *       [[ReducerableMultiple]] type class will convert any
 *       `List[`[[Reducer]]`]` automatically to one of these.
 */
case class CombinationReducerMultiple[T <: Reducer with Prepare with PresentMultiple](reducers: List[T]) extends Reducer
  with Prepare with PresentMultiple {
  type T = List[Any]

  def prepare[P <: Position, D <: Dimension](slc: Slice[P, D], pos: P, con: Content): T = {
    reducers.map { case reducer => reducer.prepare(slc, pos, con) }
  }

  def reduce(lt: T, rt: T): T = {
    (reducers, lt, rt).zipped.map {
      case (reducer, l, r) => reducer.reduce(l.asInstanceOf[reducer.T], r.asInstanceOf[reducer.T])
    }
  }

  def presentMultiple[P <: Position with ExpandablePosition](pos: P,
    t: T): Option[Either[(P#M, Content), List[(P#M, Content)]]] = {
    Some(Right((reducers, t).zipped.flatMap {
      case (reducer, s) => Miscellaneous.mapFlatten(reducer.presentMultiple(pos, s.asInstanceOf[reducer.T]))
    }))
  }
}

/**
 * Reducer that is a combination of one or more [[Reducer]] with [[PrepareWithValue]] with [[PresentMultiple]].
 *
 * @param reducers `List` of reducers that are combined together.
 *
 * @note This need not be called in an application. The
 *       [[ReducerableMultipleWithValue]] type class will convert any
 *       `List[`[[Reducer]]`]` automatically to one of these.
 */
case class CombinationReducerMultipleWithValue[T <: Reducer with PrepareWithValue with PresentMultiple, W](reducers: List[T])
  extends Reducer with PrepareWithValue with PresentMultiple {
  type T = List[Any]
  type V = W

  def prepare[P <: Position, D <: Dimension](slc: Slice[P, D], pos: P, con: Content, ext: V): T = {
    reducers.map { case reducer => reducer.prepare(slc, pos, con, ext.asInstanceOf[reducer.V]) }
  }

  def reduce(lt: T, rt: T): T = {
    (reducers, lt, rt).zipped.map {
      case (reducer, l, r) => reducer.reduce(l.asInstanceOf[reducer.T], r.asInstanceOf[reducer.T])
    }
  }

  def presentMultiple[P <: Position with ExpandablePosition](pos: P,
    t: T): Option[Either[(P#M, Content), List[(P#M, Content)]]] = {
    Some(Right((reducers, t).zipped.flatMap {
      case (reducer, s) => Miscellaneous.mapFlatten(reducer.presentMultiple(pos, s.asInstanceOf[reducer.T]))
    }))
  }
}

/** Type class for transforming a type `T` to a [[Reducer]] with [[PresentMultiple]]. */
trait ReducerableMultiple[T] {
  /**
   * Returns a [[Reducer]] with [[PresentMultiple]] for type `T`.
   *
   * @param t Object that can be converted to a [[Reducer]] with [[PresentMultiple]].
   */
  def convert(t: T): Reducer with Prepare with PresentMultiple
}

/** Companion object for the [[ReducerableMultiple]] type class. */
object ReducerableMultiple {
  /**
   * Converts a `List[`[[Reducer]] with [[PresentMultiple]]`]` to a single [[Reducer]] with [[PresentMultiple]]
   * using [[CombinationReducerMultiple]].
   */
  implicit def ReducerListReducerableMultiple[T <: Reducer with Prepare with PresentMultiple]: ReducerableMultiple[List[T]] =
    new ReducerableMultiple[List[T]] {
      def convert(t: List[T]): Reducer with Prepare with PresentMultiple = CombinationReducerMultiple(t)
    }
  /**
   * Converts a [[Reducer]] with [[PresentMultiple]] to a [[Reducer]] with [[PresentMultiple]]; that is, it is a pass through.
   */
  implicit def ReducerReducerableMultiple[T <: Reducer with Prepare with PresentMultiple]: ReducerableMultiple[T] =
    new ReducerableMultiple[T] {
      def convert(t: T): Reducer with Prepare with PresentMultiple = t
    }
}

/** Type class for transforming a type `T` to a [[Reducer]] with [[PrepareWithValue]] with [[PresentMultiple]]. */
trait ReducerableMultipleWithValue[T, W] {
  /**
   * Returns a [[Reducer]] with [[PrepareWithValue]] with [[PresentMultiple]] for type `T`.
   *
   * @param t Object that can be converted to a [[Reducer]] with [[PrepareWithValue]] with [[PresentMultiple]].
   */
  def convert(t: T): Reducer with PrepareWithValue with PresentMultiple
}

/** Companion object for the [[ReducerableMultipleWithValue]] type class. */
object ReducerableMultipleWithValue {
  /**
   * Converts a `List[`[[Reducer]] with [[PrepareWithValue]] with [[PresentMultiple]]`]` to a single
   * [[Reducer]] with [[PrepareWithValue]] with [[PresentMultiple]] using [[CombinationReducerMultipleWithValue]].
   */
  implicit def ReducerListReducerableMultipleWithValue[T <: Reducer with PrepareWithValue with PresentMultiple { type V >: W }, W]: ReducerableMultipleWithValue[List[T], W] =
    new ReducerableMultipleWithValue[List[T], W] {
      def convert(t: List[T]): Reducer with PrepareWithValue with PresentMultiple = CombinationReducerMultipleWithValue[Reducer with PrepareWithValue with PresentMultiple, W](t)
    }
  /**
   * Converts a [[Reducer]] with [[PrepareWithValue]] with [[PresentMultiple]] to a [[Reducer]] with
   * [[PrepareWithValue]] with [[PresentMultiple]]; that is, it is a pass through.
   */
  implicit def ReducerReducerableMultipleWithValue[T <: Reducer with PrepareWithValue with PresentMultiple { type V >: W }, W]: ReducerableMultipleWithValue[T, W] =
    new ReducerableMultipleWithValue[T, W] {
      def convert(t: T): Reducer with PrepareWithValue with PresentMultiple = t
    }
}

