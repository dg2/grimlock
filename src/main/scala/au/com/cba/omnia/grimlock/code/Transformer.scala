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

package grimlock.transform

import grimlock.contents._
import grimlock.Matrix._
import grimlock.position._
import grimlock.utilities._

// TODO: Add the ability to compose individual transformers. For example,
//       first clamp a variable, then normalise. Perhaps use an andThen
//       construct. See Clamp() for an example.

/** Base trait for transformations. */
trait Transformer

trait ExternalValue {
  /** Type of the external value. */
  type V
}

/** Base trait for transformers that do not modify the number of dimensions. */
trait Present { self: Transformer =>
  /**
   * Present the transformed [[contents.Content]](s).
   *
   * @param pos The [[position.Position]] of the content.
   * @param con The [[contents.Content]] to transform.
   *
   * @return Optional of either a `(`[[position.Position.S]]`,
   *         `[[contents.Content]]`)` or a `List` of these tuples where the
   *         [[position.Position]] is creating by modifiying `pos` and the
   *         [[contents.Content]] is derived from `con`.
   *
   * @note An `Option` is used in the return type to allow reducers to be
   *       selective in what [[contents.Content]] they apply to. For example,
   *       normalising is undefined for categorical variables. The transformer
   *       now has the option to return `None`. This in turn permits an
   *       external API, for simple cases, where the user need not know about
   *       the types of variables of their data.
   * @note An `Either` is used to all either one-to-one or one-to-many
   *       transformations.
   *
   * @see [[transform.Transformable]]
   */
  def present[P <: Position with ModifyablePosition](pos: P,
    con: Content): Option[Either[(P#S, Content), List[(P#S, Content)]]]
}

/**
 * Base trait for transformers that use a user supplied value but do not
 * modify the number of dimensions.
 */
trait PresentWithValue extends ExternalValue { self: Transformer =>
  /**
   * Present the transformed [[contents.Content]](s).
   *
   * @param pos The [[position.Position]] of the content.
   * @param con The [[contents.Content]] to transform.
   * @param ext The external value.
   *
   * @return Optional of either a `(`[[position.Position.S]]`,
   *         `[[contents.Content]]`)` or a `List` of these tuples where the
   *         [[position.Position]] is creating by modifiying `pos` and the
   *         [[contents.Content]] is derived from `con`.
   *
   * @note An `Option` is used in the return type to allow reducers to be
   *       selective in what [[contents.Content]] they apply to. For example,
   *       normalising is undefined for categorical variables. The transformer
   *       now has the option to return `None`. This in turn permits an
   *       external API, for simple cases, where the user need not know about
   *       the types of variables of their data.
   * @note An `Either` is used to all either one-to-one or one-to-many
   *       transformations.
   *
   * @see [[transform.TransformableWithValue]]
   */
  def present[P <: Position with ModifyablePosition](pos: P, con: Content,
    ext: V): Option[Either[(P#S, Content), List[(P#S, Content)]]]
}

/**
 * Base trait for transformers that expand the [[position.Position]] by
 * appending a dimension.
 */
trait PresentExpanded { self: Transformer =>
  /**
   * Present the transformed [[contents.Content]](s).
   *
   * @param pos The [[position.Position]] of the content.
   * @param con The [[contents.Content]] to transform.
   *
   * @return Optional of either a `(`[[position.ExpandablePosition.M]]`,
   *         `[[contents.Content]]`)` or a `List` of these tuples where the
   *         [[position.Position]] is creating by appending to `pos` and the
   *         [[contents.Content]] is derived from `con`.
   *
   * @note An `Option` is used in the return type to allow reducers to be
   *       selective in what [[contents.Content]] they apply to. For example,
   *       normalising is undefined for categorical variables. The transformer
   *       now has the option to return `None`. This in turn permits an
   *       external API, for simple cases, where the user need not know about
   *       the types of variables of their data.
   * @note An `Either` is used to all either one-to-one or one-to-many
   *       transformations.
   *
   * @see [[position.ExpandablePosition]], [[transform.TransformableExpanded]]
   */
  def present[P <: Position with ExpandablePosition](pos: P,
    con: Content): Option[Either[(P#M, Content), List[(P#M, Content)]]]
}

/**
 * Base trait for transformers that use a user supplied value and expand the
 * [[position.Position]] by appending a dimension.
 */
trait PresentExpandedWithValue extends ExternalValue { self: Transformer =>
  /**
   * Present the transformed [[contents.Content]](s).
   *
   * @param pos The [[position.Position]] of the content.
   * @param con The [[contents.Content]] to transform.
   * @param ext The external value.
   *
   * @return Optional of either a `(`[[position.ExpandablePosition.M]]`,
   *         `[[contents.Content]]`)` or a `List` of these tuples where the
   *         [[position.Position]] is creating by appending to `pos` and the
   *         [[contents.Content]] is derived from `con`.
   *
   * @note An `Option` is used in the return type to allow reducers to be
   *       selective in what [[contents.Content]] they apply to. For example,
   *       normalising is undefined for categorical variables. The transformer
   *       now has the option to return `None`. This in turn permits an
   *       external API, for simple cases, where the user need not know about
   *       the types of variables of their data.
   * @note An `Either` is used to all either one-to-one or one-to-many
   *       transformations.
   *
   * @see [[position.ExpandablePosition]],
   *      [[transform.TransformableExpandedWithValue]]
   */
  def present[P <: Position with ExpandablePosition](pos: P, con: Content,
    ext: V): Option[Either[(P#M, Content), List[(P#M, Content)]]]
}

/**
 * Transformer that is a combination of one or more [[Transformer]] with
 * [[Present]].
 *
 * @param singles `List` of transformers that are combined together.
 *
 * @note This need not be called in an application. The [[Transformable]]
 *       type class will convert any `List[`[[Transformer]]`]`
 *       automatically to one of these.
 *
 * @see [[Transformable]]
 */
case class CombinationTransformer[T <: Transformer with Present](singles: List[T]) extends Transformer with Present {
  def present[P <: Position with ModifyablePosition](pos: P, con: Content) = {
    Some(Right(singles.flatMap {
      case s => Miscellaneous.mapFlatten(s.present(pos, con))
    }))
  }
}

/**
 * Transformer that is a combination of one or more [[Transformer]] with
 * [[PresentWithValue]].
 *
 * @param singles `List` of transformers that are combined together.
 *
 * @note This need not be called in an application. The
 *       [[TransformableWithValue]] type class will convert any
 *       `List[`[[Transformer]]`]` automatically to one of these.
 *
 * @see [[TransformableWithValue]]
 */
case class CombinationTransformerWithValue[T <: Transformer with PresentWithValue, W](singles: List[T]) extends Transformer with PresentWithValue {
  type V = W
  def present[P <: Position with ModifyablePosition](pos: P, con: Content,
    ext: V) = {
    Some(Right(singles.flatMap {
      case s => Miscellaneous.mapFlatten(s.present(pos, con,
        ext.asInstanceOf[s.V]))
    }))
  }
}

/**
 * Transformer that is a combination of one or more [[Transformer]] with
 * [[PresentExpanded]].
 *
 * @param singles `List` of transformers that are combined together.
 *
 * @note This need not be called in an application. The
 *       [[TransformableExpanded]] type class will convert any
 *       `List[`[[Transformer]]`]` automatically to one of these.
 *
 * @see [[TransformableExpanded]]
 */
case class CombinationTransformerExpanded[T <: Transformer with PresentExpanded](singles: List[T]) extends Transformer with PresentExpanded {
  def present[P <: Position with ExpandablePosition](pos: P, con: Content) = {
    Some(Right(singles.flatMap {
      case s => Miscellaneous.mapFlatten(s.present(pos, con))
    }))
  }
}

/**
 * Transformer that is a combination of one or more [[Transformer]] with
 * [[PresentExpandedWithValue]].
 *
 * @param singles `List` of transformers that are combined together.
 *
 * @note This need not be called in an application. The
 *       [[TransformableExpandedWithValue]] type class will convert any
 *       `List[`[[Transformer]]`]` automatically to one of these.
 *
 * @see [[TransformableExpandedWithValue]]
 */
case class CombinationTransformerExpandedWithValue[T <: Transformer with PresentExpandedWithValue, W](singles: List[T]) extends Transformer with PresentExpandedWithValue {
  type V = W
  def present[P <: Position with ExpandablePosition](pos: P, con: Content,
    ext: W) = {
    Some(Right(singles.flatMap {
      case s => Miscellaneous.mapFlatten(s.present(pos, con,
        ext.asInstanceOf[s.V]))
    }))
  }
}

/**
 * Type class for transforming a type `T` to a [[Transformer]] with [[Present]].
 */
trait Transformable[T] {
  /**
   * Returns a [[Transformer]] with [[Present]] for type `T`.
   *
   * @param t Object that can be converted to a [[Transformer]] with
   *          [[Present]].
   */
  def convert(t: T): Transformer with Present
}

/** Companion object for the [[Transformable]] type class. */
object Transformable {
  /**
   * Converts a `List[`[[Transformer]] with [[Present]]`]` to a single
   * [[Transformer]] with [[Present]] using [[CombinationTransformer]].
   */
  implicit def TransformerListTransformable[T <: Transformer with Present]: Transformable[List[T]] = {
    new Transformable[List[T]] {
      def convert(t: List[T]): Transformer with Present = {
        CombinationTransformer(t)
      }
    }
  }
  /**
   * Converts a [[Transformer]] with [[Present]] to a [[Transformer]] with
   * [[Present]]; that is, * it is a pass through.
   */
  implicit def TransformerTransformable[T <: Transformer with Present]: Transformable[T] = {
    new Transformable[T] {
      def convert(t: T): Transformer with Present = t
    }
  }
}

/**
 * Type class for transforming a type `T` to a [[Transformer]] with
 * [[PresentWithValue]].
 */
trait TransformableWithValue[T, W] {
  /**
   * Returns a [[Transformer]] with [[PresentWithValue]] for type `T`.
   *
   * @param t Object that can be converted to a [[Transformer]] with
   *          [[PresentWithValue]].
   */
  def convert(t: T): Transformer with PresentWithValue
}

/** Companion object for the [[TransformableWithValue]] type class. */
object TransformableWithValue {
  /**
   * Converts a `List[`[[Transformer]] with [[PresentWithValue]]`]` to a
   * single [[Transformer]] with [[PresentWithValue]] using
   * [[CombinationTransformerWithValue]].
   */
  implicit def TransformerListTransformableWithValue[T <: Transformer with PresentWithValue { type V >: W }, W]: TransformableWithValue[List[T], W] = {
    new TransformableWithValue[List[T], W] {
      def convert(t: List[T]): Transformer with PresentWithValue = {
        CombinationTransformerWithValue[Transformer with PresentWithValue, W](t)
      }
    }
  }
  /**
   * Converts a [[Transformer]] with [[PresentWithValue]] to a [[Transformer]]
   * with [[PresentWithValue]]; that is, * it is a pass through.
   */
  implicit def TransformerTransformableWithValue[T <: Transformer with PresentWithValue { type V >: W }, W]: TransformableWithValue[T, W] = {
    new TransformableWithValue[T, W] {
      def convert(t: T): Transformer with PresentWithValue = t
    }
  }
}

/**
 * Type class for transforming a type `T` to a [[Transformer]] with
 * [[PresentExpanded]].
 */
trait TransformableExpanded[T] {
  /**
   * Returns a [[Transformer]] with [[PresentExpanded]] for type `T`.
   *
   * @param t Object that can be converted to a [[Transformer]] with
   *          [[PresentExpanded]].
   */
  def convert(t: T): Transformer with PresentExpanded
}

/** Companion object for the [[TransformableExpanded]] type class. */
object TransformableExpanded {
  /**
   * Converts a `List[`[[Transformer]] with [[PresentExpanded]]`]` to a single
   * [[Transformer]] with [[PresentExpanded]] using
   * [[CombinationTransformerExpanded]].
   */
  implicit def TransformerListTransformableExpanded[T <: Transformer with PresentExpanded]: TransformableExpanded[List[T]] = {
    new TransformableExpanded[List[T]] {
      def convert(t: List[T]): Transformer with PresentExpanded = {
        CombinationTransformerExpanded(t)
      }
    }
  }
  /**
   * Converts a [[Transformer]] with [[PresentExpanded]] to a [[Transformer]]
   * with [[PresentExpanded]]; that is, * it is a pass through.
   */
  implicit def TransformerTransformableExpanded[T <: Transformer with PresentExpanded]: TransformableExpanded[T] = {
    new TransformableExpanded[T] {
      def convert(t: T): Transformer with PresentExpanded = t
    }
  }
}

/**
 * Type class for transforming a type `T` to a [[Transformer]] with
 * [[PresentExpandedWithValue]].
 */
trait TransformableExpandedWithValue[T, W] {
  /**
   * Returns a [[Transformer]] with [[PresentExpandedWithValue]] for type `T`.
   *
   * @param t Object that can be converted to a [[Transformer]] with
   *          [[PresentExpandedWithValue]].
   */
  def convert(t: T): Transformer with PresentExpandedWithValue
}

/** Companion object for the [[TransformableExpandedWithValue]] type class. */
object TransformableExpandedWithValue {
  /**
   * Converts a `List[`[[Transformer]] with [[PresentExpandedWithValue]]`]` to
   * a single [[Transformer]] with [[PresentExpandedWithValue]] using
   * [[CombinationTransformerExpandedWithValue]].
   */
  implicit def TransformerListTransformableExpandedWithValue[T <: Transformer with PresentExpandedWithValue { type V >: W }, W]: TransformableExpandedWithValue[List[T], W] = {
    new TransformableExpandedWithValue[List[T], W] {
      def convert(t: List[T]): Transformer with PresentExpandedWithValue = {
        CombinationTransformerExpandedWithValue[Transformer with PresentExpandedWithValue, W](t)
      }
    }
  }
  /**
   * Converts a [[Transformer]] with [[PresentExpandedWithValue]] to a
   * [[Transformer]] with [[PresentExpandedWithValue]]; that is, it is a
   * pass through.
   */
  implicit def TransformderTransformableExpandedWithValue[T <: Transformer with PresentExpandedWithValue { type V >: W }, W]: TransformableExpandedWithValue[T, W] = {
    new TransformableExpandedWithValue[T, W] {
      def convert(t: T): Transformer with PresentExpandedWithValue = t
    }
  }
}

