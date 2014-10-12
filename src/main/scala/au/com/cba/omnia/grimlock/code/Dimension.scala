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

package au.com.cba.omnia.grimlock.position

/** Base trait for dimensions of a [[Matrix]]. */
sealed trait Dimension {
  /** Index of the dimension (starting at 0). */
  val index: Int
}

/** Type for first dimension. */
case object First extends Dimension { val index = 0 }
/** Type for second dimension. */
case object Second extends Dimension { val index = 1 }
/** Type for third dimension. */
case object Third extends Dimension { val index = 2 }
/** Type for fourth dimension. */
case object Fourth extends Dimension { val index = 3 }
/** Type for fifth dimension. */
case object Fifth extends Dimension { val index = 4 }

/**
 * Trait for capturing functional dependency between a [[Position]] and a
 * [[Dimension]].
 */
trait PosDimDep[A, B]

/**
 * Companion object of [[Dimension]] trait. This defines the (implicit)
 * permitted dependencies between [[Position]] and [[Dimension]].
 */
object Dimension {
  /** List of all available dimensions ordered by index. */
  val All = List(First, Second, Third, Fourth, Fifth)

  /** Define dependency between [[Position1D]] and [[First]]. */
  implicit object P1D1 extends PosDimDep[Position1D, First.type]
  /** Define dependency between [[Position2D]] and [[First]]. */
  implicit object P2D1 extends PosDimDep[Position2D, First.type]
  /** Define dependency between [[Position2D]] and [[Second]]. */
  implicit object P2D2 extends PosDimDep[Position2D, Second.type]
  /** Define dependency between [[Position3D]] and [[First]]. */
  implicit object P3D1 extends PosDimDep[Position3D, First.type]
  /** Define dependency between [[Position3D]] and [[Second]]. */
  implicit object P3D2 extends PosDimDep[Position3D, Second.type]
  /** Define dependency between [[Position3D]] and [[Third]]. */
  implicit object P3D3 extends PosDimDep[Position3D, Third.type]
  /** Define dependency between [[Position4D]] and [[First]]. */
  implicit object P4D1 extends PosDimDep[Position4D, First.type]
  /** Define dependency between [[Position4D]] and [[Second]]. */
  implicit object P4D2 extends PosDimDep[Position4D, Second.type]
  /** Define dependency between [[Position4D]] and [[Third]]. */
  implicit object P4D3 extends PosDimDep[Position4D, Third.type]
  /** Define dependency between [[Position4D]] and [[Fourth]]. */
  implicit object P4D4 extends PosDimDep[Position4D, Fourth.type]
  /** Define dependency between [[Position5D]] and [[First]]. */
  implicit object P5D1 extends PosDimDep[Position5D, First.type]
  /** Define dependency between [[Position5D]] and [[Second]]. */
  implicit object P5D2 extends PosDimDep[Position5D, Second.type]
  /** Define dependency between [[Position5D]] and [[Third]]. */
  implicit object P5D3 extends PosDimDep[Position5D, Third.type]
  /** Define dependency between [[Position5D]] and [[Fourth]]. */
  implicit object P5D4 extends PosDimDep[Position5D, Fourth.type]
  /** Define dependency between [[Position5D]] and [[Fifth]]. */
  implicit object P5D5 extends PosDimDep[Position5D, Fifth.type]
}

