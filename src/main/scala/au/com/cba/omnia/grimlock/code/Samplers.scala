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

package grimlock.sample

import scala.util._

import grimlock.Matrix._
import grimlock.position._

/** Implements default samplers. */
object Samplers {
  /**
   * Randomly sample.
   *
   * @param ratio The sampling ratio.
   * @param rnd   The random number generator.
   *
   * @return A sampling function for use in [[Matrix.sample]]
   *
   * @note This randomly samples ignoring the [[position.Position]].
   */
  def randomSample[P <: Position](ratio: Double,
    rnd: Random = new Random()) = (pos: P) => {
    rnd.nextDouble() < ratio
  }

  /**
   * Sample based on the hash code of a dimension.
   *
   * @param dim   The dimension to sample from.
   * @param ratio The sample ratio (relative to `base`).
   * @param base  The base of the sampling ratio.
   *
   * @return A sampling function for use in [[Matrix.sample]]
   */
  def hashSample[P <: Position](dim: Dimension, ratio: Int,
    base: Int) = (pos: P) => {
    math.abs(pos.get(dim).hashCode % base) < ratio
  }

  /**
   * Sample based on the hash code of a dimension.
   *
   * @param dim   The dimension to sample from.
   * @param size  The size to sample to.
   * @param from  The index of the dimension in a [[Matrix.SliceMap]].
   * @param state The name of the matrix size in a [[Matrix.SliceMap]].
   *
   * @return A sampling function for use in [[Matrix.sampleWithValue]]
   */
  def hashSampleToSize[P <: Position](dim: Dimension, size: Long, from: Int,
    state: String = "size") = (pos: P, sm: SliceMap) => {
    sm(Position1D(from.toString))(Position1D(state)).value.asDouble match {
      case Some(s) =>
        math.abs(pos.get(dim).hashCode % math.round(s / size)) == 0
      case _ => false
    }
  }
}

