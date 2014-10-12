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

package au.com.cba.omnia.grimlock.sample

import au.com.cba.omnia.grimlock.contents._
import au.com.cba.omnia.grimlock.position._

import scala.util.Random

/**
 * Randomly sample.
 *
 * @param ratio The sampling ratio.
 * @param rnd   The random number generator.
 *
 * @note This randomly samples ignoring the [[position.Position]].
 */
case class RandomSample(ratio: Double, rnd: Random = new Random())
  extends Sampler with SelectAndWithValue {
  def select[P <: Position](pos: P): Boolean = rnd.nextDouble() < ratio
}

/**
 * Sample based on the hash code of a dimension.
 *
 * @param dim   The dimension to sample from.
 * @param ratio The sample ratio (relative to `base`).
 * @param base  The base of the sampling ratio.
 */
case class HashSample(dim: Dimension, ratio: Int, base: Int) extends Sampler
  with SelectAndWithValue {
  def select[P <: Position](pos: P): Boolean = {
    math.abs(pos.get(dim).hashCode % base) < ratio
  }
}

/**
 * Sample based on the hash code of a dimension.
 *
 * @param dim  The dimension to sample from.
 * @param size The size to sample to.
 */
case class HashSampleToSize(dim: Dimension, size: Long) extends Sampler
  with SelectWithValue {
  type V = Map[Position1D, Content]

  def select[P <: Position](pos: P, ext: V): Boolean = {
    ext(Position1D(dim.toString)).value.asDouble match {
      case Some(s) =>
        math.abs(pos.get(dim).hashCode % math.round(s / size)) == 0
      case _ => false
    }
  }
}

