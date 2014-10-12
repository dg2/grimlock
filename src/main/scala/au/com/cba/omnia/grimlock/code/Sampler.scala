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

import au.com.cba.omnia.grimlock.position._

/** Base trait for sampling. */
trait Sampler

/** Base trait for selecting samples. */
trait Select { self: Sampler =>
  /**
   * Indicate if the cell is selected as part of the sample.
   *
   * @param pos The [[position.Position]] of the content.
   */
  def select[P <: Position](pos: P): Boolean
}

/** Base trait for selecting samples with a user provided value. */
trait SelectWithValue { self: Sampler =>
  /** Type of the external value. */
  type V

  /**
   * Indicate if the cell is selected as part of the sample.
   *
   * @param pos The [[position.Position]] of the content.
   * @param ext The user define the value.
   */
  def select[P <: Position](pos: P, ext: V): Boolean
}

/**
 * Convenience trait for [[Sampler]]s that selects with or without using a
 * user supplied value.
 */
trait SelectAndWithValue extends Select with SelectWithValue { self: Sampler =>
  type V = Any

  def select[P <: Position](pos: P, ext: V) = select(pos)
}

