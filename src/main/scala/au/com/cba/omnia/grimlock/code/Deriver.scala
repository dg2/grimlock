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

package au.com.cba.omnia.grimlock.derive

import au.com.cba.omnia.grimlock.contents._
import au.com.cba.omnia.grimlock.position._

/**
 * Base trait for generating derived data.
 *
 * Derived data is derived from two or more values, for example deltas
 * or gradients. To generate this, the process is as follows. First the
 * [[Matrix]] is grouped according to a [[Slice]]. The data in each
 * group is then sorted by the remaining coordinates. The first cell of
 * each group is passed to the `prepare` method. This allows a [[Deriver]]
 * to initialise it's running state. All subsequent cells are passed to
 * the present method (together with the running state). The present
 * method can update the running state, and optionally return one or more
 * cells with derived data. Note that the running state can be used to
 * create derived features of different windows.
 */
trait Deriver {
  /** Type of the state. */
  type T

  /**
   * Prepare the state using the first cell (ordered according to its
   * [[position.Position]]).
   *
   * @param curr The current cell.
   *
   * @return The state for this object.
   */
  // TODO: Add with value version
  def prepare[P <: Position](curr: (P, Content)): T

  /**
   * Update state with the current cell and, optionally, output
   * derived data.
   *
   * @param curr The current cell.
   * @param t    The state.
   *
   * @return A tuple consisting of updated state together with optional
   *         derived data.
   *
   * @note An `Option` is used for the derived data to allow derivers to
   *       be selective in when derived data is returned. An `Either` is
   *       used to allow a deriver to return more than one derived data.
   */
  def present[P <: Position with ModifyablePosition](curr: (P, Content),
    t: T): (T, Option[Either[(P#S, Content), List[(P#S, Content)]]])
}

