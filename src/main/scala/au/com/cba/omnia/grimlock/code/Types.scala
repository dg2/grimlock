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

package grimlock

import com.twitter.scalding._
import com.twitter.scalding.TDsl._, Dsl._
import cascading.flow.FlowDef

import grimlock.contents.variable._
import grimlock.position._

/**
 * Rich wrapper around a `TypedPipe[(`[[position.Position]]`,
 * `[[contents.variable.Type]]`)]`.
 *
 * @param data `TypedPipe[(`[[position.Position]]`,
 *             `[[contents.variable.Type]]`)]`.
 *
 * @note This class represents the [[contents.variable.Type]] along the
 *       dimensions of a [[Matrix]].
 */
class Types[P <: Position](data: TypedPipe[(P, Type)]) {
  /**
   * Persist [[Types]] to disk.
   *
   * @param file        Name of the output file.
   * @param separator   Separator to use between [[position.Position]] and
   *                    [[contents.variable.Type]].
   * @param descriptive Indicates if the output should be descriptive.
   *
   * @return A Scalding `TypedPipe[(P, `[[contents.variable.Type]]`)]` which
   *         is this [[Types]].
   */
  def persist(file: String, separator: String = "|",
    descriptive: Boolean = false)(implicit flow: FlowDef,
      mode: Mode): TypedPipe[(P, Type)] = {
    data
      .map {
        case (p, t) => descriptive match {
          case true => p.toString + separator + t.toString
          case false => p.toShortString(separator) + separator + t.name
        }
      }
      .toPipe('line)
      .write(TextLine(file))

    data
  }
}

object Types {
  /**
   * Conversion from `TypedPipe[(`[[position.Position]]`,
   * `[[contents.variable.Type]]`)]` to a [[Types]].
   */
  implicit def typedPipePositionType[P <: Position](data: TypedPipe[(P, Type)]): Types[P] = {
    new Types(data)
  }
}

