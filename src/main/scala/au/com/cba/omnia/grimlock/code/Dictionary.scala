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

package au.com.cba.omnia.grimlock.contents.metadata

object Dictionary {
  /** Placeholder type of a dictionary (map of schema). */
  type Dictionary = Map[String, Schema]

  /**
   * Placeholder for reading in a dictionary.
   *
   * @param file File containing dictionary data.
   *
   * @return A dictionary object.
   */
  def read(file: String, separator: String = "\\|"): Dictionary = {
    (for (line <- io.Source.fromFile(file).getLines()) yield {
      val parts = line.split(separator)
      (parts(0), Schema.fromString(parts(1), parts(2)).get)
    }).toMap
  }
}

