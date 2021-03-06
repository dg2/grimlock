// Copyright 2015 Commonwealth Bank of Australia
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

package au.com.cba.omnia.grimlock

import au.com.cba.omnia.grimlock.content._
import au.com.cba.omnia.grimlock.content.metadata._
import au.com.cba.omnia.grimlock.encoding._
import au.com.cba.omnia.grimlock.position._
import au.com.cba.omnia.grimlock.squash._

import org.scalatest._

trait TestSquashers {

  val dfmt = new java.text.SimpleDateFormat("yyyy-MM-dd")
  val con = Content(ContinuousSchema[Codex.LongCodex](), 123)
  val cell1 = Cell(Position3D(1, "b", DateValue(dfmt.parse("2001-01-01"), DateCodex)), con)
  val cell2 = Cell(Position3D(2, "a", DateValue(dfmt.parse("2002-01-01"), DateCodex)), con)
}

class TestPreservingMaxPosition extends FlatSpec with Matchers with TestSquashers {

  "A PreservingMaxPosition" should "return the second cell for the first dimension when greater" in {
    PreservingMaxPosition().reduce(First, cell1, cell2) should be (cell2)
  }

  it should "return the first cell for the first dimension when greater" in {
    PreservingMaxPosition().reduce(First, cell2, cell1) should be (cell2)
  }

  it should "return the first cell for the first dimension when equal" in {
    PreservingMaxPosition().reduce(First, cell2, cell2) should be (cell2)
  }

  it should "return the first cell for the second dimension when greater" in {
    PreservingMaxPosition().reduce(Second, cell1, cell2) should be (cell1)
  }

  it should "return the second cell for the second dimension when greater" in {
    PreservingMaxPosition().reduce(Second, cell2, cell1) should be (cell1)
  }

  it should "return the first cell for the second dimension when equal" in {
    PreservingMaxPosition().reduce(Second, cell1, cell1) should be (cell1)
  }

  it should "return the second cell for the third dimension when greater" in {
    PreservingMaxPosition().reduce(Third, cell1, cell2) should be (cell2)
  }

  it should "return the first cell for the third dimension when greater" in {
    PreservingMaxPosition().reduce(Third, cell2, cell1) should be (cell2)
  }

  it should "return the first cell for the third dimension when equal" in {
    PreservingMaxPosition().reduce(Third, cell2, cell2) should be (cell2)
  }
}

class TestPreservingMinPosition extends FlatSpec with Matchers with TestSquashers {

  "A PreservingMinPosition" should "return the first cell for the first dimension when less" in {
    PreservingMinPosition().reduce(First, cell1, cell2) should be (cell1)
  }

  it should "return the second cell for the first dimension when less" in {
    PreservingMinPosition().reduce(First, cell2, cell1) should be (cell1)
  }

  it should "return the first cell for the first dimension when equal" in {
    PreservingMinPosition().reduce(First, cell1, cell1) should be (cell1)
  }

  it should "return the second cell for the second dimension when less" in {
    PreservingMinPosition().reduce(Second, cell1, cell2) should be (cell2)
  }

  it should "return the first cell for the second dimension when less" in {
    PreservingMinPosition().reduce(Second, cell2, cell1) should be (cell2)
  }

  it should "return the first cell for the second dimension when equal" in {
    PreservingMinPosition().reduce(Second, cell2, cell2) should be (cell2)
  }

  it should "return the first cell for the third dimension when less" in {
    PreservingMinPosition().reduce(Third, cell1, cell2) should be (cell1)
  }

  it should "return the second cell for the third dimension when less" in {
    PreservingMinPosition().reduce(Third, cell2, cell1) should be (cell1)
  }

  it should "return the first cell for the third dimension when equal" in {
    PreservingMinPosition().reduce(Third, cell1, cell1) should be (cell1)
  }
}

