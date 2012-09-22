/*
* Copyright 2011 P.Budzik
*
* Licensed under the Apache License, Version 2.0 (the "License");
* you may not use this file except in compliance with the License.
* You may obtain a copy of the License at
*
*     http://www.apache.org/licenses/LICENSE-2.0
*
* Unless required by applicable law or agreed to in writing, software
* distributed under the License is distributed on an "AS IS" BASIS,
* WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
* See the License for the specific language governing permissions and
* limitations under the License.
*
* User: przemek
* Date: 7/2/11
* Time: 12:07 PM
*/

import collection.mutable.ListBuffer
import org.scalatest.matchers.ShouldMatchers
import org.scalatest.{BeforeAndAfterEach, FunSuite}


class BKTreeSuite extends FunSuite with ShouldMatchers with BeforeAndAfterEach {

  test("searching") {
    val tree = new BKTree(Seq("loop", "book", "look", "przemek"))

    tree.search("foop", 0) should be(ListBuffer())
    tree.search("foop", 1) should be(ListBuffer("loop"))
    tree.search("book", 2) should be(ListBuffer("loop", "look", "book"))
    tree.search("prezmek", 2) should be(ListBuffer("przemek"))
  }


}