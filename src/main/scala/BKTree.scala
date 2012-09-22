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
* Date: 9/22/12
* Time: 4:31 PM
*/

/*

Implementation of BK-Tree in Scala

http://en.wikipedia.org/wiki/BK-tree
http://blog.notdot.net/2007/4/Damn-Cool-Algorithms-Part-1-BK-Trees
http://hamberg.no/erlend/posts/2012-01-17-BK-trees.html

 */

import collection.mutable
import collection.mutable.ListBuffer


class BKTree(dictionary: Seq[String], dist: (String, String) => Int = Levenshtein.distance) {
  val root = new BKNode(dictionary.head)

  dictionary.tail.foreach(s => addNode(root, new BKNode(s)))

  private def addNode(dst: BKNode, src: BKNode) {
    if (!src.equals(dst)) {
      val d = dist(src.name, dst.name)
      dst.childAt(d) match {
        case Some(child) => addNode(child, src)
        case _ => dst.addChild(d, src)
      }
    }
  }

  def search(s: String, maxDistance: Int) = root.search(s, maxDistance)

  final class BKNode(val name: String) {
    val children = new mutable.HashMap[Int, BKNode]

    def childAt(d: Int) = children.get(d)

    def addChild(d: Int, child: BKNode) {
      children.put(d, child)
    }

    def search(s: String, maxDistance: Int): Seq[String] = {
      val d = dist(s, name)
      val matches = new ListBuffer[String]
      if (d <= maxDistance)
        matches.append(name)
      if (children.nonEmpty) {
        for (i <- List.range(1.max(d - maxDistance), d + maxDistance)) {
          children.get(i) match {
            case Some(child) => matches.appendAll(child.search(s, maxDistance))
            case _ =>
          }
        }
      }
      matches
    }
  }

}

object Levenshtein {

  def distance(s1: String, s2: String): Int = {
    val len1 = s1.length
    val len2 = s2.length

    def min(nums: Int*) = nums.min

    val d: Array[Array[Int]] = Array.ofDim(len1 + 1, len2 + 1)

    for (i <- 0 to len1) d(i)(0) = i
    for (j <- 0 to len2) d(0)(j) = j

    for (i <- 1 to len1; j <- 1 to len2) {
      val cost = if (s1(i - 1) == s2(j - 1)) 0 else 1

      d(i)(j) = min(
        d(i - 1)(j) + 1, // deletion
        d(i)(j - 1) + 1, // insertion
        d(i - 1)(j - 1) + cost // substitution
      )
    }

    d(len1)(len2)
  }

}