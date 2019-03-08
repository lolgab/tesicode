package tesi

import utest._
import PageRank._

object PageRankTest extends TestSuite {
  val tests = Tests {
    "pagerank" - {
      val citanti = Array(
        Array(2),
        Array(0, 2),
        Array(0, 3),
        Array(1, 2)
      )

      val pesiPageRank = Array(
        Array(1.0),
        Array(1.0, 1.0),
        Array(1.0, 1.0),
        Array(1.0, 1.0)
      )

      "pagerank example" - {
        val calculated = pageRank(citanti, pesiPageRank,2, 1.0).toList
        val shouldBe = Array(1.5 / 12, 2.0 / 12, 4.5 / 12, 4.0 / 12).toList
        assert(calculated == shouldBe)
      }
    }

    "pagerank mod" - {
      
    }
  }
}
