package common
import org.scalameter.api._

object ListTest extends PerformanceTest.Quickbenchmark {
  val sizes: Gen[Int] = Gen.range("size")(100000, 1000000, 100000)
  
  val ranges: Gen[(Int, List[Int])] = for {
    size <- sizes
  } yield ((size, new Array[Int](size).toList))
  
    performance of "List" in {
    measure method "head" in {
      using(ranges) in {
        r => r._2.head
      }
    }
  } 

  performance of "List" in {
    measure method "last" in {
      using(ranges) in {
        r => r._2.last
      }
    }
  }
  
  performance of "List" in {
    measure method "updated" in {
      using(ranges) in {
        r => r._2.updated((r._1-1),1)
      }
    }
  }
}