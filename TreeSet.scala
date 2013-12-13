package common
import org.scalameter.api._
import scala.collection.immutable.TreeSet
object TreeTest extends PerformanceTest.Quickbenchmark {
  val expSizes: Gen[Int] = Gen.range("size")(1, 23, 1)
  val gen = new util.Random()
  def createSet(k: TreeSet[Int], n: Int): TreeSet[Int] = {
    if (n == 0) k
    else createSet(k + n, n - 1)
  }
  def power(a: Int, p: Int): Int = {
    if (p == 1 || p == 0) a
    else a * power(a, p - 1)
  }
  val trees: Gen[(Int, TreeSet[Int])] = {
    for (size <- expSizes)
      yield ((size, createSet(new TreeSet[Int](), power(2, size))))
  }
  //1000 times testing if tree contains a random value, for all trees  
  //each size N = 2^p where p= 1 to 23
  performance of "TreeSet" in {
    measure method "contains" in {
      using(trees) in {
        r =>
          for (tmp <- 1 to 1000) yield {
            val rndValue = gen.nextInt(r._1)
            r._2.apply(rndValue)
          }

      }
    }
  }

  //1000 times inserting new value into the tree of 
  //each size N = 2^p where p= 1 to 23
  performance of "TreeSet_1000" in {
    measure method "insert" in {
      using(trees) in {
        r => for (tmp <- 1 to 1000) yield (r._2 + (r._1 + tmp))
      }
    }
  }
  //1000 times deleting random value from the tree of 
  //each size N = 2^p where p= 1 to 23
  performance of "TreeSet_1000" in {
    measure method "delete" in {
      using(trees) in {
        r =>
          {
            for (tmp <- 1 to 1000) yield {
              val rndValue = gen.nextInt(r._1)
              (r._2 - rndValue)
            }
          }
      }
    }
  }
}