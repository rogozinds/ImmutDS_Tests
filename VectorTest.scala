package common
import org.scalameter.api._
import scala.collection.immutable.Vector
import scala.util
object VectorTest extends PerformanceTest.Quickbenchmark {
  val sizes: Gen[Int] = Gen.range("size")(10000, 100000, 10000)
  val gen = new util.Random()
  def createVector(k: Vector[Int], n: Int): Vector[Int] = {
    if (n == 0) k
    else createVector(n +: k, n - 1)
  }
  val vectors: Gen[Vector[Int]] = {
    for {
      size <- sizes
    } yield (createVector(Vector[Int](), size))
  }

  //Creating vectors of different sizes
  performance of "Vector" in {
    measure method "create" in {
      using(sizes) in {
        r => createVector(Vector(), r)
      }
    }
  }
  //Take a value from vector by index
  performance of "Vector" in {
    measure method "apply" in {
      using(vectors) in {
        r =>
          {
            for (tmp <- 1 to 1000) yield {
              val index = gen.nextInt(r.size)
              r.apply(index)
            }
          }
      }
    }
  }

  //Updating vectors
  performance of "Vector" in {
    measure method "update" in {
      using(vectors) in {
        r =>
          {
            for (tmp <- 1 to 1000) yield {
              val index = gen.nextInt(r.size)
              r.updated(index, 1)
            }
          }
      }
    }
  }
}