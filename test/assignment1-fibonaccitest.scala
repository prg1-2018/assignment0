package prg1.assignment1

import org.scalatest._
import FIB._

class FIBTest extends FunSuite with Matchers {
  val COUNT = 5000
  test("FIB tests") {
    for (i<- 0 until 20) {
      val itr = fib_itr(i)
      for (e <- fib_rec(i)::fib_matrix(i)::fib_polynomial(i)::Nil) {
        e should equal(itr)
      }
    }
    for (i <- 4 until COUNT) {
      val itr = fib_itr(i * 5)
      for (e <- fib_matrix(i * 5)::fib_polynomial(i * 5)::Nil) {
        e should equal(itr)
      }
    }
  }
}



