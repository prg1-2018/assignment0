package prg1.assignment1

import org.scalatest._
import FIB._

class FIBTest extends FunSuite with Matchers {
   test("FIB tests") {
     fib_itr(2) should equal (fib_rec(2))
     fib_itr(50) should equal (fib_rec(50))
     fib_rec(100) should equal (fib_matrix(100))
     fib_matrix(1000) should equal(fib_polynomial(1000))
   }

}
