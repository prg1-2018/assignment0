package prg1.assignment1

import org.scalatest._
import FIB._

class FIBTest extends FunSuite with Matchers {
   test("FIB tests") {
     fib_itr(2) should equal (fib_rec(2))
     fib_matrix(2) should equal (fib_rec(2))
     fib_polynomial(2) should equal (fib_rec(2))

     fib_matrix(100) should equal (fib_itr(100))
     fib_polynomial(100) should equal (fib_itr(100))

     fib_polynomial(1000) should equal (fib_matrix(1000))
   }

}
