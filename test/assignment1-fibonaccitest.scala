package prg1.assignment1

import org.scalatest._
import FIB._

class FIBTest extends FunSuite with Matchers {
   test("FIB tests") {
     fib_itr(0) should equal (fib_rec(0))
     fib_matrix(0) should equal (fib_itr(0))
     fib_polynomial(0) should equal (fib_matrix(0))

     fib_itr(1) should equal (fib_rec(1))
     fib_matrix(1) should equal (fib_itr(1))
     fib_polynomial(1) should equal (fib_matrix(1))

     fib_itr(2) should equal (fib_rec(2))
     fib_matrix(2) should equal (fib_itr(2))
     fib_polynomial(2) should equal (fib_matrix(2))

     fib_itr(10) should equal (fib_rec(10))
     fib_matrix(10) should equal (fib_itr(10))
     fib_polynomial(10) should equal (fib_matrix(10))

     fib_matrix(100) should equal (fib_itr(100))
     fib_polynomial(100) should equal (fib_matrix(100))

     fib_polynomial(1000) should equal (fib_matrix(1000))

     fib_polynomial(100000) should equal (fib_matrix(100000))
   }
}
