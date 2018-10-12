package prg1.assignment1

import org.scalatest._
import FIB._

class FIBTest extends FunSuite with Matchers {
   test("FIB tests") {
     fib_itr(2) should equal (fib_rec(2))
     fib_itr(6) should equal (fib_rec(6))
     fib_itr(11) should equal (fib_rec(11))
     fib_itr(30) should equal (fib_rec(30))
   }
   test("FIB tests2") {
     fib_matrix(2) should equal (fib_itr(2))
     fib_matrix(12) should equal (fib_itr(12))
     fib_matrix(52) should equal (fib_itr(52))
     fib_matrix(102) should equal (fib_itr(102))
     fib_matrix(1003) should equal (fib_itr(1003))
   }
   test("FIB tests3") {
     fib_polynomial(2) should equal (fib_matrix(2))
     fib_polynomial(55) should equal (fib_matrix(55))
     fib_polynomial(304) should equal (fib_matrix(304))
     fib_polynomial(1403) should equal (fib_matrix(1403))
     fib_polynomial(25000) should equal (fib_matrix(25000))
   }
}
