package prg1.assignment1

import org.scalatest._
import FIB._

class FIBTest extends FunSuite with Matchers {
   test("FIB tests") {
     fib_itr(0) should equal (fib_rec(0))
     fib_itr(2) should equal (fib_rec(2))
     fib_itr(5) should equal (fib_rec(5))
     fib_itr(8) should equal (fib_rec(8))
     fib_itr(20) should equal (fib_rec(20))
     fib_matrix(0)should equal (fib_itr(0))
     fib_matrix(3)should equal (fib_itr(3))
     fib_matrix(20)should equal (fib_itr(20))
     fib_matrix(200)should equal (fib_itr(200))
     fib_matrix(2000)should equal (fib_itr(2000))
     fib_matrix(20000)should equal (fib_itr(20000))
     fib_polynomial(0)should equal (fib_matrix(0))
     fib_polynomial(2)should equal (fib_matrix(2))
     fib_polynomial(20)should equal (fib_matrix(20))
     fib_polynomial(2000)should equal (fib_matrix(2000))
     fib_polynomial(200000)should equal (fib_matrix(200000))
     fib_polynomial(2000000)should equal (fib_matrix(2000000))
   }

}



