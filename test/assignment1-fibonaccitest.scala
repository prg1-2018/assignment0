package prg1.assignment1

import org.scalatest._
import FIB._

class FIBTest extends FunSuite with Matchers {
   test("FIB tests") {
     fib_itr(2) should equal (fib_rec(2))
     fib_itr(10) should equal (fib_rec(10))
     fib_itr(15) should equal (fib_rec(15))
     fib_itr(1000) should equal (fib_rec(20))
     fib_matrix(2) should equal (fib_rec(2))
     fib_matrix(10) should equal (fib_rec(10))
     fib_matrix(15) should equal (fib_rec(15))
     fib_matrix(100) should equal (fib_rec(20))
   }

}



