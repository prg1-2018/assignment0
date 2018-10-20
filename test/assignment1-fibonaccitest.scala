package prg1.assignment1

import org.scalatest._
import FIB._

class FIBTest extends FunSuite with Matchers {
   test("FIB tests") {
     fib_itr(0) should equal (fib_rec(0))
     fib_matrix(0) should equal (fib_itr(0))
     fib_itr(20) should equal (fib_rec(20))
     fib_matrix(20) should equal (fib_itr(20))
     fib_matrix(200) should equal (fib_itr(200))
     fib_matrix(2000) should equal (fib_itr(2000))
   }

}



