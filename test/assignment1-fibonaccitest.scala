package prg1.assignment1

import org.scalatest._
import FIB._

class FIBTest extends FunSuite with Matchers {
   test("FIB tests") {
     fib_itr(20) should equal (fib_rec(20))
     fib_matrix(20) should equal (fib_itr(20))
   }

}



