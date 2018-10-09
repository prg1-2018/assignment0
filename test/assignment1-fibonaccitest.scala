package prg1.assignment1

import org.scalatest._
import FIB._

class FIBTest extends FunSuite with Matchers {
   test("FIB tests") {
     fib_itr(10) should equal (fib_rec(10))
     fib_matrix(100) should equal (fib_itr(100))
   }

}
