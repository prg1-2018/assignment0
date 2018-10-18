package prg1.assignment1

import org.scalatest._
import FIB._

class FIBTest extends FunSuite with Matchers {
   test("FIB tests") {
     fib_itr(2) should equal (fib_rec(2))
   }
   test("FIB tests2") {
     fib_itr(10) should equal (fib_rec(10))
   }
   test("FIB tests3") {
     fib_matrix(2) should equal (fib_rec(2))
   }

}



