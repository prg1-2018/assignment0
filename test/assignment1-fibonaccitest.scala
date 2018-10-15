package prg1.assignment1

import org.scalatest._
import FIB._

class FIBTest extends FunSuite with Matchers {
   test("FIB tests itr") {
   	 fib_itr(0) should equal (fib_rec(0))
     fib_itr(2) should equal (fib_rec(2))
     fib_itr(10) should equal (fib_rec(10))
     fib_itr(20) should equal (fib_rec(20))
     fib_itr(40) should equal (fib_rec(40))
   }
   
   test("FIB tests matrix"){
   	 fib_matrix(0) should equal (fib_itr(0))
   	 fib_matrix(2) should equal (fib_itr(2))
   	 fib_matrix(10) should equal (fib_itr(10))
   	 fib_matrix(21) should equal (fib_itr(21))
   	 fib_matrix(30) should equal (fib_itr(30))
   	 fib_matrix(41) should equal (fib_itr(41))
   	 fib_matrix(50) should equal (fib_itr(50))
   	 fib_matrix(61) should equal (fib_itr(61))
   	 fib_matrix(70) should equal (fib_itr(70))
   	 fib_matrix(85) should equal (fib_itr(85))
   	 fib_matrix(100) should equal (fib_itr(100))
   }

}



