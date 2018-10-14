package prg1.assignment1

import org.scalatest._
import FIB._

class FIBTest extends FunSuite with Matchers {
   test("FIB tests ITR") {
     fib_itr(0) should equal (fib_rec(0))
     fib_itr(1) should equal (fib_rec(1))
     fib_itr(2) should equal (fib_rec(2))
     fib_itr(3) should equal (fib_rec(3))
     fib_itr(4) should equal (fib_rec(4))
     fib_itr(5) should equal (fib_rec(5))
     fib_itr(10) should equal (fib_rec(10))
     fib_itr(15) should equal (fib_rec(15))
     fib_itr(20) should equal (fib_rec(20))
     fib_itr(25) should equal (fib_itr(25))
     fib_itr(30) should equal (fib_rec(30))
     fib_itr(35) should equal (fib_rec(35))
     fib_itr(40) should equal (fib_rec(40))
   }

   test("FIB tests MATRIX"){
     fib_matrix(0) should equal (fib_itr(0))
     fib_matrix(1) should equal (fib_itr(1))
     fib_matrix(2) should equal (fib_itr(2))
     fib_matrix(3) should equal (fib_itr(3))
     fib_matrix(4) should equal (fib_itr(4))
     fib_matrix(5) should equal (fib_itr(5))
     fib_matrix(11) should equal (fib_itr(11))
     fib_matrix(21) should equal (fib_itr(21))
     fib_matrix(31) should equal (fib_itr(31))
     fib_matrix(41) should equal (fib_itr(41))
     fib_matrix(51) should equal (fib_itr(51))
     fib_matrix(61) should equal (fib_itr(61))
     fib_matrix(71) should equal (fib_itr(71))
     fib_matrix(81) should equal (fib_itr(81))
     fib_matrix(91) should equal (fib_itr(91))
     fib_matrix(101) should equal (fib_itr(101))

  }

   test("FIB tests POLYNOMIAL"){
     fib_polynomial(0) should equal (fib_matrix(0))
     fib_polynomial(1) should equal (fib_matrix(1))
     fib_polynomial(2) should equal (fib_matrix(2))
     fib_polynomial(3) should equal (fib_matrix(3))
     fib_polynomial(4) should equal (fib_matrix(4))
     fib_polynomial(5) should equal (fib_matrix(5))
     fib_polynomial(12) should equal (fib_matrix(12))
     fib_polynomial(22) should equal (fib_matrix(22))
     fib_polynomial(32) should equal (fib_matrix(32))
     fib_polynomial(42) should equal (fib_matrix(42))
     fib_polynomial(52) should equal (fib_matrix(52))
     fib_polynomial(62) should equal (fib_matrix(62))
     fib_polynomial(72) should equal (fib_matrix(72))
     fib_polynomial(82) should equal (fib_matrix(82))
     fib_polynomial(92) should equal (fib_matrix(92))
     fib_polynomial(102) should equal (fib_matrix(102))
     fib_polynomial(100001) should equal (fib_matrix(100001))
   }

}
