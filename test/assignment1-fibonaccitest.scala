package prg1.assignment1

import org.scalatest._
import FIB._

class FIBTestRecToItr extends FunSuite with Matchers {
   test("FIB test Rec To Itr.") {
   	var n=10
   	while (n>=0){
     fib_itr(n) should equal (fib_rec(n))
     n=n-1
     }
     var m=100
     while (m>=0){
     fib_matrix(100) should equal (fib_itr(100))
     m=m-1
     }
   }

}
class FIBTestItrToMat extends FunSuite with Matchers {
   test("FIB test Itr To Mat.") {
   	var n=10
   	while (n>=0){
     fib_itr(n) should equal (fib_rec(n))
     n=n-1
     }
     var m=100
     while (m>=0){
     fib_matrix(100) should equal (fib_itr(100))
     m=m-1
     }
   }

}


