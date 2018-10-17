package prg1.assignment1

import org.scalatest._
import FIB._

class FIBTest extends FunSuite with Matchers {
   test("FIB tests itr") {
     fib_itr(10) should equal (fib_rec(10))
     fib_itr(0) should equal (fib_rec(0))
 }

   test("FIB tests matrix"){
       for(n<- 0 to 20){
           fib_matrix(n*n) should equal (fib_itr(n*n))
       }
   }
}
