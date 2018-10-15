package prg1.assignment1

import org.scalatest._
import FIB._

class FIBTest extends FunSuite with Matchers {	
	test("FIB tests") {
   		for(i <- 0 to 30) fib_itr(i) should equal (fib_rec(i))
   		for(i <- 0 to 100) fib_matrix(i) should equal (fib_itr(i))
   		for(i <- 0 to 100) fib_polynomial(i) should equal (fib_matrix(i))
   	}
}



