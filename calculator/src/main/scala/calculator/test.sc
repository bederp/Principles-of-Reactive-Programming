package calculator

import Math._

object test {
  println("Welcome to the Scala worksheet")       //> Welcome to the Scala worksheet
  sqrt(4)                                         //> res0: Double = 2.0
  sqrt(-4)                                        //> res1: Double = NaN
  
  var b = -5                                      //> b  : Int = -5
  var a = -2                                      //> a  : Int = -2
  
  -b/2*a                                          //> res2: Int = -4
  (-b)/2*a                                        //> res3: Int = -4
  
}