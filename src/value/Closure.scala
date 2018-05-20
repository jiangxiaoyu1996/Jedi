package value

import expression._
import context._

class Closure(val params: List[Identifier], val body: Expression, val defEnv: Environment) extends Value{
  
  /*                    //params
   * -> def foo = lambda(x, y) 2 * x + y
   *    ok
   * -> foo(5 + 1, 6 * 3) //operands
   *          ||     ||
   * //operator6      18   //arguments
   */
  
  def apply(args: List[Value]): Value = {
    val tempEnv = new Environment(defEnv)
    //val tempEnv = new Environment(defEnv)
    //bind params to args in temEnv
    tempEnv.bulkPut(params, args)
    //execute body in tempEnv
    body.execute(tempEnv)
  }
  
  /*Extra Credit*/
  /*def apply(args: List[Value], env: Environment): Value = {
    val tempEnv = new Environment(env)
    //bind params to args in temEnv
    tempEnv.bulkPut(params, args)
    //execute body in tempEnv
    body.execute(tempEnv)
  }*/
  
}