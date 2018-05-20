package value

import expression._
import context._

class Thunk(params: List[Identifier] = Nil, body: Expression, defEnv: Environment) extends Closure(params, body, defEnv){
  def thunk(args: List[Value]): Value = {
    val tempEnv = new Environment(defEnv)
    //bind params to args in temEnv
    tempEnv.bulkPut(params, args)
    //execute body in tempEnv
    body.execute(tempEnv)
  }
}