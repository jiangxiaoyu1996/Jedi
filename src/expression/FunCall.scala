package expression

import context._
import value._

case class FunCall(val operator: Identifier, val operands: List[Expression]) extends Expression{
  def execute(env: Environment): Value = { 
   val args: List[Value] = operands.map(_.execute(env))
   try{
     val maybeClosure = operator.execute(env)
     if(!maybeClosure.isInstanceOf[Closure]) {
       throw new UndefinedException(operator)
     }
     else {
       maybeClosure.asInstanceOf[Closure].apply(args)
     }
   }
   catch{
     case e: UndefinedException => alu.execute(operator, args)
   }
  }
  
  /*def execute(env: Environment): Value = {
    if(env.contains(operator)){
      val maybeClosure = operator.execute(env)
      if(maybeClosure.isInstanceOf[Closure]){
        val closure = maybeClosure.asInstanceOf[Closure]
        val args = operands.map(_.execute(env))
        closure(args, env)
      }
      else{
        throw new TypeException("Only functions can be called")
      }
    }
    else{
      val args = operands.map(_.execute(env))
      alu.execute(operator,args)
    }
  }*/
}
  
