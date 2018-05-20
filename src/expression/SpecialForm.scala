package expression

import context._
import value._
import util.control.Breaks._

trait SpecialForm extends Expression

case class Conjunction(val operands: List[Expression]) extends SpecialForm {
  def execute(env: Environment): Value = {
     if (operands.length < 2) throw new TypeException("Inputs to && must be at least two")
     var result = true
     breakable{
       for(operand <- operands) {
         val r = operand.execute(env)
         if(r == Boole(false)) {
           result = false
           break
         }
       }
     }
     if(result == true) Boole(true)
     else Boole(false)
  }
}

case class Disjunction(val operands: List[Expression]) extends SpecialForm {
  def execute(env: Environment): Value = {
     if (operands.length < 2) throw new TypeException("Inputs to && must be at least two")
     var result = false
     breakable{
       for(operand <- operands) {
         val r = operand.execute(env)
         if(r == Boole(true)) {
           result = true
           break
         }
       }
     }
     if(result == true) Boole(true)
     else Boole(false)
  }
}

case class Conditional(val cond: Expression, val cons: Expression, val alt: Expression = null) extends SpecialForm {
  def execute(env: Environment): Value = {
    if(cond.execute(env) == Boole(true)) cons.execute(env)
    else {
      if(alt == null) Notification.UNSPECIFIED
      else alt.execute(env)
    }
  }
}

case class Declaration(val id: Identifier, val exp: Expression) extends SpecialForm {
  def execute(env: Environment): Notification = {
    env += (id -> exp.execute(env))
    Notification.OK
  }
}

case class Block(val exp: List[Expression]) extends SpecialForm {
  def execute(env: Environment): Value = {
    val tempEnv = new Environment(env)
    // execute expression relative to tempEnv
    val args: List[Value] = exp.map(_.execute(tempEnv))
    // return value of last one
    args.last
  }
}

case class Lambda(val params: List[Identifier], val body: Expression) extends SpecialForm {
  def execute(env: Environment): Closure = {
    new Closure(params, body, env)
  }
}

case class Assignment(private val vbl: Identifier, private val update: Expression) extends SpecialForm{
  def execute(env: Environment): Value = {
    val result = update.execute(env)
    env.apply(vbl).asInstanceOf[Variable].setContent(result)
    Notification.DONE
  }
}

case class Iteration(private val condition: Expression, val body: Expression) extends SpecialForm{
  def execute(env: Environment): Value = {
    while(condition.execute(env) == Boole(true)){
      body.execute(env)
    }
    Notification.DONE
  }
}
