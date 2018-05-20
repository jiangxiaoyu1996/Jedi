package test

import expression._
import context._
import value._

object FunCallTest extends App {
  val globalEnvironment = new Environment
  
  val operands = List(Integer(6), Integer(7))
  val operands2 = List(Real(6.0), Integer(7))
  val operands3 = List(Chars("Charlene"), Chars("Jiang"))
  val operands4 = List(Boole(true), Boole(false))
  
  var exp = FunCall(Identifier("add"), operands)
  println("6 + 7 = " + exp.execute(globalEnvironment))
  exp = FunCall(Identifier("add"), operands2)
  println("6.0 + 7.0 = " + exp.execute(globalEnvironment))
  exp = FunCall(Identifier("add"), operands3)
  println("Charlene + Jiang = " + exp.execute(globalEnvironment))
  
  exp = FunCall(Identifier("mul"), operands)
  println("6 * 7 = " + exp.execute(globalEnvironment))
  exp = FunCall(Identifier("mul"), operands2)
  println("6.0 * 7 = " + exp.execute(globalEnvironment))
  
  exp = FunCall(Identifier("sub"), operands)
  println("6 - 7 = " + exp.execute(globalEnvironment))
  exp = FunCall(Identifier("sub"), operands2)
  println("6.0 - 7 = " + exp.execute(globalEnvironment))
  
  exp = FunCall(Identifier("div"), operands)
  println("6 / 7 = " + exp.execute(globalEnvironment))
  exp = FunCall(Identifier("div"), operands2)
  println("6.0 / 7 = " + exp.execute(globalEnvironment))
  
  exp = FunCall(Identifier("less"), operands)
  println("6 < 7 = " + exp.execute(globalEnvironment))
  exp = FunCall(Identifier("less"), operands2)
  println("6.0 < 7 = " + exp.execute(globalEnvironment))
  exp = FunCall(Identifier("less"), operands3)
  println("Charlene < Jiang = " + exp.execute(globalEnvironment))
  
  exp = FunCall(Identifier("more"), operands)
  println("6 > 7 = " + exp.execute(globalEnvironment))
  exp = FunCall(Identifier("more"), operands2)
  println("6.0 > 7 = " + exp.execute(globalEnvironment))
  exp = FunCall(Identifier("more"), operands3)
  println("Charlene > Jiang = " + exp.execute(globalEnvironment))
  
  exp = FunCall(Identifier("equals"), operands)
  println("6 == 7 = " + exp.execute(globalEnvironment))
  
  exp = FunCall(Identifier("unequals"), operands)
  println("6 != 7 = " + exp.execute(globalEnvironment))
  
  exp = FunCall(Identifier("not"), operands)
  println("-6 = " + exp.execute(globalEnvironment))
  exp = FunCall(Identifier("not"), operands2)
  println("-6.0 = " + exp.execute(globalEnvironment))
  exp = FunCall(Identifier("not"), operands4)
  println("!true = " + exp.execute(globalEnvironment))
  
  exp = FunCall(Identifier("write"), operands)
  println(exp.execute(globalEnvironment))
  exp = FunCall(Identifier("write"), operands2)
  println(exp.execute(globalEnvironment))
  exp = FunCall(Identifier("write"), operands3)
  println(exp.execute(globalEnvironment))
  exp = FunCall(Identifier("write"), operands4)
  println(exp.execute(globalEnvironment))
  
  exp = FunCall(Identifier("prompt"), operands)
  println(exp.execute(globalEnvironment))
  exp = FunCall(Identifier("prompt"), operands2)
  println(exp.execute(globalEnvironment))
  exp = FunCall(Identifier("prompt"), operands3)
  println(exp.execute(globalEnvironment))
  exp = FunCall(Identifier("prompt"), operands4)
  println(exp.execute(globalEnvironment))
  
  exp = FunCall(Identifier("read"), operands)
  println(exp.execute(globalEnvironment))
  exp = FunCall(Identifier("read"), operands2)
  println(exp.execute(globalEnvironment))
  exp = FunCall(Identifier("read"), operands3)
  println(exp.execute(globalEnvironment))
  exp = FunCall(Identifier("read"), operands4)
  println(exp.execute(globalEnvironment))
}