package value

import expression._
import context._

class Text(val body: Expression) extends Value {
  def apply(env: Environment): Value = {
    body.execute(env)
  }
}