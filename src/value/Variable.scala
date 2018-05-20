package value

import context._ 

case class Variable(private var content: Value) extends Value{
  def getContent: Value = content
  def setContent(c: Value) = this.content = c
  override def toString(): String = "[" + content + "]"
}