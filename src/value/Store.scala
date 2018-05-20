package value

import collection.mutable._
import context._

class Store(private val elems: ArrayBuffer[Value] = ArrayBuffer[Value]()) extends Value {
  // adds e to the end of store
  def add(e: Value) {elems += e}
  
  // inserts e at position pos in this
  def put(e: Value, pos: Integer) {elems.insert(pos.value, e)}
  
  // removes element at position pos from this
  def rem(pos: Integer) {elems.remove(pos.value)}
  
  // returns element at position pos in this
  def get(pos: Integer): Value = elems.apply(pos.value)
  
  // returns true ie this contains e
  def contains(e: Value): Boole = Boole(elems.contains(e))
  
  // returns the size of this
  def size: Integer = Integer(elems.length)
  
  // returns "{e0 e1 e2 ...}"
  override def toString = {
    var string = ""
    for(e <- elems){
      string += e.toString() + " "
    }
    "{" + string + "}"
  }
  
  // returns store containing the elements of this transformed by trans
  def map(trans: Closure): Store = {
    var buffer: ArrayBuffer[Value] = ArrayBuffer[Value]()
    for(elem <- elems.toList){
      buffer += trans.apply(elem::Nil)
    }
    new Store(buffer)
  }
  
  // returns store containing the elements of this that passed test
  def filter(test: Closure): Store = {
    var buffer: ArrayBuffer[Value] = ArrayBuffer[Value]()
    for(elem <- elems.toList){
      if(test.apply(elem::Nil) == Boole(true)) buffer += elem
    }
    new Store(buffer)
  }
}