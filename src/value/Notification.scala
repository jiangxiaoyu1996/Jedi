package value

class Notification(val value: String) extends Value{
  override def toString = this.value
}

object Notification {
  val OK = new Notification("ok")
  val DONE = new Notification("done")
  val UNSPECIFIED = new Notification("unspecified")
  
  def apply(value: String) = new Notification(value)
}