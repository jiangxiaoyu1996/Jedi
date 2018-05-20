package value

import expression._
import context._

trait Value 

case class Integer(val value: Int) extends Literal with Ordered[Integer] with Equals{
  def +(other: Integer) = Integer(this.value + other.value)
  def -(other: Integer) = Integer(this.value - other.value)
  def *(other: Integer) = Integer(this.value * other.value)
  def /(other: Integer) = if(other == 0) throw new Exception("Division by 0.") else Integer(this.value / other.value)
  def unary_- = Integer(this.value * (-1))
  override def toString = value.toString
  def compare(other: Integer): Int = if (this.value < other.value) -1 else if (other.value < this.value) 1 else 0
  override def canEqual(other: Any) =  other.isInstanceOf[Integer]
  override def equals(other: Any): Boolean = 
    other match {
       case other: Integer => this.canEqual(other) && (other.value == this.value)
       case _ => false
    }
  override def hashCode = this.toString.##
}

object Integer {
  implicit def intToReal(n: Integer): Real = Real(n.value.toDouble)
}

case class Real(val value: Double) extends Literal with Ordered[Real] with Equals{
  def +(other: Real) = Real(this.value + other.value)
  def -(other: Real) = Real(this.value - other.value)
  def *(other: Real) = Real(this.value * other.value)
  def /(other: Real) = if(other == 0) throw new Exception("Division by 0.") else Real(this.value / other.value)
  def unary_- = Real(this.value * (-1))
  override def toString = value.toString
  def compare(other: Real): Int = if (this.value < other.value) -1 else if (other.value < this.value) 1 else 0
  override def canEqual(other: Any) =  other.isInstanceOf[Double]
  override def equals(other: Any): Boolean = 
    other match {
       case other: Real => this.canEqual(other) && (other.value == this.value)
       case _ => false
    }
  override def hashCode = this.toString.##
}

case class Chars(val scalaValue: String) extends Literal with Ordered[Chars] {
   
  def substring(start: Integer, end: Integer) = Chars(scalaValue.substring(start.value, end.value))
  def length: Integer = Integer(scalaValue.length)
  
  def +(other: Chars): Chars = Chars(this.scalaValue + other.scalaValue)
  override def toString = scalaValue
  def compare(other: Chars): Int = this.scalaValue.compare(other.scalaValue)
  override def equals(other: Any): Boolean = 
    other match {
       case other: Chars => (other.isInstanceOf[Chars]) && (other.scalaValue == this.scalaValue)
       case _ => false
    }
  override def hashCode = this.toString.##
}

case class Boole(val value: Boolean) extends Literal{
  def &&(other: Boole) = Boole(value && other.value)
  def ||(other: Boole) = Boole(value || other.value)
  def unary_! = Boole(!value)
  override def toString = value.toString
}
  
