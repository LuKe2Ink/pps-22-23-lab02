package u02

import scala.compiletime.ops.boolean.!

object Task2  extends App:

  //task 2a.3
  //a)
  val positive: Int => String = (m: Int) => m match
    case i if i>=0 => "positive"
    case i if i<0 => "negative"

  println(positive(1))

  def positive2(m:Int): String = m match
    case i if i >= 0 => "positive"
    case i if i < 0 => "negative"

  println(positive2(2))

  //b)
  val neg: (String => Boolean) => (String => Boolean) =
    m => (x => !m(x))

  def negF(m: (String => Boolean)): (String => Boolean) =
    x => !m(x)


  val empty: String => Boolean = _ == "" // predicate on strings
  val notEmpty = negF(empty)
  println(notEmpty("foo")) // true
  println(notEmpty("")) // false
  println(notEmpty("foo") && !notEmpty(""))

  //c)
  def negF1[X](m: (X => Boolean)): (X => Boolean) =
    y => !m(y)

  val zero: Int => Boolean = _ == 0 // predicate on int
  val notEmptyString = negF1[String](empty)
  val notZero = negF1[Int](zero)
  println(notEmptyString("foo")) // true
  println(notEmptyString("")) // false
  println(notEmptyString("foo") && !notEmptyString(""))
  println(notZero(1)) // true
  println(notZero(0)) // false
  println(notZero(1) && !notZero(0))

