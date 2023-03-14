package u02

import scala.compiletime.ops.boolean.!

object task2  extends App:

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
  val notEmpty = negF(empty) // which type of notEmpty?
  println(notEmpty("foo")) // true
  println(notEmpty("")) // false
  println(notEmpty("foo") && !notEmpty(""))

  

