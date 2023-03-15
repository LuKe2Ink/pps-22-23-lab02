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
  println("Predicate neg")
  println(notEmpty("foo")) // true
  println(notEmpty("")) // false
  println(notEmpty("foo") && !notEmpty(""))

  //c)
  def negF1[X](m: (X => Boolean)): (X => Boolean) =
    y => !m(y)

  val zero: Int => Boolean = _ == 0 // predicate on int
  val notEmptyString = negF1[String](empty)
  val notZero = negF1[Int](zero)
  println("Generic Predicate neg")
  println(notEmptyString("foo")) // true
  println(notEmptyString("")) // false
  println(notEmptyString("foo") && !notEmptyString(""))
  println(notZero(1)) // true
  println(notZero(0)) // false
  println(notZero(1) && !notZero(0))

  //task 2.b 4

  //curried
  val p1: Int => Int => Int => Boolean =
    x => y => z => ((x<=y) && y==z)

  //not curried
  val p2: (Int, Int, Int) => Boolean = (x:Int, y:Int, z:Int) =>
    ((x<=y) && (y==z))

  //curried
  def p3(x: Int)(y: Int)(z: Int): Boolean =
    ((x<=y) && (y==z))

  //curried
  def p4(x: Int, y: Int, z: Int): Boolean =
    ((x <= y) && (y == z))

  println("Curried and not predicate")
  println(p1(3)(4)(4))
  println(p2(3, 4, 4))
  println(p3(3)(4)(4))
  println(p4(3, 4, 4))


  //task 2.b 5

  def compose(f: Int=>Int, g: Int=>Int): Int => Int = x =>
    f(g(x))

  println("Compose")
  println(compose(_ - 1, _ *2)(5))