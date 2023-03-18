package u02


import scala.compiletime.ops.boolean.!

object Lab02  extends App:

  //task part 1
  //es 1
  def hello() = println("Hello world!")
  hello()

  //task part 2a
  //es 3
  //a)
  val positive: Int => String = (m: Int) => m match
    case i if i>=0 => "positive"
    case i if i<0 => "negative"

  println(positive(1)) // positive
  println(positive(-1)) // negative

  def positive2(m:Int): String = m match
    case i if i >= 0 => "positive"
    case i if i < 0 => "negative"

  println(positive2(2)) // positive
  println(positive2(-1)) // negative

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
  println(notEmpty("foo") && !notEmpty("")) // true

  //c)
  def negF1[X](m: (X => Boolean)): (X => Boolean) =
    y => !m(y)

  val zero: Int => Boolean = _ == 0 // predicate on int
  val notEmptyString = negF1[String](empty)
  val notZero = negF1[Int](zero)
  println("Generic Predicate neg")
  println(notEmptyString("foo")) // true
  println(notEmptyString("")) // false
  println(notEmptyString("foo") && !notEmptyString("")) //true
  println(notZero(1)) // true
  println(notZero(0)) // false
  println(notZero(1) && !notZero(0))//true

  //task part 2b
  //es 4

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
  println(p1(3)(4)(4)) //true
  println(p2(3, 4, 4)) //true
  println(p3(3)(4)(4)) //true
  println(p4(3, 4, 4)) //true


  //task part 2b
  //es 5

  def compose(f: Int=>Int, g: Int=>Int): Int => Int = x =>
    f(g(x))

  println("Compose")
  println(compose(_ - 1, _ *2)(5)) // 9

  def composeGeneric[X](f: X => X, g: X => X): X => X = x =>
    f(g(x))

  println("Generic Compose")
  println(composeGeneric[Int](_ - 1, _ * 2)(5))// 9

  //task part 3
  //es 6
  @annotation.tailrec
  def gcd(a: Int, b: Int): Int = (a,b) match
    case (a,b) if (a % b == 0) => b
    case (a,b) if (a > b) => gcd(b, (a % b))
    case (a,b) if (a <= b) => gcd(a, (a % b))

  println("Greatest common divisor")
  println(gcd(12,8)) // 4
  println(gcd(14,7)) // 7
  println(gcd(4,2)) // 2
  println(gcd(4,3)) // 1

  //task part 4
  //es 7
  enum Shape:
    case Rectangle(base: Double, height: Double, center:(Double, Double))
    case Circle(radius: Double, center:(Double, Double))
    case Square(side: Double, center:(Double, Double))

  def perimeter(s: Shape): Double = s match
    case Shape.Rectangle(b, h, _) => (2*b)+(2*h)
    case Shape.Circle(r, _) => 2*r*scala.math.Pi
    case Shape.Square(s, _) => s*4

  def contains(s: Shape, point: (Double, Double)): Boolean = s match
    case Shape.Rectangle(b, h, (x, y)) => ((point._1 <= x + b && point._1 >= x) && (point._2 <= y + h && point._2 >= y))
    case Shape.Circle(r, (x, y)) => ((point._1 <= x + r && point._1 >= -(x + r)) && (point._2 <= y + r && point._2 >= -(y + r)))
    case Shape.Square(s, (x, y)) => ((point._1 <= x + s && point._1 >= x) && (point._2 <= y + s && point._2 >= y))


  println("Perimeter of the shapes")
  println(perimeter(Shape.Circle(2.0, (0.0,0.0)))) //12.56...
  println(perimeter(Shape.Rectangle(2.0, 2.0, (0.0,0.0)))) //8
  println(perimeter(Shape.Square(2.0, (0.0,0.0)))) //8

  println("Shape contains points")
  println(contains(Shape.Circle(2.0, (0.0, 0.0)), (1.0, 2.0))) //true
  println(contains(Shape.Rectangle(2.0, 2.0, (0.0, 0.0)), (1.0, 2.0))) //true
  println(contains(Shape.Square(2.0, (0.0, 0.0)), (1.0, 2.0))) //true

  println(contains(Shape.Circle(2.0, (0.0, 0.0)), (1.0, 2.0))) //false
  println(contains(Shape.Rectangle(2.0, 2.0, (0.0, 0.0)), (1.0, 2.0))) //false
  println(contains(Shape.Square(2.0, (0.0, 0.0)), (1.0, 2.0))) //false

  //task part 5
  //es 8
  enum Option[A]:
    case Some(a: A)
    case None() // here parens are needed because of genericity

  object Option:

    def isEmpty[A](opt: Option[A]): Boolean = opt match
      case None() => true
      case _ => false

    def orElse[A, B >: A](opt: Option[A], orElse: B): B = opt match
      case Some(a) => a
      case _ => orElse

    def flatMap[A, B](opt: Option[A])(f: A => Option[B]): Option[B] = opt match
      case Some(a) => f(a)
      case _ => None()

    def filter[A](opt: Option[A])(pred: A => Boolean): Option[A] = opt match
      case Some(a) if pred(a) => Some(a)
      case _ => None()

    def map[A](opt: Option[A])(pred: A => Boolean): Option[Boolean] = opt match
      case Some(a) => Option.Some(pred(a))
      case _ => None()

    def fold[A](opt: Option[A])(x: Int)(f: A => Int): Int = opt match
      case Some(a) => f(a)
      case _ => x

  import Option.*

  val s1: Option[Int] = Some(1)
  val s2: Option[Int] = Some(2)
  val s3: Option[Int] = None()

  println("Extended Options[A]")
  println(s1) // Some(1)
  println(orElse(s1, 0))
  println(orElse(s3, 0)) // 1,0
  println(flatMap(s1)(i => Some(i + 1))) // Some(2)
  println(flatMap(s1)(i => flatMap(s2)(j => Some(i + j)))) // Some(3)
  println(flatMap(s1)(i => flatMap(s3)(j => Some(i + j)))) // None
  println(filter(s1)(_ > 2)) // None
  println(filter(s2)(_ > 1)) // Some(2)
  println(filter(s3)(_ > 2)) // None
  println(map(s1)(_ > 2)) // Some(false)
  println(map(s2)(_ > 1)) // Some(true)
  println(map(s3)(_ > 2)) // None
  println(fold(s1)(1)(_ + 2)) // 3
  println(fold(s2)(1)(_ + 2)) // 4
  println(fold(s3)(1)(_ + 2)) // 1