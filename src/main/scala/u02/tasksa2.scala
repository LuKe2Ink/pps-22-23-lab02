package u02

object tasksa2  extends App:
  val positive: Int => String = (m: Int) => m match
    case i if i>=0 => "positive"
    case i if i<0 => "negative"

  println(positive(1))

  def positive2(m:Int): String = m match
    case i if i >= 0 => "positive"
    case i if i < 0 => "negative"

  println(positive2(2))
