package scalax

object ScalaDayUnapplyConsumer extends App {
  val monday: DayConstructors = classOf[DayConstructors].getField("Monday").get(null).asInstanceOf[DayConstructors]

  val DayConstructors(abbr, weekend) = monday
  println(abbr)
  println(weekend)
  println(monday.foo)
}