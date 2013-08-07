package scalax

@Enum
class Day {
    Monday
    Tuesday
    Wednesday
    Thursday
    Friday
}

object Main extends App {

  val mon = Day.Monday
  val tue = Day.Tuesday
  val days = Day.values.toList
  println(s"Day.Monday: \t\t\t\t $mon")
  println(s"Day.Monday.name: \t\t\t ${mon.name}")
  println(s"Day.Monday.ordinal: \t\t\t ${mon.ordinal}")
  println(s"Day.Monday  compareTo Day.Tuesday: \t ${mon compareTo tue}")
  println(s"Day.Monday  compareTo Day.Monday: \t ${mon compareTo mon}")
  println(s"Day.Tuesday compareTo Day.Monday: \t ${tue compareTo mon}")
  println(s"Day.Monday == Day.Tuesday: \t\t ${mon == tue}")
  println(s"Day.Monday == Day.Monday: \t\t ${mon == mon}")
  println(s"Day.Monday.getDeclaringClass: \t\t ${mon.getDeclaringClass}")
  println(s"Day.values (not implemented yet): \t $days") // List(), because $VALUES is not populated yet
  // println(Day valueOf "Wednesday") // java.lang.IllegalArgumentException: scalax.Day is not an enum type
                                      // Why? Because the ENUM flag is not set yet
}
