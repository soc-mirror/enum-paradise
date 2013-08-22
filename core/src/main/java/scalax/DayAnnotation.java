package scalax;

public @interface DayAnnotation {
  // It's Friday, Friday
  Day bestDay() default Day.Friday;
}
