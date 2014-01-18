package scalax;

@DayAnnotation(bestDay = Day.Monday)
public class DayConsumer {
    public static void main(String[] args) {
        System.out.println("Day.Monday");
        System.out.println(Day.Monday);
        System.out.println("Day.values()");
        for (Day day : Day.values()) {
            System.out.println(day);
        }
        System.out.println("Day.valueOf(\"Monday\")");
        Day monday = Day.valueOf("Monday");
        System.out.println(monday);
    }
}
