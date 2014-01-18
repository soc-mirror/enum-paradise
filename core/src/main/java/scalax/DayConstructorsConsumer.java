package scalax;

public class DayConstructorsConsumer {
    public static void main(String[] args) {
        System.out.println("Day.Monday");
        System.out.println(DayConstructors.Monday);
        System.out.println("Day.values()");
        for (DayConstructors day : DayConstructors.values()) {
            System.out.println(day);
        }
        System.out.println("Day.valueOf(\"Monday\")");
        DayConstructors monday = DayConstructors.valueOf("Monday");
        System.out.println(monday);
        System.out.println("Day.Monday.abbreviation()");
        System.out.println(monday.abbreviation());
        System.out.println("Call method foo defined in Day");
        System.out.println(monday.foo());
        System.out.println("Call method bar defined in Bar and mixed into Day");
        System.out.println(monday.bar());
    }
}
