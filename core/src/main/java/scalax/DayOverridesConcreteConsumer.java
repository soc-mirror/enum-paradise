package scalax;

public class DayOverridesConcreteConsumer {
    public static void main(String[] args) {
        System.out.println("Day.Monday");
        System.out.println(DayOverridesConcrete.Monday);
        System.out.println("Day.values()");
        for (DayOverridesConcrete day : DayOverridesConcrete.values()) {
            System.out.println(day);
        }
        System.out.println("Day.valueOf(\"Monday\")");
        DayOverridesConcrete monday = DayOverridesConcrete.valueOf("Monday");
        System.out.println(monday);
        System.out.println("Day.Monday.isWeekend()");
        System.out.println(monday.isWeekend());
        System.out.println("Day.Sunday.isWeekend()");
        System.out.println(DayOverridesConcrete.Friday.isWeekend());
    }
}
