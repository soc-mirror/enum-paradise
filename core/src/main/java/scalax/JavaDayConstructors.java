package scalax;

public enum JavaDayConstructors {
    Monday("Mon"),
    Tuesday("Tue"),
    Wednesday("Wed"),
    Thursday("Thu"),
    Friday("Fri"),
    Saturday("Sat"),
    Sunday("Sun");
    private JavaDayConstructors(String abbr) {
        this.abbr = abbr;
    }
    public final String abbr;
}
