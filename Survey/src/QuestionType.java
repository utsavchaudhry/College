import java.io.Serializable;

public enum QuestionType implements Serializable {
    TRUE_FALSE("True/False"),
    MULTIPLE_CHOICE("Multiple Choice"),
    SHORT_ANSWER("Short Answer"),
    ESSAY("Essay"),
    DATE("Date"),
    MATCHING("Matching");

    private final String displayName;

    QuestionType(String name) {
        this.displayName = name;
    }

    @Override
    public String toString() {
        return displayName;
    }
}
