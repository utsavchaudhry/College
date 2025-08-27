import java.time.LocalDate;
import java.time.format.DateTimeFormatter;
import java.time.format.DateTimeParseException;

public class Date extends Question {

    private String date;

    public Date() {
        super(QuestionType.DATE);
        this.date = "";
    }

    @Override
    public void addQuestion() {
        this.question = UserInput.readNonEmptyLine("Enter the prompt for the Date question: ");
    }

    @Override
    public void displayQuestion() {
        System.out.println(question + " (Format: MM/DD/YYYY)");
    }

    public static boolean isValidDate(String dateStr) {
        if (dateStr == null) return false;
        String s = dateStr.trim();
        // Check format with regex: one or two digits for month/day and exactly 4 digits for year, separated by '/'
        if (!s.matches("\\d{1,2}/\\d{1,2}/\\d{4}")) {
            return false;
        }
        String[] parts = s.split("/");
        if (parts.length != 3) {
            return false;
        }
        try {
            int month = Integer.parseInt(parts[0]);
            int day = Integer.parseInt(parts[1]);
            int year = Integer.parseInt(parts[2]);
            if (month < 1 || month > 12) {
                return false;
            }
            if (day < 1 || day > 31) {
                return false;
            }
            // Adjust day limits for specific months
            if ((month == 4 || month == 6 || month == 9 || month == 11) && day > 30) {
                return false;
            }
            if (month == 2) {
                // February: check for leap year if day is 29
                if (day > 29) {
                    return false;
                }
                if (day == 29) {
                    // Check leap year: divisible by 4, but not by 100 unless also by 400
                    return (year % 4 == 0) && ((year % 100 != 0) || (year % 400 == 0));
                }
            }
            // If we've passed all checks, it's a plausible date.
            return true;
        } catch (NumberFormatException e) {
            return false;
        }
    }

    @Override
    public boolean isValidAnswer(String input) {
        return isValidDate(input);
    }

    @Override
    public void modifyQuestion() {
        System.out.println("Current prompt: " + this.question);
        boolean changePrompt = UserInput.confirm("Do you want to modify the prompt of this date question? (Y/N): ");
        if (changePrompt) {
            this.question = UserInput.readNonEmptyLine("Enter the new prompt: ");
        }
    }
}
