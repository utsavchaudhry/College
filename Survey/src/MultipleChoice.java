import java.util.*;

public class MultipleChoice extends Question {
    private List<String> options;
    private boolean allowMultipleAnswers;

    public MultipleChoice() {
        super(QuestionType.MULTIPLE_CHOICE);
        this.options = new ArrayList<>();
        this.allowMultipleAnswers = false;
    }

    @Override
    public void addQuestion() {
        this.question = UserInput.readNonEmptyLine("Enter the prompt for the Multiple Choice question: ");
        int numOptions;
        do {
            numOptions = UserInput.readInt("Enter the number of answer choices for this question: ", 2, Integer.MAX_VALUE);
            if (numOptions < 2) {
                System.out.println("There must be at least 2 options. Please enter 2 or more.");
            }
        } while (numOptions < 2);
        // Read each option
        options.clear();
        for (int i = 1; i <= numOptions; i++) {
            String optText = UserInput.readNonEmptyLine("Enter option " + i + ": ");
            options.add(optText);
        }

        allowMultipleAnswers = UserInput.confirm("Allow multiple selections for this question? (Y/N): ");
    }

    @Override
    public void displayQuestion() {
        System.out.println(question);
        char label = 'A';
        for (String opt : options) {
            System.out.println("    " + label + ") " + opt);
            label++;
        }
        if (allowMultipleAnswers) {
            System.out.println("    (Select all that apply)");
        }
    }

    @Override
    public boolean isValidAnswer(String input) {
        if (input == null) return false;
        String resp = input.trim().toUpperCase();
        if (resp.isEmpty()) {
            return false;
        }
        if (allowMultipleAnswers) {
            // For multiple selection, allow comma-separated or space-separated letters.
            // Split the input by commas and/or whitespace.
            String[] tokens = resp.split("[, ]+");
            if (tokens.length == 0) {
                return false;
            }
            Set<String> chosen = new HashSet<>();
            for (String token : tokens) {
                if (token.isEmpty()) continue; // skip any empty tokens (could happen with consecutive delimiters)
                // Each token should be a single letter representing an option.
                if (token.length() != 1) {
                    return false; // if someone provided more than one character per option, invalid
                }
                char choice = token.charAt(0);

                if (Character.isDigit(choice)) {
                    int index = choice - '0';
                    try {
                        index = Integer.parseInt(token);
                    } catch (NumberFormatException e) {
                        return false;
                    }
                    index -= 1;
                    if (index < 0 || index >= options.size()) {
                        return false;
                    }
                    choice = (char) ('A' + index);
                }
                if (choice < 'A' || choice >= (char) ('A' + options.size())) {
                    return false;
                }
                String choiceStr = String.valueOf(choice);
                if (chosen.contains(choiceStr)) {
                    return false; // duplicate choice
                }
                chosen.add(choiceStr);
            }
            return !chosen.isEmpty();
        } else {
            // Only one answer allowed. Expect a single option letter (or a single digit).
            if (resp.length() == 1) {
                char choice = resp.charAt(0);
                if (Character.isDigit(choice)) {
                    // User entered a number instead of a letter.
                    int index = choice - '0';
                    index -= 1;
                    if (index >= 0 && index < options.size()) {
                        return true;
                    } else {
                        return false;
                    }
                } else {
                    // Check that it's within A..(A+options-1)
                    return (choice >= 'A' && choice < (char) ('A' + options.size()));
                }
            }
            return false;
        }
    }

    @Override
    public void modifyQuestion() {
        System.out.println("Current prompt: " + this.question);
        boolean changePrompt = UserInput.confirm("Do you want to modify the prompt of this multiple choice question? (Y/N): ");
        if (changePrompt) {
            this.question = UserInput.readNonEmptyLine("Enter the new prompt: ");
        }

        System.out.println("Current options:");
        char label = 'A';
        for (String opt : options) {
            System.out.println("    " + label + ") " + opt);
            label++;
        }
        System.out.println("Current setting: " + (allowMultipleAnswers ? "Multiple selections allowed" : "Single selection only"));

        boolean changeOptions = UserInput.confirm("Do you want to change the answer options for this question? (Y/N): ");
        if (changeOptions) {
            int numOptions;
            do {
                numOptions = UserInput.readInt("Enter the new number of options: ", 2, Integer.MAX_VALUE);
                if (numOptions < 2) {
                    System.out.println("There must be at least 2 options.");
                }
            } while (numOptions < 2);
            options.clear();
            for (int i = 1; i <= numOptions; i++) {
                String optText = UserInput.readNonEmptyLine("Enter option " + i + ": ");
                options.add(optText);
            }

            allowMultipleAnswers = UserInput.confirm("Allow multiple selections for this question? (Y/N): ");
        }
    }

    public List<String> getOptions() {
        return options;
    }
}
