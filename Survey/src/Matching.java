import java.util.*;

public class Matching extends Question {
    private List<String> leftOptions;
    private List<String> rightOptions;

    public Matching() {
        super(QuestionType.MATCHING);
        this.leftOptions = new ArrayList<>();
        this.rightOptions = new ArrayList<>();
    }

    @Override
    public void addQuestion() {
        this.question = UserInput.readNonEmptyLine("Enter the prompt for the Matching question (e.g., 'Match the following'): ");
        int numPairs = UserInput.readInt("Enter the number of matching pairs: ", 1, Integer.MAX_VALUE);
        leftOptions.clear();
        rightOptions.clear();
        for (int i = 1; i <= numPairs; i++) {
            String left = UserInput.readNonEmptyLine("Enter left item " + i + ": ");
            String right = UserInput.readNonEmptyLine("Enter right item " + i + ": ");
            leftOptions.add(left);
            rightOptions.add(right);
        }
    }

    @Override
    public void displayQuestion() {
        System.out.println(question);
        // Determine width for left column for alignment.
        int maxLeftLength = 0;
        for (int i = 0; i < leftOptions.size(); i++) {
            String leftItem = (i + 1) + ") " + leftOptions.get(i);
            if (leftItem.length() > maxLeftLength) {
                maxLeftLength = leftItem.length();
            }
        }
        // Ensure some minimum spacing if needed
        maxLeftLength = Math.max(maxLeftLength, 10);
        // Print each pair with left and right columns.
        for (int i = 0; i < leftOptions.size(); i++) {
            String leftLabel = (i + 1) + ") " + leftOptions.get(i);
            // Pad the left label to maxLeftLength
            int padding = maxLeftLength - leftLabel.length();
            if (padding < 1) padding = 1;
            String spaces = " ".repeat(padding);
            char rightLabel = (char) ('A' + i);
            String rightItem = rightOptions.get(i);
            System.out.println(leftLabel + spaces + rightLabel + ") " + rightItem);
        }
    }

    @Override
    public boolean isValidAnswer(String input) {
        if (input == null) return false;
        String resp = input.trim().toUpperCase();
        if (resp.isEmpty()) {
            return false;
        }
        // Expect the user to provide matches for all left items, e.g., "A B C" or "A,C,B" etc.
        String[] tokens = resp.split("[, ]+");
        int n = leftOptions.size();
        if (tokens.length != n) {
            return false; // user must provide exactly one choice for each left item
        }
        Set<String> seen = new HashSet<>();
        for (String token : tokens) {
            if (token.isEmpty()) continue;
            // Each token should be a single letter.
            if (token.length() != 1) {
                return false;
            }
            char choice = token.charAt(0);
            if (choice < 'A' || choice >= (char) ('A' + rightOptions.size())) {
                return false; // letter out of range
            }
            String choiceStr = String.valueOf(choice);
            if (seen.contains(choiceStr)) {
                return false; // duplicate use of a right item
            }
            seen.add(choiceStr);
        }
        // Also ensure the number of unique choices equals number of left items
        return (seen.size() == leftOptions.size());
    }

    @Override
    public void modifyQuestion() {
        // Show current prompt and pairs.
        System.out.println("Current prompt: " + this.question);
        boolean changePrompt = UserInput.confirm("Do you want to modify the prompt of this matching question? (Y/N): ");
        if (changePrompt) {
            this.question = UserInput.readNonEmptyLine("Enter the new prompt: ");
        }
        System.out.println("Current matching pairs:");
        for (int i = 0; i < leftOptions.size(); i++) {
            char rightLabel = (char) ('A' + i);
            System.out.println("    " + (i + 1) + ") " + leftOptions.get(i) + "  <->  " + rightLabel + ") " + rightOptions.get(i));
        }
        // Modify pairs if desired
        boolean changePairs = UserInput.confirm("Do you want to change the matching pairs? (Y/N): ");
        if (changePairs) {
            int numPairs = UserInput.readInt("Enter the new number of pairs: ", 1, Integer.MAX_VALUE);
            leftOptions.clear();
            rightOptions.clear();
            for (int i = 1; i <= numPairs; i++) {
                String left = UserInput.readNonEmptyLine("Enter left item " + i + ": ");
                String right = UserInput.readNonEmptyLine("Enter right item " + i + ": ");
                leftOptions.add(left);
                rightOptions.add(right);
            }
        }
    }

    public List<String> getLeftOptions() {
        return leftOptions;
    }

    public List<String> getRightOptions() {
        return rightOptions;
    }
}
