
public class TrueFalse extends Question {
    public TrueFalse() {
        super(QuestionType.TRUE_FALSE);
    }

    @Override
    public void addQuestion() {
        this.question = UserInput.readNonEmptyLine("Enter the prompt for the True/False question: ");
    }

    @Override
    public void displayQuestion() {
        System.out.println(question + " (True/False)");
    }

    @Override
    public boolean isValidAnswer(String input) {
        if (input == null) return false;
        String resp = input.trim().toLowerCase();
        return resp.equals("true") || resp.equals("false") || resp.equals("t") || resp.equals("f");
    }

    @Override
    public void modifyQuestion() {
        System.out.println("Current prompt: " + this.question);
        boolean changePrompt = UserInput.confirm("Do you want to modify the prompt of this True/False question? (Y/N): ");
        if (changePrompt) {
            this.question = UserInput.readNonEmptyLine("Enter a new prompt for the True/False question: ");
        }
    }
}
