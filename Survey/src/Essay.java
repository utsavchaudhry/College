
public class Essay extends Question {
    public Essay() {
        super(QuestionType.ESSAY);
    }

    @Override
    public void addQuestion() {
        this.question = UserInput.readNonEmptyLine("Enter the prompt for the Essay question: ");
    }

    @Override
    public void displayQuestion() {
        System.out.println(question);
    }

    @Override
    public boolean isValidAnswer(String input) {
        if (input == null) return false;
        return !input.trim().isEmpty();
    }

    @Override
    public void modifyQuestion() {
        System.out.println("Current prompt: " + this.question);
        boolean changePrompt = UserInput.confirm("Do you want to modify the prompt of this essay question? (Y/N): ");
        if (changePrompt) {
            this.question = UserInput.readNonEmptyLine("Enter the new prompt: ");
        }
    }
}
