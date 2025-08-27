
public class ShortAnswer extends Question {
    private int characterLimit;

    public ShortAnswer() {
        super(QuestionType.SHORT_ANSWER);
        this.characterLimit = 0;
    }

    @Override
    public void addQuestion() {
        this.question = UserInput.readNonEmptyLine("Enter the prompt for the Short Answer question: ");
        characterLimit = UserInput.readInt("Enter the character limit for the answer: ", 1, Integer.MAX_VALUE);
    }

    @Override
    public void displayQuestion() {
        System.out.println(question + " (Maximum " + characterLimit + " characters)");
    }

    @Override
    public boolean isValidAnswer(String input) {
        if (input == null) return false;
        String resp = input.trim();
        if (resp.isEmpty()) {
            return false;
        }
        return resp.length() <= characterLimit;
    }

    @Override
    public void modifyQuestion() {
        System.out.println("Current prompt: " + this.question);
        System.out.println("Current character limit: " + this.characterLimit);
        boolean changePrompt = UserInput.confirm("Do you want to modify the prompt of this short answer question? (Y/N): ");
        if (changePrompt) {
            this.question = UserInput.readNonEmptyLine("Enter the new prompt: ");
        }
        boolean changeLimit = UserInput.confirm("Do you want to change the character limit? (Y/N): ");
        if (changeLimit) {
            characterLimit = UserInput.readInt("Enter the new character limit: ", 1, Integer.MAX_VALUE);
        }
    }

    public int getCharacterLimit() {
        return characterLimit;
    }
}
