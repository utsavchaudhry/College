import java.io.Serializable;
import java.util.ArrayList;
import java.util.List;

public abstract class Question implements Serializable {
    protected String question;
    protected QuestionType questionType;
    protected Answer userAnswer;

    public Question(QuestionType type) {
        this.questionType = type;
    }

    public String getQuestionPrompt() {
        return question;
    }
    public String getQuestion() {
        return question;
    }
    public QuestionType getQuestionType() {
        return questionType;
    }

    public void setUserResponse(Answer response) {
        this.userAnswer = response;
    }

    public Answer getUserResponse() {
        return userAnswer;
    }

    public abstract boolean isValidAnswer(String input);

    public abstract void addQuestion();

    public abstract void modifyQuestion();

    public abstract void displayQuestion();
}
