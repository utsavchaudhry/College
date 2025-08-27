import java.io.Serializable;
import java.util.ArrayList;
import java.util.List;

public class Answer implements Serializable {
    private List<String> answers;

    public Answer(List<String> answerList) {
        this.answers = answerList;
    }

    public Answer() {
        this.answers = new ArrayList<>();
    }

    public List<String> getResponse() {
        return answers;
    }

    public void addResponse(String response) {
        answers.add(response);
    }

    public boolean isValidAnswer() {
        return answers != null && !answers.isEmpty();
    }

    public void editResponse(int id, String newResponse) {
        if (id >= 0 && id < answers.size()) {
            answers.set(id, newResponse);
        }
    }

    public String loadResponse(int id) {
        if (id >= 0 && id < answers.size()) {
            return answers.get(id);
        }
        return null;
    }

    public void saveResponse() {

    }

    public List<Answer> getCorrectAnswers() {
        // returning a self list here for compatibility
        List<Answer> list = new ArrayList<>();
        list.add(this);
        return list;
    }
}
