import java.util.*;
import java.io.*;

public class Test extends Survey {
    private List<Answer> correctAnswer;

    public Test() {
        super();
        correctAnswer = new ArrayList<>();
    }

    public void display() {
        super.displaySurvey();
    }

    public void displayWithAnswers() {
        System.out.println("\n--- Test: " + this.getSurveyName() + " (with Correct Answers) ---");
        for (int i = 0; i < questionList.size(); i++) {
            Question q = questionList.get(i);
            System.out.print((i + 1) + ") ");
            q.displayQuestion();  // existing method prints prompt and choices

            // Now print correct answer(s). We assume you have a List<Answer> correctAnswer list:
            Answer correct = this.correctAnswer.get(i);
            if (correct != null && !correct.getResponse().isEmpty()) {
                // Format differently depending on question type
                if (q instanceof TrueFalse) {
                    // TrueFalse correct answer is a single string, e.g. "True" or "False"
                    System.out.println("   Correct Answer: " + correct.getResponse().get(0));
                }
                else if (q instanceof MultipleChoice) {
                    // Print each correct choice string (e.g., "A", "C")
                    System.out.print("   Correct Answer(s): ");
                    System.out.println(String.join(", ", correct.getResponse()));
                }
                else if (q instanceof ShortAnswer || q instanceof Date) {
                    // Single correct response
                    System.out.println("   Correct Answer: " + correct.getResponse().get(0));
                }
                else if (q instanceof Matching) {
                    // Each matching pair is stored as "Left -> Right"
                    System.out.println("   Correct Matches:");
                    for (String pair : correct.getResponse()) {
                        System.out.println("     " + pair);
                    }
                }
                else {
                    // Essay has no correct answer; skip or note
                    System.out.println("   (No correct answer for essay)");
                }
            } else {
                System.out.println("   (No correct answer recorded)");
            }
            System.out.println();  // blank line after each question block
        }
    }


    public double grade() {
        double total = 0;
        int count = 0;
        for (int i = 0; i < questionList.size(); i++) {
            Question q = questionList.get(i);
            if (q instanceof Essay) continue; // skip grading essays
            Answer userAnswer = q.getUserResponse();
            Answer correct = correctAnswer.get(i);
            if (userAnswer != null && userAnswer.getResponse().equals(correct.getResponse())) {
                total++;
            }
            count++;
        }
        return count == 0 ? 0 : (total / count) * 100;
    }

    public void create() {
        super.createSurvey();
        Scanner sc = new Scanner(System.in);
        for (Question q : questionList) {
            System.out.println("Enter correct answer(s) for: " + q.getQuestionPrompt());
            String answer = sc.nextLine();
            boolean okay = q.isValidAnswer(answer);
            while (!okay) {
                System.out.println("Invalid Format! Enter correct answer(s) for: " + q.getQuestionPrompt());
                answer = sc.nextLine();
                okay = q.isValidAnswer(answer);
            }
            Answer correct = new Answer();
            correct.addResponse(answer);
            correctAnswer.add(correct);
        }
    }

    public void modify() {
        super.changeSurvey();
        Scanner sc = new Scanner(System.in);
        for (int i = 0; i < correctAnswer.size(); i++) {
            System.out.println("Modify correct answer for question " + (i+1) + "? (y/n)");
            if (sc.nextLine().trim().equalsIgnoreCase("y")) {
                System.out.print("Enter new correct answer: ");
                Answer a = new Answer();
                a.addResponse(sc.nextLine());
                correctAnswer.set(i, a);
            }
        }
    }

    public void take() {
        super.takeSurvey();
    }

    public void store() {
        String filename = UserInput.readNonEmptyLine("Enter a filename to save the survey: ");
        if (!filename.endsWith(".ser")) {
            filename += ".ser";
        }
        try (ObjectOutputStream oos = new ObjectOutputStream(new FileOutputStream(filename))) {
            oos.writeObject(this);
        } catch (IOException e) {
            System.out.println("Error storing test: " + e.getMessage());
        }
    }

    public void load(String fileName) {
        try (ObjectInputStream ois = new ObjectInputStream(new FileInputStream(fileName))) {
            Test loaded = (Test) ois.readObject();
            this.questionList = loaded.questionList;
            this.correctAnswer = loaded.correctAnswer;
        } catch (IOException | ClassNotFoundException e) {
            System.out.println("Error loading test: " + e.getMessage());
        }
    }

    public List<Answer> getCorrectAnswer() {
        return correctAnswer;
    }
}