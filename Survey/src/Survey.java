import java.io.*;
import java.util.*;
import java.time.LocalDateTime;
import java.time.format.DateTimeFormatter;

public class Survey implements Serializable {
    private String surveyName;
    protected List<Question> questionList;
    private boolean saved;

    public Survey() {
        this.surveyName = "";
        this.questionList = new ArrayList<>();
        this.saved = false;
    }

    public String getSurveyName() {
        return surveyName;
    }

    public List<Question> getQuestions() {
        return questionList;
    }

    public boolean isSaved() {
        return saved;
    }

    public void setSaved(boolean isSaved) {
        this.saved = isSaved;
    }

    public void createSurvey() {
        this.surveyName = UserInput.readNonEmptyLine("Enter a title for the new survey: ");
        System.out.println("Creating survey \"" + surveyName + "\". Now add questions:");
        questionList.clear();
        boolean adding = true;
        while (adding) {
            System.out.println("Choose question type to add:");
            System.out.println("  1) True/False");
            System.out.println("  2) Multiple Choice");
            System.out.println("  3) Short Answer");
            System.out.println("  4) Essay");
            System.out.println("  5) Date");
            System.out.println("  6) Matching");
            System.out.println("  7) Finish adding questions");
            int choice = UserInput.readInt("Select an option (1-7): ", 1, 7);
            Question q = null;
            switch (choice) {
                case 1:
                    q = new TrueFalse();
                    break;
                case 2:
                    q = new MultipleChoice();
                    break;
                case 3:
                    q = new ShortAnswer();
                    break;
                case 4:
                    q = new Essay();
                    break;
                case 5:
                    q = new Date();
                    break;
                case 6:
                    q = new Matching();
                    break;
                case 7:
                    adding = false;
                    break;
            }
            if (q != null) {
                // Let the question object handle gathering its details.
                q.addQuestion();
                questionList.add(q);
                saved = false;
            }
            // If done adding, exit loop.
        }
        System.out.println("Survey \"" + surveyName + "\" created with " + questionList.size() + " question(s).");
    }

    public void displaySurvey() {
        for (int i = 0; i < questionList.size(); i++) {
            Question q = questionList.get(i);
            int number = i + 1;
            // Number the question
            System.out.print(number + ") ");
            q.displayQuestion();
            System.out.println(); // blank line after each question for readability
        }
    }

    public void storeSurvey() {
        String filename = UserInput.readNonEmptyLine("Enter a filename to save the survey: ");
        if (!filename.endsWith(".ser")) {
            filename += ".ser";
        }
        try {
            FileOutputStream fos = new FileOutputStream(filename);
            ObjectOutputStream oos = new ObjectOutputStream(fos);
            oos.writeObject(this);
            oos.close();
            this.saved = true;
            System.out.println("Survey saved successfully to " + filename + ".");
        } catch (IOException e) {
            System.out.println("Error: Unable to save the survey to file.");
        }
    }

    public void takeSurvey() {
        if (this.questionList == null || this.questionList.isEmpty()) {
            System.out.println("No questions available in the survey to take.");
            return;
        }

        System.out.println("** Taking Survey: " + this.surveyName + " **");

        for (int i = 0; i < questionList.size(); i++) {
            Question q = questionList.get(i);
            System.out.println();
            System.out.println("Question " + (i+1) + ":");
            q.displayQuestion();

            Answer answerObj = new Answer();

            String userResponse;
            boolean valid = false;

            switch (q) {
                case Matching matchQ -> {
                    int leftCount = matchQ.getLeftOptions().size();
                    int rightCount = matchQ.getRightOptions().size();
                    Set<Integer> usedRightIndices = new HashSet<>();

                    for (int j = 0; j < leftCount; j++) {
                        String leftItem = matchQ.getLeftOptions().get(j);
                        valid = false;
                        while (!valid) {
                            System.out.print("Match for \"" + leftItem + "\" (enter option number 1-" + rightCount + "): ");
                            UserInput.readTerminal();
                            userResponse = UserInput.getInput().trim();
                            int choice;
                            try {
                                choice = Integer.parseInt(userResponse);
                            } catch (NumberFormatException e) {
                                System.out.println("Invalid input. Please enter a number between 1 and " + rightCount + ".");
                                continue;
                            }
                            if (choice < 1 || choice > rightCount) {
                                System.out.println("Choice out of range. Please enter a number from 1 to " + rightCount + ".");
                            } else if (usedRightIndices.contains(choice)) {
                                System.out.println("That option has already been used. Please select a different option for this match.");
                            } else {
                                usedRightIndices.add(choice);
                                String rightItem = matchQ.getRightOptions().get(choice - 1);
                                answerObj.addResponse(leftItem + " -> " + rightItem);
                                valid = true;
                            }
                        }
                    }
                }
                case MultipleChoice mcq -> {
                    int optionCount = mcq.getOptions().size();
                    while (!valid) {
                        if (optionCount > 1) {
                            System.out.print("Enter your choice(s) (you may select multiple options, separated by commas or spaces): ");
                        } else {
                            System.out.print("Enter your choice: ");
                        }
                        UserInput.readTerminal();
                        userResponse = UserInput.getInput().trim();
                        if (userResponse.isEmpty()) {
                            System.out.println("Answer cannot be empty. Please enter at least one option.");
                            continue;
                        }

                        // Split input by commas and whitespace
                        String[] tokens = userResponse.split("[,\\s]+");
                        Set<Integer> chosenIndices = new LinkedHashSet<>();  // use LinkedHashSet to avoid duplicates and maintain order
                        boolean parseError = false;
                        for (String token : tokens) {
                            if (token.isEmpty()) continue;
                            int choice;
                            // Support alphabetical inputs (e.g., 'A' or 'B')
                            if (token.length() == 1 && Character.isLetter(token.charAt(0))) {
                                char letter = Character.toUpperCase(token.charAt(0));
                                choice = letter - 'A' + 1;  // convert 'A' -> 1, 'B' -> 2, etc.
                            } else {
                                try {
                                    choice = Integer.parseInt(token);
                                } catch (NumberFormatException e) {
                                    parseError = true;
                                    break;
                                }
                            }
                            if (choice < 1 || choice > optionCount) {
                                parseError = true;
                                break;
                            }
                            chosenIndices.add(choice);
                        }
                        if (parseError || chosenIndices.isEmpty()) {
                            System.out.println("Invalid selection. Please enter option number(s) between 1 and " + optionCount + " (separated by commas if multiple).");
                            continue;
                        }

                        // All selections are valid
                        valid = true;
                        // Store each selected option's text as a separate response entry
                        for (int choice : chosenIndices) {
                            String optionText = mcq.getOptions().get(choice - 1);
                            answerObj.addResponse(optionText);
                        }
                    }
                }
                case TrueFalse _ -> {
                    while (!valid) {
                        System.out.print("Enter True or False: ");
                        UserInput.readTerminal();
                        userResponse = UserInput.getInput().trim();
                        if (userResponse.equalsIgnoreCase("true") || userResponse.equalsIgnoreCase("t") || userResponse.equals("1")) {
                            answerObj.addResponse("True");
                            valid = true;
                        } else if (userResponse.equalsIgnoreCase("false") || userResponse.equalsIgnoreCase("f") || userResponse.equals("2")) {
                            answerObj.addResponse("False");
                            valid = true;
                        } else {
                            System.out.println("Invalid input. Please type \"True\" or \"False\".");
                        }
                    }
                }
                case ShortAnswer saq -> {
                    int limit = saq.getCharacterLimit();

                    while (!valid) {
                        System.out.print("Your answer: ");
                        UserInput.readTerminal();
                        userResponse = UserInput.getInput().replace("\n", "");  // read the whole line; remove any newline char
                        if (userResponse.isEmpty()) {
                            System.out.println("Response cannot be empty. Please enter an answer.");
                        } else if (limit > 0 && userResponse.length() > limit) {
                            System.out.println("Your answer is too long. It must be " + limit + " characters or fewer. Please shorten your response.");
                        } else {
                            answerObj.addResponse(userResponse);
                            valid = true;
                        }
                    }
                }
                case Essay _ -> {
                    while (!valid) {
                        System.out.println("Enter your response (press Enter when finished):");
                        UserInput.readTerminal();
                        userResponse = UserInput.getInput().replace("\n", "");
                        if (userResponse.isEmpty()) {
                            System.out.println("Response cannot be empty. Please provide an answer (it can be as long as needed).");
                        } else {
                            answerObj.addResponse(userResponse);
                            valid = true;
                        }
                    }
                }
                case Date _ -> {
                    while (!valid) {
                        System.out.print("Enter a date (MM/DD/YYYY): ");
                        UserInput.readTerminal();
                        userResponse = UserInput.getInput().trim();
                        if (Date.isValidDate(userResponse)) {
                            answerObj.addResponse(userResponse);
                            valid = true;
                        } else {
                            System.out.println("Invalid date. Please enter a valid date in MM/DD/YYYY format.");
                        }
                    }
                }
                default -> {
                    while (!valid) {
                        System.out.print("Your answer: ");
                        UserInput.readTerminal();
                        userResponse = UserInput.getInput().trim();
                        if (!userResponse.isEmpty() && q.isValidAnswer(userResponse)) {
                            answerObj.addResponse(userResponse);
                            valid = true;
                        } else {
                            System.out.println("Invalid input. Please try again.");
                        }
                    }
                }
            }

            q.setUserResponse(answerObj);
        }

        System.out.print("\nDo you want to save your responses to a file? (yes/no): ");
        UserInput.readTerminal();
        String saveChoice = UserInput.getInput().trim().toLowerCase();
        if (saveChoice.startsWith("y")) {

            String fileName = surveyName + GetRandomFileName() + "_response.ser";

            File file = new File(fileName);

            // Save the current Survey object (with responses) via serialization
            try (FileOutputStream fos = new FileOutputStream(file);
                 ObjectOutputStream oos = new ObjectOutputStream(fos)) {
                oos.writeObject(this);  // serialize the entire Survey (questions + responses)
                oos.flush();
                this.setSaved(true);
                System.out.println("Survey responses saved successfully to \"" + fileName + "\".");
            } catch (IOException e) {
                System.err.println("Error: Unable to save responses to file. " + e.getMessage());
            }
        } else {
            System.out.println("Responses were **not** saved to a file.");
        }
    }

    private String GetRandomFileName() {
        // Define the desired date-time format
        DateTimeFormatter formatter = DateTimeFormatter.ofPattern("yyyyMMdd_HHmmss");

        // Obtain the current date and time
        LocalDateTime now = LocalDateTime.now();

        return now.format(formatter);
    }

    public void changeSurvey() {
        // List all questions with their number, type, and prompt summary
        System.out.println("\n--- Modify Survey \"" + surveyName + "\" ---");
        for (int i = 0; i < questionList.size(); i++) {
            Question q = questionList.get(i);
            System.out.println((i + 1) + ") (" + q.getQuestionType() + ") " + q.getQuestionPrompt());
        }
        int choice = UserInput.readInt("Enter the number of the question you want to modify: ", 1, questionList.size());
        Question selected = questionList.get(choice - 1);
        selected.modifyQuestion();
        // Mark as not saved since content has changed.
        this.saved = false;
        System.out.println("Question " + choice + " has been updated.");
    }

    public List<Answer> getResponses() {
        List<Answer> responses = new ArrayList<>();
        for (Question q : questionList) {
            Answer ans = q.getUserResponse();
            responses.add(Objects.requireNonNullElseGet(ans, Answer::new));
        }
        return responses;
    }

    private List<List<Answer>> getAllResponses() {
        // 1) List all files ending with "_Response.ser"
        File dir = new File(".");
        File[] respFiles = dir.listFiles((d, name) ->
                name.toLowerCase().endsWith("_response.ser") && name.startsWith(this.surveyName));
        // 2) Create an outer list with one inner List<Answer> per question
        int qCount = questionList.size();
        List<List<Answer>> all = new ArrayList<>();
        for (int i = 0; i < qCount; i++) {
            all.add(new ArrayList<>());  // prepare a list to collect answers for question i
        }
        if (respFiles != null) {
            for (File rf : respFiles) {
                try (FileInputStream fis = new FileInputStream(rf.getName());
                     ObjectInputStream ois = new ObjectInputStream(fis)) {
                    Test surveyResp = (Test) ois.readObject();
                    // Suppose SurveyResponse holds a List<Answer> in order of questions
                    List<Answer> single = surveyResp.getCorrectAnswer();
                    for (int i = 0; i < qCount; i++) {
                        all.get(i).add(single.get(i));
                    }
                } catch (Exception e) {
                    System.out.println("Failed to read response file: " + rf.getName());
                }
            }
        }
        return all;
    }


    public void tabulate() {
        System.out.println("\n--- Tabulation for \"" + surveyName + "\" ---\n");

        List<List<Answer>> questionResponses = getAllResponses();

        for (int i = 0; i < questionList.size(); i++) {
            Question q = questionList.get(i);
            System.out.println("Question " + (i+1) + ": " + q.getQuestionPrompt());

            List<Answer> responses = questionResponses.get(i);
            if (q instanceof TrueFalse || q instanceof MultipleChoice ||
                    q instanceof ShortAnswer || q instanceof Date) {
                // Frequency map: answer text -> count
                Map<String, Integer> freq = new LinkedHashMap<>();
                for (Answer a : responses) {
                    // Each Answer may have multiple strings (for MCQ with multiple selections)
                    for (String respText : a.getResponse()) {
                        freq.put(respText, freq.getOrDefault(respText, 0) + 1);
                    }
                }
                // Print frequencies for each key
                for (Map.Entry<String, Integer> entry : freq.entrySet()) {
                    System.out.printf("  %s: %d%n", entry.getKey(), entry.getValue());
                }
                if (freq.isEmpty()) {
                    System.out.println("  (No responses yet)");
                }
            }
            else if (q instanceof Matching) {
                // Count identical match permutations
                Map<String, Integer> comboFreq = new LinkedHashMap<>();
                for (Answer a : responses) {
                    // Each Answer for matching stores strings like "Left -> Right"
                    // We can join those with commas to make a single key
                    List<String> pairs = a.getResponse();
                    Collections.sort(pairs); // ensure consistent order
                    String comboKey = String.join(", ", pairs);
                    comboFreq.put(comboKey, comboFreq.getOrDefault(comboKey, 0) + 1);
                }
                for (Map.Entry<String, Integer> entry : comboFreq.entrySet()) {
                    System.out.printf("  [%s]: %d%n", entry.getKey(), entry.getValue());
                }
                if (comboFreq.isEmpty()) {
                    System.out.println("  (No responses yet)");
                }
            }
            else if (q instanceof Essay) {
                // List each essay response
                if (responses.isEmpty()) {
                    System.out.println("  (No responses yet)");
                } else {
                    int rnum = 1;
                    for (Answer a : responses) {
                        // Each Answer for essay contains one long string
                        String essayText = a.getResponse().get(0);
                        System.out.printf("  Response %d: %s%n", rnum++, essayText);
                    }
                }
            }
            System.out.println();  // blank line before next question
        }
    }

}
