import java.io.*;
import java.util.Arrays;
import java.util.List;

public class Driver {

    private Survey currentSurvey;
    private Test currentTest;

    public Driver() {
        currentSurvey = null;
        currentTest   = null;
    }

    public void mainMenu() {
        boolean running = true;
        while (running) {
            System.out.println("\n=== Main Menu ===");
            System.out.println("1) Survey");
            System.out.println("2) Test");
            System.out.println("3) Quit");

            int choice = UserInput.readInt("Select an option (1-3): ", 1, 3);
            switch (choice) {
                case 1:
                    surveyMenu();
                    break;
                case 2:
                    testMenu();
                    break;
                case 3:
                    // Warn if unsaved Survey or Test exists
                    if ((currentSurvey != null && !currentSurvey.isSaved()) ||
                            (currentTest   != null && !currentTest.isSaved())) {
                        boolean quit = UserInput.confirm(
                                "You have unsaved work. Are you sure you want to quit? (Y/N): ");
                        if (!quit) break;
                    }
                    System.out.println("Goodbye!");
                    running = false;
                    break;
            }
        }
    }

    private void surveyMenu() {
        boolean back = false;
        while (!back) {
            System.out.println("\n--- Survey Menu ---");
            System.out.println("1) Create a new Survey");
            System.out.println("2) Display the current Survey");
            System.out.println("3) Load a Survey from file");
            System.out.println("4) Save the current Survey to file");
            System.out.println("5) Take the current Survey");
            System.out.println("6) Modify the current Survey");
            System.out.println("7) Tabulate the current Survey");
            System.out.println("8) Return to Main Menu");

            int choice = UserInput.readInt("Select an option (1-8): ", 1, 8);
            switch (choice) {
                case 1:
                    // Create
                    if (currentSurvey != null && !currentSurvey.isSaved()) {
                        boolean ok = UserInput.confirm(
                                "Unsaved survey exists. Discard and create new? (Y/N): ");
                        if (!ok) break;
                    }
                    currentSurvey = new Survey();
                    currentSurvey.createSurvey();
                    break;
                case 2:
                    // Display
                    if (currentSurvey == null) {
                        System.out.println("No survey loaded. Create or load first.");
                    } else {
                        System.out.println("\n--- Survey: " + currentSurvey.getSurveyName() + " ---");
                        currentSurvey.displaySurvey();
                    }
                    break;
                case 3:
                    // Load
                    if (currentSurvey != null && !currentSurvey.isSaved()) {
                        boolean ok = UserInput.confirm(
                                "Unsaved survey exists. Discard and load? (Y/N): ");
                        if (!ok) break;
                    }
                    loadSurvey();
                    break;
                case 4:
                    // Save
                    if (currentSurvey == null) {
                        System.out.println("No survey to save.");
                    } else {
                        currentSurvey.storeSurvey();
                    }
                    break;
                case 5:
                    // Take
                    if (currentSurvey == null) {
                        System.out.println("No survey loaded.");
                    } else {
                        currentSurvey.takeSurvey();
                    }
                    break;
                case 6:
                    // Modify
                    if (currentSurvey == null) {
                        System.out.println("No survey loaded.");
                    } else {
                        currentSurvey.changeSurvey();
                    }
                    break;
                case 7:
                    // Tabulate
                    if (currentSurvey == null) {
                        System.out.println("No survey loaded.");
                    } else {
                        currentSurvey.tabulate();
                    }
                    break;
                case 8:
                    back = true;
                    break;
            }
        }
    }

    private void testMenu() {
        boolean back = false;
        while (!back) {
            System.out.println("\n--- Test Menu ---");
            System.out.println("1) Create a new Test");
            System.out.println("2) Display Test WITHOUT correct answers");
            System.out.println("3) Display Test WITH correct answers");
            System.out.println("4) Load a Test from file");
            System.out.println("5) Save the current Test to file");
            System.out.println("6) Take the current Test");
            System.out.println("7) Modify the current Test");
            System.out.println("8) Tabulate the current Test");
            System.out.println("9) Grade the current Test");
            System.out.println("10) Return to Main Menu");

            int choice = UserInput.readInt("Select an option (1-10): ", 1, 10);
            switch (choice) {
                case 1:
                    // Create
                    if (currentTest != null && !currentTest.isSaved()) {
                        boolean ok = UserInput.confirm(
                                "Unsaved test exists. Discard and create new? (Y/N): ");
                        if (!ok) break;
                    }
                    currentTest = new Test();
                    currentTest.create();
                    break;
                case 2:
                    // Display without answers
                    if (currentTest == null) {
                        System.out.println("No test loaded.");
                    } else {
                        currentTest.display();
                    }
                    break;
                case 3:
                    // Display with correct answers
                    if (currentTest == null) {
                        System.out.println("No test loaded.");
                    } else {
                        currentTest.displayWithAnswers();
                    }
                    break;
                case 4:
                    // Load
                    if (currentTest != null && !currentTest.isSaved()) {
                        boolean ok = UserInput.confirm(
                                "Unsaved test exists. Discard and load? (Y/N): ");
                        if (!ok) break;
                    }
                    loadTest();
                    break;
                case 5:
                    // Save
                    if (currentTest == null) {
                        System.out.println("No test to save.");
                    } else {
                        currentTest.store();
                    }
                    break;
                case 6:
                    // Take
                    if (currentTest == null) {
                        System.out.println("No test loaded.");
                    } else {
                        currentTest.take();
                    }
                    break;
                case 7:
                    // Modify
                    if (currentTest == null) {
                        System.out.println("No test loaded.");
                    } else {
                        currentTest.modify();
                    }
                    break;
                case 8:
                    // Tabulate
                    if (currentTest == null) {
                        System.out.println("No test loaded.");
                    } else {
                        currentTest.tabulate();
                    }
                    break;
                case 9:
                    // Grade

                    loadTestWithResponse();

                    if (currentTest == null) {
                        System.out.println("No test loaded.");
                    } else {
                        double score = currentTest.grade();
                        System.out.printf("You received %.2f%% on this test.%n", score);
                    }
                    break;
                case 10:
                    back = true;
                    break;
            }
        }
    }

    private void loadSurvey() {
        // 1) List all available .ser files that do NOT contain "Response"
        File dir = new File(".");
        File[] files = dir.listFiles((d, name) ->
                name.toLowerCase().endsWith(".ser") && !name.toLowerCase().contains("response"));
        if (files == null || files.length == 0) {
            System.out.println("No saved surveys found.");
            return;
        }
        // 2) Print the filenames with indices
        for (int i = 0; i < files.length; i++) {
            System.out.println((i + 1) + ") " + files[i].getName());
        }
        // 3) Ask user to select by number or type a name
        String sel = UserInput.readNonEmptyLine("Enter number or filename to load: ");
        File chosen = null;
        if (sel.matches("\\d+")) {
            int idx = Integer.parseInt(sel) - 1;
            if (idx >= 0 && idx < files.length) {
                chosen = files[idx];
            }
        } else {
            // Attempt to match exactly the filename
            for (File f : files) {
                if (f.getName().equals(sel)) {
                    chosen = f;
                    break;
                }
            }
        }
        if (chosen == null) {
            System.out.println("Invalid selection. Aborting load.");
            return;
        }
        // 4) Deserialize into currentSurvey
        try (FileInputStream fis = new FileInputStream(chosen.getName());
             ObjectInputStream ois = new ObjectInputStream(fis)) {
            currentSurvey = (Survey) ois.readObject();
            currentSurvey.setSaved(true);
            System.out.println("Loaded survey: " + currentSurvey.getSurveyName());
        } catch (IOException | ClassNotFoundException e) {
            System.out.println("Failed to load survey: " + e.getLocalizedMessage());
        }
    }

    private void loadTest() {
        // Very similar to loadSurvey(), except casting to Test
        File dir = new File(".");
        File[] files = dir.listFiles((d, name) ->
                name.toLowerCase().endsWith(".ser") && !name.toLowerCase().contains("response"));
        if (files == null || files.length == 0) {
            System.out.println("No saved tests found.");
            return;
        }
        for (int i = 0; i < files.length; i++) {
            System.out.println((i + 1) + ") " + files[i].getName());
        }
        String sel = UserInput.readNonEmptyLine("Enter number or filename to load: ");
        File chosen = null;
        if (sel.matches("\\d+")) {
            int idx = Integer.parseInt(sel) - 1;
            if (idx >= 0 && idx < files.length) {
                chosen = files[idx];
            }
        } else {
            for (File f : files) {
                if (f.getName().equals(sel)) {
                    chosen = f;
                    break;
                }
            }
        }
        if (chosen == null) {
            System.out.println("Invalid selection. Aborting load.");
            return;
        }
        try (FileInputStream fis = new FileInputStream(chosen.getName());
             ObjectInputStream ois = new ObjectInputStream(fis)) {
            currentTest = (Test) ois.readObject();
            currentTest.setSaved(true);
            System.out.println("Loaded test: " + currentTest.getSurveyName());
        } catch (IOException | ClassNotFoundException e) {
            System.out.println("Failed to load test: " + e.getLocalizedMessage());
        }
    }

    private void loadTestWithResponse() {
        // Very similar to loadSurvey(), except casting to Test
        File dir = new File(".");
        File[] files = dir.listFiles((d, name) ->
                name.toLowerCase().endsWith(".ser") && name.toLowerCase().contains("response"));
        if (files == null || files.length == 0) {
            System.out.println("No saved test response found.");
            return;
        }
        for (int i = 0; i < files.length; i++) {
            System.out.println((i + 1) + ") " + files[i].getName());
        }
        String sel = UserInput.readNonEmptyLine("Enter number or filename to load: ");
        File chosen = null;
        if (sel.matches("\\d+")) {
            int idx = Integer.parseInt(sel) - 1;
            if (idx >= 0 && idx < files.length) {
                chosen = files[idx];
            }
        } else {
            for (File f : files) {
                if (f.getName().equals(sel)) {
                    chosen = f;
                    break;
                }
            }
        }
        if (chosen == null) {
            System.out.println("Invalid selection. Aborting load.");
            return;
        }
        try (FileInputStream fis = new FileInputStream(chosen.getName());
             ObjectInputStream ois = new ObjectInputStream(fis)) {
            currentTest = (Test) ois.readObject();
            currentTest.setSaved(true);
            System.out.println("Loaded test: " + currentTest.getSurveyName());
        } catch (IOException | ClassNotFoundException e) {
            System.out.println("Failed to load test: " + e.getLocalizedMessage());
        }
    }

    public static void main(String[] args) {
        new Driver().mainMenu();
    }
}
