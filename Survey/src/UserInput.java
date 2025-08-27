import java.util.Scanner;

public class UserInput {

    private static Scanner scanner = new Scanner(System.in); // (initialized once).
    // Temporarily holds the last line of input read (not always needed since methods return values directly).
    private static String input;

    public static void readTerminal() {
        input = scanner.nextLine();
    }

    public static String getInput() {
        if (input == null) {
            return "";
        }
        return input.trim();
    }

    public static boolean validateInput() {
        return input != null && !input.trim().isEmpty();
    }

    public static boolean confirm(String message) {
        while (true) {
            System.out.print(message);
            readTerminal();
            if (!validateInput()) {
                // If nothing entered, loop again.
                continue;
            }
            String resp = getInput().toLowerCase();
            if (resp.startsWith("y")) {
                return true;
            } else if (resp.startsWith("n")) {
                return false;
            } else {
                System.out.println("Please enter Y or N.");
                // Loop again on invalid input.
            }
        }
    }

    public static int readInt(String prompt, int min, int max) {
        while (true) {
            System.out.print(prompt);
            readTerminal();
            if (!validateInput()) {
                System.out.println("Input cannot be empty. Please enter a number between " + min + " and " + max + ".");
                continue;
            }
            String line = getInput();
            try {
                int value = Integer.parseInt(line);
                if (value < min || value > max) {
                    System.out.println("Please enter a number between " + min + " and " + max + ".");
                } else {
                    return value;
                }
            } catch (NumberFormatException e) {
                System.out.println("Please enter a valid integer.");
            }
        }
    }

    public static String readNonEmptyLine(String prompt) {
        while (true) {
            System.out.print(prompt);
            readTerminal();
            if (validateInput()) {
                return getInput();
            } else {
                System.out.println("Input cannot be empty. Please try again.");
            }
        }
    }
}
