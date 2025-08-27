#include <stdio.h>
#include <stdlib.h>
#include <errno.h>
#include <string.h>

// Function to print the first n lines of a given file or standard input
void print_head(FILE *file, int n) {
    int c;
    int line_count = 0;
    
    while ((c = getc(file)) != EOF && line_count < n) {
        putc(c, stdout);
        if (c == '\n') {
            line_count++;
        }
    }
}

// Function to read n lines from stdin and print them
void read_and_print_stdin(int n) {
    char buffer[1024];
    char *lines[n];
    int line_count = 0;

    // Allocate memory for storing lines
    for (int i = 0; i < n; i++) {
        lines[i] = malloc(1024 * sizeof(char));
        if (!lines[i]) {
            fprintf(stderr, "Error: Memory allocation failed\n");
            exit(1);
        }
    }
    
    // Read lines from stdin
    while (line_count < n && fgets(buffer, sizeof(buffer), stdin)) {
        strcpy(lines[line_count], buffer);
        line_count++;
    }

    // Print collected lines
    for (int i = 0; i < line_count; i++) {
        fputs(lines[i], stdout);
        free(lines[i]); // Free allocated memory
    }
}

int main(int argc, char *argv[]) {
    int n = 10; // Default number of lines to print
    char *filename = NULL;
    FILE *file = stdin;

    // Parse command line arguments
    for (int i = 1; i < argc; i++) {
        if (argv[i][0] == '-') {
            if (argv[i][1] == 'n') {
                if (i + 1 < argc) {
                    n = atoi(argv[++i]);
                } else {
                    fprintf(stderr, "Error: Missing argument for -n\n");
                    return 1;
                }
            } else {
                n = atoi(&argv[i][1]);
                if (n <= 0) {
                    fprintf(stderr, "Error: Invalid number of lines %s\n", &argv[i][1]);
                    return 1;
                }
            }
        } else {
            filename = argv[i];
        }
    }

    // If no filename is provided, read from stdin
    if (!filename) {
        read_and_print_stdin(n);
        return 0;
    }

    // If a file name is provided, open the file
    file = fopen(filename, "r");
    if (!file) {
        perror("Error opening file");
        return 1;
    }

    // Print the first n lines of the file
    print_head(file, n);

    // Close the file
    fclose(file);

    return 0;
}
