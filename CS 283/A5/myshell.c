#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#include <sys/wait.h>

#define MAX_INPUT_SIZE 1024
#define MAX_ARG_SIZE 100

void cd_command(char *path) {
    if (path == NULL) {
        fprintf(stderr, "Expected argument to \"cd\"\n");
    } else {
        if (chdir(path) != 0) {
            perror("cd");
        }
    }
}

void execute_command(char **args) {
    pid_t pid, wpid;
    int status;

    pid = fork();
    if (pid == 0) { 
        // Child process
        if (execvp(args[0], args) == -1) {
            perror("exec");
        }
        exit(EXIT_FAILURE);
    } else if (pid < 0) { 
        // Error forking
        perror("fork");
    } else { 
        // Parent process
        do {
            wpid = waitpid(pid, &status, WUNTRACED);
        } while (!WIFEXITED(status) && !WIFSIGNALED(status));
    }
}

void parse_and_execute(char *input) {
    char *args[MAX_ARG_SIZE];
    char *token;
    int arg_count = 0;

    token = strtok(input, " \t\r\n\a");
    while (token != NULL) {
        args[arg_count] = token;
        arg_count++;

        if (arg_count >= MAX_ARG_SIZE) {
            fprintf(stderr, "Too many arguments\n");
            return;
        }

        token = strtok(NULL, " \t\r\n\a");
    }
    args[arg_count] = NULL;

    if (arg_count == 0) {
        return;
    }

    if (strcmp(args[0], "cd") == 0) {
        cd_command(args[1]);
    } else {
        execute_command(args);
    }
}

int main() {
    char input[MAX_INPUT_SIZE];

    while (1) {
        printf("> ");
        if (fgets(input, sizeof(input), stdin) == NULL) {
            perror("fgets");
            exit(EXIT_FAILURE);
        }

        parse_and_execute(input);
    }

    return 0;
}
