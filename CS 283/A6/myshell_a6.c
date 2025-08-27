
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#include <sys/wait.h>
#include <fcntl.h>

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

void execute_command(char **args, char *input_file, char *output_file, int append) {
    pid_t pid, wpid;
    int status;

    pid = fork();
    if (pid == 0) { 
        // Child process

        // Input redirection
        if (input_file != NULL) {
            int fd0 = open(input_file, O_RDONLY);
            if (fd0 < 0) {
                perror("Input file open error");
                exit(EXIT_FAILURE);
            }
            dup2(fd0, STDIN_FILENO);
            close(fd0);
        }

        // Output redirection
        if (output_file != NULL) {
            int fd1;
            if (append) {
                fd1 = open(output_file, O_WRONLY | O_CREAT | O_APPEND, 0644);
            } else {
                fd1 = open(output_file, O_WRONLY | O_CREAT | O_TRUNC, 0644);
            }
            if (fd1 < 0) {
                perror("Output file open error");
                exit(EXIT_FAILURE);
            }
            dup2(fd1, STDOUT_FILENO);
            close(fd1);
        }

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
    char *input_file = NULL;
    char *output_file = NULL;
    int append = 0;
    int arg_count = 0;
    char *token;

    token = strtok(input, " \t\r\n\a");
    while (token != NULL) {
        if (strcmp(token, "<") == 0) {
            token = strtok(NULL, " \t\r\n\a");
            input_file = token;
        } else if (strcmp(token, ">>") == 0) {
            token = strtok(NULL, " \t\r\n\a");
            output_file = token;
            append = 1;
        } else if (strcmp(token, ">") == 0) {
            token = strtok(NULL, " \t\r\n\a");
            output_file = token;
        } else {
            args[arg_count] = token;
            arg_count++;
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
        execute_command(args, input_file, output_file, append);
    }
}

int main() {
    char input[MAX_INPUT_SIZE];

    while (1) {
        printf("> ");
        if (fgets(input, sizeof(input), stdin) == NULL) {
            break;
        }

        if (strcmp(input, "exit\n") == 0) {
            break;
        }

        parse_and_execute(input);
    }

    return 0;
}
