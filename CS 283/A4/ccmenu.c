#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <string.h>
#include <sys/wait.h>

void run_program(char *program, char *args[]) {
    pid_t pid = fork();

    if (pid < 0) {
        perror("fork");
        exit(1);
    } else if (pid == 0) {
        execvp(program, args);
        perror("execvp");
        exit(1);
    } else {
        wait(NULL);
    }
}

void print_menu() {
    printf("Database Application Menu:\n");
    printf("1. ccitem - Display an item by item number\n");
    printf("2. cclist - List all items\n");
    printf("3. ccadd - Add a new item\n");
    printf("4. ccdel - Delete an item by item number\n");
    printf("5. ccmatch - Search for items by substring\n");
    printf("6. ccyear - List items within a year range\n");
    printf("7. ccedit - Edit an item by item number\n");
    printf("8. Quit\n");
}

int main() {
    char choice[10];
    char itemnumber[10], maker[50], cpu[50], year[10], description[100];
    char startyear[10], endyear[10], substring[50];

    while (1) {
        print_menu();
        printf("Enter your choice: ");
        fgets(choice, sizeof(choice), stdin);
        choice[strcspn(choice, "\n")] = 0;

        if (strcmp(choice, "1") == 0) {
            printf("Enter item number: ");
            fgets(itemnumber, sizeof(itemnumber), stdin);
            itemnumber[strcspn(itemnumber, "\n")] = 0;
            char *args[] = {"./ccitem", itemnumber, NULL};
            run_program("./ccitem", args);
        } else if (strcmp(choice, "2") == 0) {
            char *args[] = {"./cclist", NULL};
            run_program("./cclist", args);
        } else if (strcmp(choice, "3") == 0) {
            printf("Enter -a or item number: ");
            fgets(itemnumber, sizeof(itemnumber), stdin);
            itemnumber[strcspn(itemnumber, "\n")] = 0;
            printf("Enter maker: ");
            fgets(maker, sizeof(maker), stdin);
            maker[strcspn(maker, "\n")] = 0;
            printf("Enter CPU: ");
            fgets(cpu, sizeof(cpu), stdin);
            cpu[strcspn(cpu, "\n")] = 0;
            printf("Enter year: ");
            fgets(year, sizeof(year), stdin);
            year[strcspn(year, "\n")] = 0;
            printf("Enter description: ");
            fgets(description, sizeof(description), stdin);
            description[strcspn(description, "\n")] = 0;
            char *args[] = {"./ccadd", itemnumber, maker, cpu, year, description, NULL};
            run_program("./ccadd", args);
        } else if (strcmp(choice, "4") == 0) {
            printf("Enter item number: ");
            fgets(itemnumber, sizeof(itemnumber), stdin);
            itemnumber[strcspn(itemnumber, "\n")] = 0;
            char *args[] = {"./ccdel", itemnumber, NULL};
            run_program("./ccdel", args);
        } else if (strcmp(choice, "5") == 0) {
            printf("Enter substring to search: ");
            fgets(substring, sizeof(substring), stdin);
            substring[strcspn(substring, "\n")] = 0;
            char *args[] = {"./ccmatch", substring, NULL};
            run_program("./ccmatch", args);
        } else if (strcmp(choice, "6") == 0) {
            printf("Enter start year: ");
            fgets(startyear, sizeof(startyear), stdin);
            startyear[strcspn(startyear, "\n")] = 0;
            printf("Enter end year: ");
            fgets(endyear, sizeof(endyear), stdin);
            endyear[strcspn(endyear, "\n")] = 0;
            char *args[] = {"./ccyear", startyear, endyear, NULL};
            run_program("./ccyear", args);
        } else if (strcmp(choice, "7") == 0) {
            printf("Enter item number: ");
            fgets(itemnumber, sizeof(itemnumber), stdin);
            itemnumber[strcspn(itemnumber, "\n")] = 0;
            char *args[] = {"./ccedit", itemnumber, NULL};
            run_program("./ccedit", args);
        } else if (strcmp(choice, "8") == 0) {
            printf("Quitting...\n");
            break;
        } else {
            printf("Invalid choice. Please try again.\n");
        }
    }

    return 0;
}
