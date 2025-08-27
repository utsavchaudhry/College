#include "cc.h"
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#include <fcntl.h>
#include <sys/file.h>
#include <sys/stat.h>

int main(int argc, char *argv[]) {
    if (argc != 2 && argc != 6) {
        fprintf(stderr, "Usage: %s [-a] maker cpu year description\n", argv[0]);
        fprintf(stderr, "       %s itemnumber maker cpu year description\n", argv[0]);
        exit(1);
    }

    int fd = open(DATABASE, O_RDWR | O_CREAT, 0644);
    if (fd == -1) {
        perror(DATABASE);
        exit(1);
    }

    if (flock(fd, LOCK_EX) == -1) {
        perror("flock");
        close(fd);
        exit(1);
    }

    int itemnumber;
    if (strcmp(argv[1], "-a") == 0) {
        struct stat statbuf;
        if (fstat(fd, &statbuf) == -1) {
            perror("fstat");
            close(fd);
            exit(1);
        }
        itemnumber = statbuf.st_size / sizeof(ITEM);
    } else {
        itemnumber = atoi(argv[1]);
    }

    ITEM item;
    memset(&item, 0, sizeof(ITEM));  // Ensure the structure is zero-initialized

    // Copy the strings and ensure they are null-terminated
    strncpy(item.maker, argv[2], sizeof(item.maker) - 1);
    strncpy(item.cpu, argv[3], sizeof(item.cpu) - 1);
    item.year = atoi(argv[4]);
    strncpy(item.description, argv[5], sizeof(item.description) - 1);

    lseek(fd, itemnumber * sizeof(ITEM), SEEK_SET);
    if (write(fd, &item, sizeof(ITEM)) != sizeof(ITEM)) {
        perror("write");
        close(fd);
        exit(1);
    }

    if (flock(fd, LOCK_UN) == -1) {
        perror("flock");
    }
    close(fd);

    return 0;
}

