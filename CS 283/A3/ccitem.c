#include "cc.h"
#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <fcntl.h>
#include <sys/file.h>

int main(int argc, char *argv[]) {
    if (argc != 2) {
        fprintf(stderr, "Usage: %s itemnumber\n", argv[0]);
        exit(1);
    }

    int fd = open(DATABASE, O_RDONLY);
    if (fd == -1) {
        perror(DATABASE);
        exit(1);
    }

    if (flock(fd, LOCK_SH) == -1) {
        perror("flock");
        close(fd);
        exit(1);
    }

    int itemnumber = atoi(argv[1]);
    ITEM item;

    lseek(fd, itemnumber * sizeof(ITEM), SEEK_SET);
    if (read(fd, &item, sizeof(ITEM)) != sizeof(ITEM)) {
        fprintf(stderr, "Item %d not found\n", itemnumber);
        close(fd);
        exit(1);
    }

    printf("Item %d\n", itemnumber);
    printf(" Maker: %s\n", item.maker);
    printf(" CPU: %s\n", item.cpu);
    printf(" Year: %d\n", item.year);
    printf(" Description: %s\n", item.description);

    if (flock(fd, LOCK_UN) == -1) {
        perror("flock");
    }
    close(fd);

    return 0;
}

