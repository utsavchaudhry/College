#include "cc.h"
#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <fcntl.h>
#include <sys/file.h>

int main(int argc, char *argv[]) {
    if (argc != 3) {
        fprintf(stderr, "Usage: %s startyear endyear\n", argv[0]);
        exit(1);
    }

    int startyear = atoi(argv[1]);
    int endyear = atoi(argv[2]);

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

    ITEM item;
    int itemnumber = 0;
    while (read(fd, &item, sizeof(ITEM)) == sizeof(ITEM)) {
        if (item.year >= startyear && item.year <= endyear) {
            printf("Item %d\n", itemnumber);
            printf(" Maker: %s\n", item.maker);
            printf(" CPU: %s\n", item.cpu);
            printf(" Year: %d\n", item.year);
            printf(" Description: %s\n\n", item.description);
        }
        itemnumber++;
    }

    if (flock(fd, LOCK_UN) == -1) {
        perror("flock");
    }
    close(fd);

    return 0;
}

