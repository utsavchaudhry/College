#include "cc.h"
#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <fcntl.h>
#include <sys/file.h>

int main(void) {
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
        printf("Item %d\n", itemnumber++);
        printf(" Maker: %s\n", item.maker);
        printf(" CPU: %s\n", item.cpu);
        printf(" Year: %d\n", item.year);
        printf(" Description: %s\n\n", item.description);
    }

    if (flock(fd, LOCK_UN) == -1) {
        perror("flock");
    }
    close(fd);

    return 0;
}

