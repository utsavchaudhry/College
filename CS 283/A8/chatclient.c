#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#include <arpa/inet.h>
#include <netdb.h>
#include <pthread.h>

#define BUFFER_SIZE 1024

int sockfd;

void *send_message(void *arg) {
    char message[BUFFER_SIZE];
    while (1) {
        fgets(message, BUFFER_SIZE, stdin);
        if (strcmp(message, "exit\n") == 0) {
            printf("Exiting chat...\n");
            close(sockfd);
            exit(0);
        }
        write(sockfd, message, strlen(message));
    }
}

void *receive_message(void *arg) {
    char message[BUFFER_SIZE];
    int n;
    while ((n = read(sockfd, message, BUFFER_SIZE)) > 0) {
        message[n] = '\0';
        printf("%s", message);
    }
    printf("Server disconnected. Exiting...\n");
    close(sockfd);
    exit(0);
}

int main(int argc, char *argv[]) {
    struct sockaddr_in remote;
    struct hostent *remaddr;
    pthread_t send_thread, receive_thread;

    if (argc != 2) {
        printf("Usage: %s <server_hostname>\n", argv[0]);
        return -1;
    }

    sockfd = socket(AF_INET, SOCK_STREAM, 0);
    if (sockfd < 0) {
        perror("socket");
        return -1;
    }
    printf("Got socket\n");

    remaddr = gethostbyname(argv[1]);
    if (remaddr == NULL) {
        perror("gethostbyname");
        return -1;
    }
    printf("Got address: %x\n", *(int *)(remaddr->h_addr));

    memmove(&remote.sin_addr.s_addr, remaddr->h_addr, remaddr->h_length);
    remote.sin_family = AF_INET;
    remote.sin_port = htons(2024);

    if (connect(sockfd, (struct sockaddr *)&remote, sizeof(struct sockaddr_in)) < 0) {
        perror("connect");
        return -1;
    }
    printf("Got connection\n");

    pthread_create(&send_thread, NULL, send_message, NULL);
    pthread_create(&receive_thread, NULL, receive_message, NULL);

    pthread_join(send_thread, NULL);
    pthread_join(receive_thread, NULL);

    return 0;
}

