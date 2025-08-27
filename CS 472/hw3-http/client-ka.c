#include "http.h"

#include <sys/socket.h>
#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <string.h>
#include <time.h>   // Include time.h for timing functions

#define  BUFF_SZ            1024
#define  MAX_REOPEN_TRIES   5

char recv_buff[BUFF_SZ];

char *generate_cc_request(const char *host, int port, const char *path){
	static char req[512] = {0};
	int offset = 0;
	
    //note that all paths should start with "/" when passed in
	offset += sprintf((char *)(req + offset),"GET %s HTTP/1.1\r\n", path);
	offset += sprintf((char *)(req + offset),"Host: %s\r\n", host);
	offset += sprintf((char *)(req + offset),"Connection: Keep-Alive\r\n");
	offset += sprintf((char *)(req + offset),"\r\n");

	printf("DEBUG: %s", req);
	return req;
}


void print_usage(char *exe_name){
    fprintf(stderr, "Usage: %s <hostname> <port> <path...>\n", exe_name);
    fprintf(stderr, "Using default host %s, port %d  and path [\\]\n", DEFAULT_HOST, DEFAULT_PORT); 
}

int reopen_socket(const char *host, uint16_t port) {
    int sock = 0;

    //----------------------------------------------------------------------------
    // Implement a loop that attempts a certain number of times to open a 
    // socket with the host and port that is passed in as parameters.
    //----------------------------------------------------------------------------

    for (int i = 0; i < MAX_REOPEN_TRIES; i++) {
        sock = socket_connect(host, port);
        if (sock >= 0) {
            // Successful connection
            return sock;
        }
        // Optionally, wait or log before retrying
    }

    // If we fall out of the loop, we are unable to connect
    return -1;
}

int server_connect(const char *host, uint16_t port){
    return socket_connect(host, port);
}

void server_disconnect(int sock){
    close(sock);
}

int submit_request(int sock, const char *host, uint16_t port, char *resource){
    int sent_bytes = 0; 

    const char *req = generate_cc_request(host, port, resource);
    int send_sz = strlen(req);

    // This is the initial send, this is where the send will fail with 
    // Keep-Alive if the server closed the socket, sent_bytes will have
    // the number of bytes sent, which should equal send_sz if all went
    // well.  If sent_bytes < 0, this indicates a send error, so we will
    // need to reconnect to the server.
    sent_bytes = send(sock, req, send_sz,0);

    //we have a socket error, perhaps the server closed it, lets try to reopen
    //the socket
    if (sent_bytes < 0){
        //----------------------------------------------------------------------------
        // Reimplement the retry logic to reopen the socket
        //----------------------------------------------------------------------------

        // Attempt to reopen the socket
        sock = reopen_socket(host, port);
        if (sock < 0) {
            // Could not reopen the socket
            return sock;
        }

        // Reissue the send again with the new socket
        sent_bytes = send(sock, req, send_sz, 0);
    }

    //This should not happen, but just checking if we didnt send everything and 
    //handling appropriately 
    if(sent_bytes != send_sz){
        if(sent_bytes < 0)
            perror("send failed after reconnect attempt");
        else
            fprintf(stderr, "Sent bytes %d is not equal to sent size %d\n", sent_bytes, send_sz);
        
        close(sock);
        return -1;
    }

    int bytes_recvd = 0;    //used to track amount of data received on each recv() call
    int total_bytes = 0;    //used to accumulate the total number of bytes across all recv() calls
    
    //do the first recv
    bytes_recvd = recv(sock, recv_buff, sizeof(recv_buff),0);
    if(bytes_recvd < 0) {
        perror("initial receive failed");
        close(sock);
        return -1;
    }

    //remember the first receive we just did has the HTTP header, and likely some body
    //data.  We need to determine how much data we expect

    //--------------------------------------------------------------------------------
    // Get the header length
    //--------------------------------------------------------------------------------
    int header_len = 0;     // Change this to get the header length as per the directions below

    // 1. Use the get_http_header_len() function to set the header_len variable.
    header_len = get_http_header_len(recv_buff, bytes_recvd);
    // 2. Check if header_len is negative
    if (header_len < 0) {
        // a. Close the socket
        close(sock);
        // b. Return -1 to exit this function
        return -1;
    }
    
    //--------------------------------------------------------------------------------
    // Get the content length
    //--------------------------------------------------------------------------------
    int content_len = 0;    // Change this to get the content length

    // 1. Use the get_http_content_len() function to set the content_len variable.
    content_len = get_http_content_len(recv_buff, header_len);
    // No error checking is needed as per the instructions

    //--------------------------------------------------------------------------------
    // TODO:  Make sure you understand the calculations below
    //
    // You do not have to write any code, but add to this comment your thoughts on 
    // what the following 2 lines of code do to track the amount of data received
    // from the server
    //
    // MY ANSWER:
    // The first line calculates the amount of body data received in the initial recv.
    // It subtracts the header length from the total bytes received to get the initial
    // amount of content data (body) that has already been received.
    //
    // The second line calculates how many bytes of content data are remaining to be
    // received by subtracting the initial data received from the total content length.
    // This tells us how much more data we need to read from the socket to get the full
    // content as specified by the Content-Length header.
    //--------------------------------------------------------------------------------
    int initial_data =  bytes_recvd - header_len;
    int bytes_remaining = content_len - initial_data;


    //This loop keeps going until bytes_remaining is essentially zero, to be more
    //defensive an prevent an infinite loop, i have it set to keep looping as long
    //as bytes_remaining is positive
    while(bytes_remaining > 0){
        
        //-----------------------------------------------------------------------------
        // Continue receiving data from the server
        //-----------------------------------------------------------------------------
        // 1. Make a recv() call to the server, using recv_buff
        bytes_recvd = recv(sock, recv_buff, sizeof(recv_buff), 0);
        // 2. Get the number of bytes received and store in the bytes_recvd variable
        // Already stored in bytes_recvd
        // 3. Check for an error
        if (bytes_recvd < 0) {
            // a. Close the socket
            close(sock);
            // b. Return -1 to indicate an error
            return -1;
        } else if (bytes_recvd == 0) {
            // Server closed the connection unexpectedly
            fprintf(stderr, "Server closed the connection unexpectedly\n");
            close(sock);
            return -1;
        }
        
        //You can uncomment out the fprintf() calls below to see what is going on

        //fprintf(stdout, "%.*s", bytes_recvd, recv_buff);
        total_bytes += bytes_recvd;
        //fprintf(stdout, "remaining %d, received %d\n", bytes_remaining, bytes_recvd);
        bytes_remaining -= bytes_recvd;
    }

    fprintf(stdout, "\n\nOK\n");
    fprintf(stdout, "TOTAL BYTES: %d\n", total_bytes);

    //processed the request OK, return the socket, in case we had to reopen
    //so that it can be used in the next request

    //---------------------------------------------------------------------------------
    // Documentation
    //
    // MY ANSWER:
    // This function returns the active socket because it may have reopened a new socket
    // during the request (if the initial send failed). By returning the (possibly new)
    // socket descriptor, we ensure that the caller has the correct socket to use for
    // subsequent requests. This is important because we're using a persistent connection
    // with "Connection: Keep-Alive" and want to maintain the same socket across multiple
    // requests when possible.
    //--------------------------------------------------------------------------------
    return sock;
}

int main(int argc, char *argv[]){
    int sock;

    const char *host = DEFAULT_HOST;
    uint16_t   port = DEFAULT_PORT;
    char       *resource = DEFAULT_PATH;
    int        remaining_args = 0;

    // Timing variables
    clock_t start_time, end_time;
    double duration;

    // Get the start time
    start_time = clock();

    //YOU DONT NEED TO DO ANYTHING OR MODIFY ANYTHING IN MAIN().  MAKE SURE YOU UNDERSTAND
    //THE CODE HOWEVER
    sock = server_connect(host, port);

    if(argc < 4){
        print_usage(argv[0]);
        //process the default request
        submit_request(sock, host, port, resource);
	} else {
        host = argv[1];
        port = atoi(argv[2]);
        resource = argv[3];
        if (port == 0) {
            fprintf(stderr, "NOTE: <port> must be an integer, using default port %d\n", DEFAULT_PORT);
            port = DEFAULT_PORT;
        }
        fprintf(stdout, "Running with host = %s, port = %d\n", host, port);
        remaining_args = argc-3;
        for(int i = 0; i < remaining_args; i++){
            resource = argv[3+i];
            fprintf(stdout, "\n\nProcessing request for %s\n\n", resource);
            sock = submit_request(sock, host, port, resource);
        }
    }

    server_disconnect(sock);
}