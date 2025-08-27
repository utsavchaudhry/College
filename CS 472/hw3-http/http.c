#include <stdio.h>
#include <string.h>
#include <strings.h>
#include <stdlib.h>
#include <unistd.h>
#include <fcntl.h>
#include <stddef.h>

#include <netinet/tcp.h>
#include <sys/socket.h>
#include <sys/types.h>
#include <netinet/in.h>
#include <netdb.h>

#include "http.h"

//---------------------------------------------------------------------------------
// TODO:  Documentation
//
// Note that this module includes a number of helper functions to support this
// assignment.  YOU DO NOT NEED TO MODIFY ANY OF THIS CODE.  What you need to do
// is to appropriately document the socket_connect(), get_http_header_len(), and
// get_http_content_len() functions. 
//
// NOTE:  I am not looking for a line-by-line set of comments.  I am looking for 
//        a comment block at the top of each function that clearly highlights you
//        understanding about how the function works and that you researched the
//        function calls that I used.  You may (and likely should) add additional
//        comments within the function body itself highlighting key aspects of 
//        what is going on.
//
// There is also an optional extra credit activity at the end of this function. If
// you partake, you need to rewrite the body of this function with a more optimal 
// implementation. See the directions for this if you want to take on the extra
// credit. 
//--------------------------------------------------------------------------------

char *strcasestr(const char *s, const char *find)
{
	char c, sc;
	size_t len;

	if ((c = *find++) != 0) {
		c = tolower((unsigned char)c);
		len = strlen(find);
		do {
			do {
				if ((sc = *s++) == 0)
					return (NULL);
			} while ((char)tolower((unsigned char)sc) != c);
		} while (strncasecmp(s, find, len) != 0);
		s--;
	}
	return ((char *)s);
}

char *strnstr(const char *s, const char *find, size_t slen)
{
	char c, sc;
	size_t len;

	if ((c = *find++) != '\0') {
		len = strlen(find);
		do {
			do {
				if ((sc = *s++) == '\0' || slen-- < 1)
					return (NULL);
			} while (sc != c);
			if (len > slen)
				return (NULL);
		} while (strncmp(s, find, len) != 0);
		s--;
	}
	return ((char *)s);
}

/**
 * Establishes a TCP connection to a specified host and port.
 *
 * @param host The hostname or IP address to connect to.
 * @param port The port number to connect to.
 * @return On success, returns a socket file descriptor connected to the specified host and port.
 *         On failure, returns a negative value indicating the error (-1 for socket or connect errors,
 *         -2 for gethostbyname error).
 *
 * This function performs the following steps:
 * 1. Resolves the hostname to an IP address using gethostbyname().
 *    - If the hostname cannot be resolved, it returns -2 and prints an error using herror().
 * 2. Initializes a sockaddr_in structure with the resolved IP address and the specified port.
 *    - The port is converted to network byte order using htons().
 *    - The address family is set to AF_INET (IPv4).
 * 3. Creates a socket using the socket() system call.
 *    - If socket creation fails, it returns -1 and prints an error using perror().
 * 4. Attempts to connect to the specified address using connect().
 *    - If the connection fails, it closes the socket, returns -1, and prints an error using perror().
 * 5. On success, returns the connected socket file descriptor.
 *
 * Note:
 * - The function uses bcopy() to copy the IP address from the hostent structure to the sockaddr_in structure.
 * - The caller is responsible for closing the socket after use.
 */
int socket_connect(const char *host, uint16_t port){
    struct hostent *hp;
    struct sockaddr_in addr;
    int sock;

    if((hp = gethostbyname(host)) == NULL){
		herror("gethostbyname");
		return -2;
	}
    
    
	bcopy(hp->h_addr_list[0], &addr.sin_addr, hp->h_length);
	addr.sin_port = htons(port);
	addr.sin_family = AF_INET;
	sock = socket(PF_INET, SOCK_STREAM, 0); 
	
	if(sock == -1){
		perror("socket");
		return -1;
	}

    if(connect(sock, (struct sockaddr *)&addr, sizeof(struct sockaddr_in)) == -1){
		perror("connect");
		close(sock);
        return -1;
	}

    return sock;
}

/**
 * Calculates the length of the HTTP header within a given buffer.
 *
 * @param http_buff Pointer to the buffer containing the HTTP response data.
 * @param http_buff_len The total length of data in the buffer.
 * @return On success, returns the length of the HTTP header (number of bytes).
 *         On failure (if the end of the header is not found), returns -1.
 *
 * This function searches for the end of the HTTP header in the provided buffer.
 * The end of the HTTP header is denoted by the sequence "\r\n\r\n" as per the HTTP specification.
 * It uses strnstr() to search for the header terminator within the buffer up to http_buff_len bytes.
 *
 * Steps:
 * 1. Searches for the end of the header ("\r\n\r\n") in the buffer.
 *    - If not found, prints an error message and returns -1.
 * 2. Calculates the header length by computing the difference between the start of the buffer
 *    and the position of the header end sequence, adding the length of the header end sequence.
 * 3. Returns the calculated header length.
 *
 * Note:
 * - The function assumes that the buffer contains at least part of an HTTP response.
 * - It is the caller's responsibility to ensure that the buffer contains valid data.
 */
int get_http_header_len(char *http_buff, int http_buff_len){
    char *end_ptr;
    int header_len = 0;
    end_ptr = strnstr(http_buff,HTTP_HEADER_END,http_buff_len);

    if (end_ptr == NULL) {
        fprintf(stderr, "Could not find the end of the HTTP header\n");
        return -1;
    }

    header_len = (end_ptr - http_buff) + strlen(HTTP_HEADER_END);

    return header_len;
}

/**
 * Extracts the Content-Length value from the HTTP header in a buffer.
 *
 * @param http_buff Pointer to the buffer containing the HTTP response header.
 * @param http_header_len The length of the HTTP header within the buffer.
 * @return On success, returns the Content-Length as an integer.
 *         If the Content-Length header is not found or parsing fails, returns 0.
 *
 * This function parses the HTTP header to find the "Content-Length" field and extracts its value.
 *
 * Steps:
 * 1. Initializes pointers to iterate through the header lines within the buffer.
 * 2. While there are header lines to process:
 *    a. Reads a header line using sscanf(), up to the next "\r\n".
 *    b. Checks if the header line matches "Content-Length" (case-insensitive).
 *       - Uses strcasestr() to perform a case-insensitive search for "Content-Length".
 *    c. If found, searches for the ':' delimiter to separate the header name and value.
 *       - Extracts the value part and converts it to an integer using atoi().
 *       - Returns the extracted Content-Length value.
 * 3. If "Content-Length" is not found after processing all header lines, prints an error message and returns 0.
 *
 * Note:
 * - The function uses MAX_HEADER_LINE to limit the size of a header line.
 * - It assumes that header lines are terminated by "\r\n" and that the header ends before http_header_len bytes.
 * - The function does not handle chunked transfer encoding or other cases where Content-Length might not be present.
 */
int get_http_content_len(char *http_buff, int http_header_len){
    char header_line[MAX_HEADER_LINE];

    char *next_header_line = http_buff;
    char *end_header_buff = http_buff + http_header_len;

    while (next_header_line < end_header_buff){
        bzero(header_line,sizeof(header_line));
        sscanf(next_header_line,"%[^\r\n]s", header_line);

        char *isCLHeader2 = strcasecmp(header_line,CL_HEADER);
        char *isCLHeader = strcasestr(header_line,CL_HEADER);
        if(isCLHeader != NULL){
            char *header_value_start = strchr(header_line, HTTP_HEADER_DELIM);
            if (header_value_start != NULL){
                char *header_value = header_value_start + 1;
                int content_len = atoi(header_value);
                return content_len;
            }
        }
        next_header_line += strlen(header_line) + strlen(HTTP_HEADER_EOL);
    }
    fprintf(stderr,"Did not find content length\n");
    return 0;
}

//This function just prints the header, it might be helpful for your debugging
//You dont need to document this or do anything with it, its self explanitory. :-)
void print_header(char *http_buff, int http_header_len){
    fprintf(stdout, "%.*s\n",http_header_len,http_buff);
}

//--------------------------------------------------------------------------------------
//EXTRA CREDIT - 10 pts - READ BELOW
//
// Implement a function that processes the header in one pass to figure out BOTH the
// header length and the content length.  I provided an implementation below just to 
// highlight what I DONT WANT, in that we are making 2 passes over the buffer to determine
// the header and content length.
//
// To get extra credit, you must process the buffer ONCE getting both the header and content
// length.  Note that you are also free to change the function signature, or use the one I have
// that is passing both of the values back via pointers.  If you change the interface dont forget
// to change the signature in the http.h header file :-).  You also need to update client-ka.c to 
// use this function to get full extra credit. 
//--------------------------------------------------------------------------------------

//--------------------------------------------------------------------------------------
// Optional Extra Credit Activity
// Optimized function that processes the header in one pass to determine both the
// header length and the content length.
//--------------------------------------------------------------------------------------
int process_http_header(char *http_buff, int http_buff_len, int *header_len, int *content_len){
    int i = 0;
    *header_len = 0;
    *content_len = 0;

    while (i < http_buff_len - 3) {
        // Check for end of header "\r\n\r\n"
        if (http_buff[i] == '\r' && http_buff[i+1] == '\n' &&
            http_buff[i+2] == '\r' && http_buff[i+3] == '\n') {
            *header_len = i + 4; // Include the "\r\n\r\n"
            break;
        }
        i++;
    }

    if (*header_len == 0) {
        fprintf(stderr, "Could not find the end of the HTTP header\n");
        return -1;
    }

    // Now, parse the header to find "Content-Length"
    i = 0;
    while (i < *header_len) {
        // Find the end of the current header line
        int line_end = i;
        while (line_end < *header_len - 1 && !(http_buff[line_end] == '\r' && http_buff[line_end+1] == '\n')) {
            line_end++;
        }

        if (line_end >= *header_len - 1) {
            break;
        }

        // Extract the header line
        int line_len = line_end - i;
        char header_line[MAX_HEADER_LINE];
        if (line_len >= MAX_HEADER_LINE) {
            fprintf(stderr, "Header line too long\n");
            return -1;
        }
        memcpy(header_line, &http_buff[i], line_len);
        header_line[line_len] = '\0'; // Null-terminate the header line

        // Check if this is the Content-Length header
        if (strncasecmp(header_line, "Content-Length:", 15) == 0) {
            // Skip any spaces after ':'
            char *value_start = header_line + 15;
            while (*value_start == ' ') {
                value_start++;
            }
            int content_length = atoi(value_start);
            *content_len = content_length;
        }

        // Move to the next line
        i = line_end + 2; // Skip over '\r\n'
    }

    return 0; // Success
}