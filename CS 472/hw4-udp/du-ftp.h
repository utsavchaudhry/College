
#pragma once

#include <stdint.h>

#define FTP_MAX_FILENAME_LEN 256
#define FTP_MAX_ERROR_MSG_LEN 256

// Message Types
#define FTP_MSG_REQUEST    1  // Client requests to send a file
#define FTP_MSG_RESPONSE   2  // Server responds to the request
#define FTP_MSG_DATA       3  // Data message containing file content
#define FTP_MSG_ACK        4  // Acknowledgment for data reception
#define FTP_MSG_ERROR      5  // Error message
#define FTP_MSG_CLOSE      6  // Close connection

// Status Codes
#define FTP_STATUS_SUCCESS         0  // Success
#define FTP_STATUS_FILE_NOT_FOUND  1  // File not found
#define FTP_STATUS_ERROR           2  // General error

// PDU Structure
typedef struct ftp_pdu {
    uint16_t msg_type;     // Message type
    uint16_t status_code;  // Status code (if applicable)
    uint32_t data_length;  // Length of the data following the header
    char     filename[FTP_MAX_FILENAME_LEN];  // For REQUEST and RESPONSE messages
    char     error_msg[FTP_MAX_ERROR_MSG_LEN]; // For ERROR messages
} ftp_pdu;


#define PROG_MD_CLI     0
#define PROG_MD_SVR     1
#define DEF_PORT_NO     2080
#define FNAME_SZ        150
#define PROG_DEF_FNAME  "test.c"
#define PROG_DEF_SVR_ADDR   "127.0.0.1"

typedef struct prog_config {
    int     prog_mode;
    int     port_number;
    char    svr_ip_addr[16];
    char    file_name[128];
} prog_config;
