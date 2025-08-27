
#include <stdlib.h>
#include <unistd.h>
#include <string.h>
#include <stdio.h>
#include <stdbool.h>
#include <getopt.h>
#include <sys/stat.h>

#include "du-ftp.h"
#include "du-proto.h"

#define BUFF_SZ DP_MAX_BUFF_SZ  // Use maximum allowed buffer size
static char full_file_path[FNAME_SZ];

static int initParams(int argc, char *argv[], prog_config *cfg);
void start_client(dp_connp dpc, prog_config *cfg);
void start_server(dp_connp dpc, prog_config *cfg);
void send_file_data(dp_connp dpc, prog_config *cfg);
void receive_file_data(dp_connp dpc, FILE *f);

static int initParams(int argc, char *argv[], prog_config *cfg) {
    int option;
    // Setup defaults if no arguments are passed
    static char cmdBuffer[64] = {0};

    cfg->prog_mode = PROG_MD_CLI;
    cfg->port_number = DEF_PORT_NO;
    strcpy(cfg->file_name, PROG_DEF_FNAME);
    strcpy(cfg->svr_ip_addr, PROG_DEF_SVR_ADDR);

    while ((option = getopt(argc, argv, ":p:f:a:csh")) != -1) {
        switch (option) {
            case 'p':
                strncpy(cmdBuffer, optarg, sizeof(cmdBuffer));
                cfg->port_number = atoi(cmdBuffer);
                break;
            case 'f':
                strncpy(cfg->file_name, optarg, sizeof(cfg->file_name));
                break;
            case 'a':
                strncpy(cfg->svr_ip_addr, optarg, sizeof(cfg->svr_ip_addr));
                break;
            case 'c':
                cfg->prog_mode = PROG_MD_CLI;
                break;
            case 's':
                cfg->prog_mode = PROG_MD_SVR;
                break;
            case 'h':
                printf("USAGE: %s [-p port] [-f fname] [-a svr_addr] [-s] [-c] [-h]\n", argv[0]);
                printf("WHERE:\n\t[-c] runs in client mode, [-s] runs in server mode; DEFAULT= client_mode\n");
                printf("\t[-a svr_addr] specifies the server's IP address as a string; DEFAULT = %s\n", cfg->svr_ip_addr);
                printf("\t[-p portnum] specifies the port number; DEFAULT = %d\n", cfg->port_number);
                printf("\t[-f fname] specifies the filename to send or recv; DEFAULT = %s\n", cfg->file_name);
                printf("\t[-h] displays this help message\n\n");
                exit(0);
            case ':':
                fprintf(stderr, "Option -%c requires an argument.\n", optopt);
                exit(-1);
            default:
            case '?':
                fprintf(stderr, "Unknown option -%c\n", optopt);
                exit(-1);
        }
    }
    return cfg->prog_mode;
}

int server_loop(dp_connp dpc, void *sBuff, void *rBuff, int sbuff_sz, int rbuff_sz) {
    int rcvSz;

    // Ensure the infile directory exists
    mkdir("./infile", 0777);

    FILE *f = fopen(full_file_path, "wb");
    if (f == NULL) {
        printf("ERROR: Cannot open file %s\n", full_file_path);
        exit(-1);
    }
    if (dpc->isConnected == false) {
        perror("Expecting the protocol to be in connected state, but it's not");
        fclose(f);
        exit(-1);
    }
    // Loop until a disconnect is received, or error happens
    while (1) {
        // Receive data from client
        rcvSz = dprecv(dpc, rBuff, rbuff_sz);
        if (rcvSz == DP_CONNECTION_CLOSED) {
            fclose(f);
            printf("Client closed connection\n");
            return DP_CONNECTION_CLOSED;
        }
        fwrite(rBuff, 1, rcvSz, f);
        fflush(f);
        rcvSz = rcvSz > 50 ? 50 : rcvSz; // Just print the first 50 characters max

        printf("========================> \n%.*s\n========================> \n",
               rcvSz, (char *)rBuff);
    }
}

void start_client(dp_connp dpc, prog_config *cfg) {
    ftp_pdu pdu = {0};
    int sndSz, rcvSz;

    // Send REQUEST message with filename
    pdu.msg_type = FTP_MSG_REQUEST;
    strncpy(pdu.filename, cfg->file_name, FTP_MAX_FILENAME_LEN);
    pdu.data_length = strlen(cfg->file_name) + 1; // Include null terminator

    sndSz = dpsend(dpc, &pdu, sizeof(ftp_pdu));
    if (sndSz < 0) {
        fprintf(stderr, "Error sending REQUEST message\n");
        dpdisconnect(dpc);
        exit(-1);
    }

    // Receive RESPONSE message from server
    rcvSz = dprecv(dpc, &pdu, sizeof(ftp_pdu));
    if (rcvSz < 0 || pdu.msg_type != FTP_MSG_RESPONSE) {
        fprintf(stderr, "Error receiving RESPONSE message\n");
        dpdisconnect(dpc);
        exit(-1);
    }

    if (pdu.status_code != FTP_STATUS_SUCCESS) {
        fprintf(stderr, "Server error: %s\n", pdu.error_msg);
        dpdisconnect(dpc);
        exit(-1);
    }

    // Send the file data
    send_file_data(dpc, cfg);

    // Send CLOSE message
    pdu.msg_type = FTP_MSG_CLOSE;
    pdu.data_length = 0;
    sndSz = dpsend(dpc, &pdu, sizeof(ftp_pdu));
    if (sndSz < 0) {
        fprintf(stderr, "Error sending CLOSE message\n");
    }

    dpdisconnect(dpc);
}


void send_file_data(dp_connp dpc, prog_config *cfg) {
    FILE *f = fopen(full_file_path, "rb");
    if (f == NULL) {
        fprintf(stderr, "ERROR: Cannot open file %s\n", full_file_path);
        // Handle error appropriately
        return;
    }

    ftp_pdu pdu = {0};
    pdu.msg_type = FTP_MSG_DATA;

    char buffer[DP_MAX_DGRAM_SZ];
    size_t maxDataSize = DP_MAX_DGRAM_SZ - sizeof(ftp_pdu);
    size_t bytesRead;

    while ((bytesRead = fread(buffer + sizeof(ftp_pdu), 1, maxDataSize, f)) > 0) {
        pdu.data_length = bytesRead;
        memcpy(buffer, &pdu, sizeof(ftp_pdu));

        int totalSendSz = sizeof(ftp_pdu) + bytesRead;
        int sent = dpsend(dpc, buffer, totalSendSz);
        if (sent < 0) {
            fprintf(stderr, "Error sending data\n");
            fclose(f);
            dpdisconnect(dpc);
            exit(-1);
        }
    }

    fclose(f);
}

void start_server(dp_connp dpc, prog_config *cfg) {
    ftp_pdu pdu = {0};
    int rcvSz, sndSz;

    // Receive REQUEST message from client
    rcvSz = dprecv(dpc, &pdu, sizeof(ftp_pdu));
    if (rcvSz < 0 || pdu.msg_type != FTP_MSG_REQUEST) {
        fprintf(stderr, "Error receiving REQUEST message\n");
        dpdisconnect(dpc);
        exit(-1);
    }

    // Prepare to receive the file
    snprintf(full_file_path, sizeof(full_file_path), "./infile/%s", pdu.filename);
    FILE *f = fopen(full_file_path, "wb");
    if (f == NULL) {
        fprintf(stderr, "ERROR: Cannot open file %s\n", full_file_path);

        // Send ERROR message to client
        pdu.msg_type = FTP_MSG_ERROR;
        pdu.status_code = FTP_STATUS_ERROR;
        strncpy(pdu.error_msg, "Cannot open file for writing", FTP_MAX_ERROR_MSG_LEN);
        pdu.data_length = strlen(pdu.error_msg) + 1;

        dpsend(dpc, &pdu, sizeof(ftp_pdu));
        dpdisconnect(dpc);
        exit(-1);
    }

    // Send RESPONSE message indicating readiness
    pdu.msg_type = FTP_MSG_RESPONSE;
    pdu.status_code = FTP_STATUS_SUCCESS;
    pdu.data_length = 0;

    sndSz = dpsend(dpc, &pdu, sizeof(ftp_pdu));
    if (sndSz < 0) {
        fprintf(stderr, "Error sending RESPONSE message\n");
        fclose(f);
        dpdisconnect(dpc);
        exit(-1);
    }

    // Receive file data
    receive_file_data(dpc, f);

    fclose(f);
    dpdisconnect(dpc);
}


void receive_file_data(dp_connp dpc, FILE *f) {
    char buffer[DP_MAX_DGRAM_SZ];
    ftp_pdu pdu;
    int rcvSz;

    while (1) {
        rcvSz = dprecv(dpc, buffer, sizeof(buffer));
        if (rcvSz < 0) {
            fprintf(stderr, "Error receiving data\n");
            break;
        }

        memcpy(&pdu, buffer, sizeof(ftp_pdu));

        if (pdu.msg_type == FTP_MSG_DATA) {
            fwrite(buffer + sizeof(ftp_pdu), 1, pdu.data_length, f);
        } else if (pdu.msg_type == FTP_MSG_CLOSE) {
            break;
        } else {
            fprintf(stderr, "Unexpected message type: %d\n", pdu.msg_type);
            break;
        }
    }
}

int main(int argc, char *argv[]) {
    prog_config cfg;
    int cmd;
    dp_connp dpc;
    int rc;

    // Process the parameters and init the header
    cmd = initParams(argc, argv, &cfg);

    printf("MODE %d\n", cfg.prog_mode);
    printf("PORT %d\n", cfg.port_number);
    printf("FILE NAME: %s\n", cfg.file_name);

    switch (cmd) {
        case PROG_MD_CLI:
            // By default client will look for files in the ./outfile directory
            snprintf(full_file_path, sizeof(full_file_path), "./outfile/%s", cfg.file_name);
            dpc = dpClientInit(cfg.svr_ip_addr, cfg.port_number);
            if (dpconnect(dpc) < 0) {
                fprintf(stderr, "Error establishing connection\n");
                exit(-1);
            }
            start_client(dpc, &cfg);
            break;

        case PROG_MD_SVR:
            // By default server will save files in the ./infile directory
            snprintf(full_file_path, sizeof(full_file_path), "./infile/%s", cfg.file_name);
            dpc = dpServerInit(cfg.port_number);
            if (dpc == NULL) {
                fprintf(stderr, "Server initialization failed\n");
                exit(-1);
            }
            rc = dplisten(dpc);
            if (rc < 0) {
                fprintf(stderr, "Error establishing connection\n");
                dpclose(dpc);
                exit(-1);
            }
            start_server(dpc, &cfg);
            break;

        default:
            fprintf(stderr, "Unknown program mode\n");
            break;
    }
    return 0;
}
