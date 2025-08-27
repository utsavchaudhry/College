
#pragma once

#include <sys/socket.h>
#include <arpa/inet.h>
#include <stdbool.h>
#include <stdint.h>
#include <netinet/in.h>
#include <string.h>

struct dp_sock {
    socklen_t          len;
    _Bool              isAddrInit;
    struct sockaddr_in addr;
};

typedef struct dp_connection {
    unsigned int       seqNum;
    int                udp_sock;
    _Bool              isConnected;
    struct dp_sock     outSockAddr;
    struct dp_sock     inSockAddr;
    int                dbgMode;
} dp_connection;

typedef struct dp_connection *dp_connp;

/*
 * Drexel Protocol (dp) PDU
 */
#define DP_PROTO_VER_1   1

// Message Types
#define DP_MT_ACK        1              // ACK MSG
#define DP_MT_SND        2              // SND MSG
#define DP_MT_CONNECT    4              // CONNECT MSG
#define DP_MT_CLOSE      8              // CLOSE MSG
#define DP_MT_NACK       16             // NACK MSG
#define DP_MT_FRAGMENT   32             // DGRAM IS A FRAGMENT
#define DP_MT_ERROR      64             // SIMULATE ERROR

// Message ACKs
#define DP_MT_SNDACK    (DP_MT_SND     | DP_MT_ACK)
#define DP_MT_CNTACK    (DP_MT_CONNECT | DP_MT_ACK)
#define DP_MT_CLOSEACK  (DP_MT_CLOSE   | DP_MT_ACK)

typedef struct dp_pdu {
    uint16_t proto_ver;
    uint16_t mtype;
    uint32_t dgram_sz;
    uint32_t seqnum;
    uint32_t err_num;
    uint32_t total_size; // New field to indicate total data size
} dp_pdu;

#define     DP_MAX_BUFF_SZ          512
#define     DP_MAX_DGRAM_SZ         (DP_MAX_BUFF_SZ + sizeof(dp_pdu))

#define     DP_NO_ERROR             0
#define     DP_ERROR_GENERAL        -1
#define     DP_ERROR_PROTOCOL       -2
#define     DP_BUFF_UNDERSIZED      -4
#define     DP_BUFF_OVERSIZED       -8
#define     DP_CONNECTION_CLOSED    -16
#define     DP_ERROR_BAD_DGRAM      -32

// API Interface
dp_connp dpServerInit(int port);
dp_connp dpClientInit(char *addr, int port);
int dprecv(dp_connp dp, void *buff, int buff_sz);
int dpsend(dp_connp dp, void *sbuff, int sbuff_sz);
int dplisten(dp_connp dp);
int dpconnect(dp_connp dp);
int dpdisconnect(dp_connp dp);
void dpclose(dp_connp dpsession);
void print_out_pdu(dp_pdu *pdu);
void print_in_pdu(dp_pdu *pdu);
int  dpmaxdgram();
