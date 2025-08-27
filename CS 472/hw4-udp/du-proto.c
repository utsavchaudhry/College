
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdbool.h>
#include <unistd.h>
#include <sys/socket.h>
#include <sys/un.h>
#include <time.h>
#include <netinet/in.h>
#include <errno.h>

#include "du-proto.h"

static int dpsenddgram(dp_connp dp, void *sbuff, int sbuff_sz, int total_size, int is_first_pdu);
static int dprecvdgram(dp_connp dp, void *buff, int buff_sz);
static int dpsendraw(dp_connp dp, void *sbuff, int sbuff_sz);
static int dprecvraw(dp_connp dp, void *buff, int buff_sz);
static void print_pdu_details(dp_pdu *pdu);
static char * pdu_msg_to_string(dp_pdu *pdu);

static char _dpBuffer[DP_MAX_DGRAM_SZ];
static int  _debugMode = 1;

static dp_connp dpinit() {
    dp_connp dpsession = malloc(sizeof(dp_connection));
    if (dpsession == NULL) {
        perror("dpinit: Memory allocation failed");
        return NULL;
    }
    bzero(dpsession, sizeof(dp_connection));
    dpsession->outSockAddr.isAddrInit = false;
    dpsession->inSockAddr.isAddrInit = false;
    dpsession->outSockAddr.len = sizeof(struct sockaddr_in);
    dpsession->inSockAddr.len = sizeof(struct sockaddr_in);
    dpsession->seqNum = 0;
    dpsession->isConnected = false;
    dpsession->dbgMode = true;
    return dpsession;
}

void dpclose(dp_connp dpsession) {
    if (dpsession != NULL) {
        close(dpsession->udp_sock);
        free(dpsession);
    }
}

int dpmaxdgram() {
    return DP_MAX_BUFF_SZ;
}

dp_connp dpServerInit(int port) {
    struct sockaddr_in *servaddr;
    int *sock;
    int rc;

    dp_connp dpc = dpinit();
    if (dpc == NULL) {
        return NULL;
    }

    sock = &(dpc->udp_sock);
    servaddr = &(dpc->inSockAddr.addr);

    // Creating socket file descriptor
    if ((*sock = socket(AF_INET, SOCK_DGRAM, 0)) < 0) {
        perror("socket creation failed");
        dpclose(dpc);
        return NULL;
    }

    // Filling server information
    servaddr->sin_family    = AF_INET; // IPv4
    servaddr->sin_addr.s_addr = INADDR_ANY;
    servaddr->sin_port = htons(port);

    // Set socket options
    if (setsockopt(*sock, SOL_SOCKET, SO_REUSEADDR, &(int){1}, sizeof(int)) < 0) {
        perror("setsockopt(SO_REUSEADDR) failed");
        dpclose(dpc);
        return NULL;
    }
    if ((rc = bind(*sock, (const struct sockaddr *)servaddr,
            dpc->inSockAddr.len)) < 0) {
        perror("bind failed");
        dpclose(dpc);
        return NULL;
    }

    dpc->inSockAddr.isAddrInit = true;
    dpc->outSockAddr.len = sizeof(struct sockaddr_in);
    return dpc;
}

dp_connp dpClientInit(char *addr, int port) {
    struct sockaddr_in *servaddr;
    int *sock;

    dp_connp dpc = dpinit();
    if (dpc == NULL) {
        return NULL;
    }

    sock = &(dpc->udp_sock);
    servaddr = &(dpc->outSockAddr.addr);

    // Creating socket file descriptor
    if ((*sock = socket(AF_INET, SOCK_DGRAM, 0)) < 0) {
        perror("socket creation failed");
        dpclose(dpc);
        return NULL;
    }

    // Filling server information
    servaddr->sin_family = AF_INET;
    servaddr->sin_port = htons(port);
    servaddr->sin_addr.s_addr = inet_addr(addr);
    dpc->outSockAddr.len = sizeof(struct sockaddr_in);
    dpc->outSockAddr.isAddrInit = true;

    // The inbound address is the same as the outbound address
    memcpy(&dpc->inSockAddr, &dpc->outSockAddr, sizeof(dpc->outSockAddr));

    return dpc;
}

int dprecv(dp_connp dp, void *buff, int buff_sz) {
    dp_pdu *inPdu;
    int total_rcv_len = 0;
    int expected_total_size = 0;
    int rcvLen = 0;
    char *data_ptr = (char *)buff;

    while (1) {
        rcvLen = dprecvdgram(dp, _dpBuffer, sizeof(_dpBuffer));
        if (rcvLen == DP_CONNECTION_CLOSED)
            return DP_CONNECTION_CLOSED;

        if (rcvLen < sizeof(dp_pdu))
            return DP_ERROR_BAD_DGRAM;

        inPdu = (dp_pdu *)_dpBuffer;

        // For the first PDU, get the total expected size
        if (total_rcv_len == 0) {
            expected_total_size = inPdu->total_size;
            if (expected_total_size > buff_sz) {
                // Buffer is too small to hold the data
                return DP_BUFF_OVERSIZED;
            }
        }

        // Copy the received data into the buffer
        if (inPdu->dgram_sz > 0) {
            memcpy(data_ptr, (_dpBuffer + sizeof(dp_pdu)), inPdu->dgram_sz);
            data_ptr += inPdu->dgram_sz;
            total_rcv_len += inPdu->dgram_sz;
        }

        // Check if we have received all the data
        if (total_rcv_len >= expected_total_size)
            break;
    }

    return total_rcv_len;
}

static int dprecvdgram(dp_connp dp, void *buff, int buff_sz) {
    int bytesIn = 0;
    int errCode = DP_NO_ERROR;

    if (buff_sz > DP_MAX_DGRAM_SZ + sizeof(dp_pdu))
        return DP_BUFF_OVERSIZED;

    bytesIn = dprecvraw(dp, buff, buff_sz);

    // Check for errors
    if (bytesIn < sizeof(dp_pdu))
        errCode = DP_ERROR_BAD_DGRAM;

    dp_pdu *inPdu = (dp_pdu *)buff;
    if (inPdu->dgram_sz > DP_MAX_DGRAM_SZ)
        errCode = DP_BUFF_UNDERSIZED;

    // Update sequence number
    if (errCode == DP_NO_ERROR) {
        dp->seqNum += inPdu->dgram_sz;
    } else {
        dp->seqNum++;
    }

    // Handle control messages if any (e.g., CLOSE)
    if (inPdu->mtype == DP_MT_CLOSE) {
        // Prepare CLOSE/ACK PDU
        dp_pdu outPdu = {0};
        outPdu.proto_ver = DP_PROTO_VER_1;
        outPdu.mtype = DP_MT_CLOSEACK;
        outPdu.dgram_sz = 0;
        outPdu.seqnum = dp->seqNum + 1; // Increment sequence number
        outPdu.err_num = DP_NO_ERROR;
        outPdu.total_size = 0;

        // Send CLOSE/ACK
        int actSndSz = dpsendraw(dp, &outPdu, sizeof(dp_pdu));
        if (actSndSz != sizeof(dp_pdu))
            return DP_ERROR_PROTOCOL;

        dpclose(dp);
        return DP_CONNECTION_CLOSED;
    }

    // Prepare acknowledgment PDU
    dp_pdu outPdu;
    outPdu.proto_ver = DP_PROTO_VER_1;
    outPdu.dgram_sz = 0;
    outPdu.seqnum = dp->seqNum;
    outPdu.err_num = errCode;
    outPdu.total_size = 0;
    outPdu.mtype = (inPdu->mtype == DP_MT_SND) ? DP_MT_SNDACK : DP_MT_ERROR;

    int actSndSz = dpsendraw(dp, &outPdu, sizeof(dp_pdu));
    if (actSndSz != sizeof(dp_pdu))
        return DP_ERROR_PROTOCOL;

    if (errCode != DP_NO_ERROR)
        return errCode;

    return bytesIn;
}

static int dprecvraw(dp_connp dp, void *buff, int buff_sz) {
    int bytesIn;
    struct sockaddr_in from_addr;
    socklen_t addr_len = sizeof(from_addr);

    bytesIn = recvfrom(dp->udp_sock, buff, buff_sz, 0, (struct sockaddr *)&from_addr, &addr_len);
    if (bytesIn < 0) {
        perror("dprecvraw: Error receiving data");
        return DP_ERROR_GENERAL;
    }

    printf("dprecvraw: Received %d bytes from %s:%d\n",
           bytesIn, inet_ntoa(from_addr.sin_addr), ntohs(from_addr.sin_port));

    return bytesIn;
}


int dpsend(dp_connp dp, void *sbuff, int sbuff_sz) {
    int total_sent = 0;
    int bytes_left = sbuff_sz;
    int sndSz = 0;
    int chunk_size = 0;
    char *data_ptr = (char *)sbuff;

    while (bytes_left > 0) {
        chunk_size = (bytes_left > dpmaxdgram()) ? dpmaxdgram() : bytes_left;

        // For the first PDU, include total_size
        sndSz = dpsenddgram(dp, data_ptr, chunk_size, sbuff_sz, (total_sent == 0));
        if (sndSz < 0) {
            return sndSz; // Return error code
        }

        total_sent += chunk_size;
        data_ptr += chunk_size;
        bytes_left -= chunk_size;
    }

    return total_sent;
}

static int dpsenddgram(dp_connp dp, void *sbuff, int sbuff_sz, int total_size, int is_first_pdu) {
    int bytesOut = 0;

    if (!dp->outSockAddr.isAddrInit) {
        perror("dpsend: dp connection not setup properly");
        return DP_ERROR_GENERAL;
    }

    if (sbuff_sz > DP_MAX_DGRAM_SZ)
        return DP_ERROR_GENERAL;

    // Build the PDU and out buffer
    dp_pdu *outPdu = (dp_pdu *)_dpBuffer;
    outPdu->proto_ver = DP_PROTO_VER_1;
    outPdu->mtype = DP_MT_SND;
    outPdu->dgram_sz = sbuff_sz;
    outPdu->seqnum = dp->seqNum;
    outPdu->err_num = DP_NO_ERROR;
    outPdu->total_size = is_first_pdu ? total_size : 0; // Include total_size only in the first PDU

    memcpy((_dpBuffer + sizeof(dp_pdu)), sbuff, sbuff_sz);

    int totalSendSz = outPdu->dgram_sz + sizeof(dp_pdu);
    bytesOut = dpsendraw(dp, _dpBuffer, totalSendSz);

    if (bytesOut != totalSendSz) {
        printf("Warning: sent %d bytes, but expected %d!\n", bytesOut, totalSendSz);
    }

    // Update seq number after send
    dp->seqNum += outPdu->dgram_sz;

    // Need to get an ack
    dp_pdu inPdu = {0};
    int bytesIn = dprecvraw(dp, &inPdu, sizeof(dp_pdu));
    if ((bytesIn < sizeof(dp_pdu)) || (inPdu.mtype != DP_MT_SNDACK)) {
        printf("Expected SND/ACK but got a different mtype %d\n", inPdu.mtype);
        return DP_ERROR_PROTOCOL;
    }

    return bytesOut - sizeof(dp_pdu);
}

static int dpsendraw(dp_connp dp, void *sbuff, int sbuff_sz) {
    int bytesOut = 0;

    if (!dp->outSockAddr.isAddrInit) {
        perror("dpsendraw: dp connection not setup properly");
        return -1;
    }

    bytesOut = sendto(dp->udp_sock, (const char *)sbuff, sbuff_sz,
                      0, (const struct sockaddr *)&(dp->outSockAddr.addr),
                      dp->outSockAddr.len);

    if (bytesOut < 0) {
        perror("dpsendraw: sendto() failed");
        return -1;
    }

    dp_pdu *outPdu = (dp_pdu *)sbuff;
    print_out_pdu(outPdu);

    return bytesOut;
}

int dplisten(dp_connp dp) {
    int sndSz, rcvSz;

    if (!dp->inSockAddr.isAddrInit) {
        perror("dplisten: dp connection not setup properly - inSockAddr not initialized");
        return DP_ERROR_GENERAL;
    }

    dp_pdu pdu = {0};

    printf("Waiting for a connection...\n");
    rcvSz = dprecvraw(dp, &pdu, sizeof(pdu));
    if (rcvSz != sizeof(pdu)) {
        perror("dplisten: The wrong number of bytes were received");
        return DP_ERROR_GENERAL;
    }

    if (pdu.mtype != DP_MT_CONNECT) {
        fprintf(stderr, "dplisten: Expected CONNECT but received a different message type\n");
        return DP_ERROR_PROTOCOL;
    }

    // Prepare CONNECT/ACK
    pdu.proto_ver = DP_PROTO_VER_1;
    pdu.mtype = DP_MT_CNTACK;
    dp->seqNum = pdu.seqnum + 1;
    pdu.seqnum = dp->seqNum;
    pdu.dgram_sz = 0;
    pdu.err_num = DP_NO_ERROR;
    pdu.total_size = 0;

    sndSz = dpsendraw(dp, &pdu, sizeof(pdu));

    if (sndSz != sizeof(pdu)) {
        perror("dplisten: The wrong number of bytes were sent");
        return DP_ERROR_GENERAL;
    }
    dp->isConnected = true;
    printf("Connection established OK!\n");

    return true;
}

int dpconnect(dp_connp dp) {
    int sndSz, rcvSz;

    if (!dp->outSockAddr.isAddrInit) {
        perror("dpconnect: dp connection not setup properly - outSockAddr not initialized");
        return DP_ERROR_GENERAL;
    }

    dp_pdu pdu = {0};
    pdu.proto_ver = DP_PROTO_VER_1;
    pdu.mtype = DP_MT_CONNECT;
    pdu.seqnum = dp->seqNum;
    pdu.dgram_sz = 0;
    pdu.err_num = DP_NO_ERROR;
    pdu.total_size = 0;

    sndSz = dpsendraw(dp, &pdu, sizeof(pdu));
    if (sndSz != sizeof(dp_pdu)) {
        perror("dpconnect: Wrong amount of connection data sent");
        return DP_ERROR_GENERAL;
    }

    rcvSz = dprecvraw(dp, &pdu, sizeof(pdu));
    if (rcvSz != sizeof(dp_pdu)) {
        perror("dpconnect: Wrong amount of connection data received");
        return DP_ERROR_GENERAL;
    }
    if (pdu.mtype != DP_MT_CNTACK) {
        fprintf(stderr, "dpconnect: Expected CONNECT/ACK but didn't get it\n");
        return DP_ERROR_PROTOCOL;
    }

    // For non-data transmissions, ACK of control data increases seqNum by one
    dp->seqNum++;
    dp->isConnected = true;
    printf("Connection established OK!\n");

    return true;
}

int dpdisconnect(dp_connp dp) {
    int sndSz, rcvSz;

    dp_pdu pdu = {0};
    pdu.proto_ver = DP_PROTO_VER_1;
    pdu.mtype = DP_MT_CLOSE;
    pdu.seqnum = dp->seqNum;
    pdu.dgram_sz = 0;
    pdu.err_num = DP_NO_ERROR;
    pdu.total_size = 0;

    // Send CLOSE message
    sndSz = dpsendraw(dp, &pdu, sizeof(pdu));
    if (sndSz != sizeof(dp_pdu)) {
        perror("dpdisconnect: Wrong amount of connection data sent");
        return DP_ERROR_GENERAL;
    }

    // Wait for response
    rcvSz = dprecvraw(dp, &pdu, sizeof(pdu));
    if (rcvSz != sizeof(dp_pdu)) {
        perror("dpdisconnect: Wrong amount of connection data received");
        return DP_ERROR_GENERAL;
    }

    if (pdu.mtype == DP_MT_CLOSEACK) {
        // Received expected CLOSE/ACK
        dpclose(dp);
        return DP_CONNECTION_CLOSED;
    } else if (pdu.mtype == DP_MT_CLOSE) {
        // Received CLOSE instead of CLOSE/ACK
        // Respond with CLOSE/ACK
        dp_pdu ackPdu = {0};
        ackPdu.proto_ver = DP_PROTO_VER_1;
        ackPdu.mtype = DP_MT_CLOSEACK;
        ackPdu.seqnum = dp->seqNum + 1;
        ackPdu.dgram_sz = 0;
        ackPdu.err_num = DP_NO_ERROR;
        ackPdu.total_size = 0;

        sndSz = dpsendraw(dp, &ackPdu, sizeof(dp_pdu));
        if (sndSz != sizeof(dp_pdu)) {
            perror("dpdisconnect: Error sending CLOSE/ACK");
            return DP_ERROR_GENERAL;
        }

        // Now, wait for the CLOSE/ACK from the other side
        rcvSz = dprecvraw(dp, &pdu, sizeof(pdu));
        if (rcvSz != sizeof(dp_pdu)) {
            perror("dpdisconnect: Wrong amount of connection data received");
            return DP_ERROR_GENERAL;
        }
        if (pdu.mtype != DP_MT_CLOSEACK) {
            fprintf(stderr, "dpdisconnect: Expected CLOSE/ACK but didn't get it\n");
            return DP_ERROR_PROTOCOL;
        }

        dpclose(dp);
        return DP_CONNECTION_CLOSED;
    } else if (pdu.mtype == DP_MT_CLOSEACK) {
        // Received CLOSE/ACK, proceed to close
        dpclose(dp);
        return DP_CONNECTION_CLOSED;
    } else {
        fprintf(stderr, "dpdisconnect: Expected CLOSE/ACK but didn't get it\n");
        return DP_ERROR_PROTOCOL;
    }
}



void print_out_pdu(dp_pdu *pdu) {
    if (_debugMode != 1)
        return;
    printf("PDU DETAILS ===>  [OUT]\n");
    print_pdu_details(pdu);
}

void print_in_pdu(dp_pdu *pdu) {
    if (_debugMode != 1)
        return;
    printf("===> PDU DETAILS  [IN]\n");
    print_pdu_details(pdu);
}

void print_pdu_details(dp_pdu *pdu) {
    printf("\tVersion:  %d\n", pdu->proto_ver);
    printf("\tMsg Type: %s\n", pdu_msg_to_string(pdu));
    printf("\tMsg Size: %d\n", pdu->dgram_sz);
    printf("\tSeq Num:  %d\n", pdu->seqnum);
    printf("\tErr Num:  %d\n", pdu->err_num);
    printf("\tTotal Size: %d\n", pdu->total_size);
    printf("\n");
}

static char *pdu_msg_to_string(dp_pdu *pdu) {
    switch (pdu->mtype) {
        case DP_MT_ACK:
            return "ACK";
        case DP_MT_SND:
            return "SEND";
        case DP_MT_CONNECT:
            return "CONNECT";
        case DP_MT_CLOSE:
            return "CLOSE";
        case DP_MT_NACK:
            return "NACK";
        case DP_MT_SNDACK:
            return "SEND/ACK";
        case DP_MT_CNTACK:
            return "CONNECT/ACK";
        case DP_MT_CLOSEACK:
            return "CLOSE/ACK";
        default:
            return "***UNKNOWN***";
    }
}
