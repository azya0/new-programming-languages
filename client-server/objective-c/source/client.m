#import <Foundation/Foundation.h>
#include <sys/socket.h>
#include <netinet/in.h>
#include <arpa/inet.h>
#include <netdb.h>
#include <unistd.h>
#include <string.h>

int main(int argc, const char * argv[]) {
    NSAutoreleasePool *pool = [[NSAutoreleasePool alloc] init];
    
    if (argc != 3) {
        fprintf(stderr, "Usage: %s <hostname_or_ip> <string>\n", argv[0]);
        return 1;
    }
    
    const char *hostname = argv[1];
    const char *message = argv[2];
    
    int clientSocket = socket(AF_INET, SOCK_STREAM, 0);
    if (clientSocket < 0) {
        fprintf(stderr, "Socket creation failed\n");
        return 1;
    }
    
    struct sockaddr_in serverAddr;
    memset(&serverAddr, 0, sizeof(serverAddr));
    serverAddr.sin_family = AF_INET;
    serverAddr.sin_port = htons(2000);
    
    // Пробуем сначала как IP-адрес
    if (inet_pton(AF_INET, hostname, &serverAddr.sin_addr) <= 0) {
        // Если не IP, пробуем разрешить доменное имя
        struct hostent *server = gethostbyname(hostname);
        if (server == NULL) {
            fprintf(stderr, "Cannot resolve hostname: %s\n", hostname);
            close(clientSocket);
            return 1;
        }
        memcpy(&serverAddr.sin_addr, server->h_addr, server->h_length);
    }
    
    if (connect(clientSocket, (struct sockaddr *)&serverAddr, sizeof(serverAddr)) < 0) {
        fprintf(stderr, "Connection failed to %s:2000\n", hostname);
        close(clientSocket);
        return 1;
    }
    
    NSString *msgStr = [NSString stringWithUTF8String:message];
    NSString *request = [NSString stringWithFormat:@"%@\n", msgStr];
    const char *requestStr = [request UTF8String];
    
    ssize_t sent = write(clientSocket, requestStr, strlen(requestStr));
    if (sent < 0) {
        fprintf(stderr, "Write failed\n");
        close(clientSocket);
        return 1;
    }
    
    char buffer[1024];
    ssize_t bytesRead = read(clientSocket, buffer, sizeof(buffer) - 1);
    
    if (bytesRead > 0) {
        buffer[bytesRead] = '\0';
        printf("Response: %s", buffer);
    } else {
        fprintf(stderr, "No response received\n");
    }
    
    close(clientSocket);
    [pool release];
    return 0;
}