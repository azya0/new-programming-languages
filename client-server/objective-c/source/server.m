#import <Foundation/Foundation.h>
#include <sys/socket.h>
#include <netinet/in.h>
#include <unistd.h>

@interface StringLengthServer : NSObject
- (void)startServer;
@end

@implementation StringLengthServer

- (void)startServer {
    int serverSocket = socket(AF_INET, SOCK_STREAM, 0);
    if (serverSocket < 0) {
        NSLog(@"Ошибка создания сокета");
        return;
    }
    
    struct sockaddr_in serverAddr;
    serverAddr.sin_family = AF_INET;
    serverAddr.sin_addr.s_addr = INADDR_ANY;
    serverAddr.sin_port = htons(2000);
    
    int opt = 1;
    setsockopt(serverSocket, SOL_SOCKET, SO_REUSEADDR, &opt, sizeof(opt));
    
    if (bind(serverSocket, (struct sockaddr *)&serverAddr, sizeof(serverAddr)) < 0) {
        NSLog(@"Ошибка bind");
        close(serverSocket);
        return;
    }
    
    if (listen(serverSocket, 5) < 0) {
        NSLog(@"Ошибка listen");
        close(serverSocket);
        return;
    }
    
    NSLog(@"Сервер запущен");
    
    while (1) {
        struct sockaddr_in clientAddr;
        socklen_t clientLen = sizeof(clientAddr);
        
        int clientSocket = accept(serverSocket, (struct sockaddr *)&clientAddr, &clientLen);
        if (clientSocket < 0) {
            NSLog(@"Ошибка accept");
            continue;
        }
        
        char buffer[1024];
        ssize_t bytesRead = read(clientSocket, buffer, sizeof(buffer) - 1);
        
        if (bytesRead > 0) {
            buffer[bytesRead] = '\0';
            NSString *request = [NSString stringWithUTF8String:buffer];
            request = [request stringByTrimmingCharactersInSet:[NSCharacterSet newlineCharacterSet]];
            
            NSUInteger length = [request length];
            NSString *response = [NSString stringWithFormat:@"%lu\n", length];
            const char *responseStr = [response UTF8String];
            
            write(clientSocket, responseStr, strlen(responseStr));
        }
        
        close(clientSocket);
    }
    
    close(serverSocket);
}

@end

int main(int argc, const char * argv[]) {
    @autoreleasepool {
        StringLengthServer *server = [[StringLengthServer alloc] init];
        [server startServer];
    }
    return 0;
}