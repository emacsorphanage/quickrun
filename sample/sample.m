#import <stdio.h>
#import <objc/Object.h>

@interface HelloClass : Object
- (void)helloWorld;
@end

@implementation HelloClass
- (void)helloWorld {
    printf("Hello world!\n");
}
@end

int main() {
    id obj = [HelloClass alloc];
    [obj helloWorld];
    return 0;
}
