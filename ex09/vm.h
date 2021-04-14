#define HALT    0x00
#define JUMP    0x01
#define JNZ     0x02
#define DUP     0x03
#define SWAP    0x04
#define DROP    0x05
#define PUSH4   0x06
#define PUSH2   0x07
#define PUSH1   0x08
#define ADD     0x09
#define SUB     0x0a
#define MUL     0x0b
#define DIV     0x0c
#define MOD     0x0d
#define EQ      0x0e
#define NE      0x0f
#define LT      0x10
#define GT      0x11
#define LE      0x12
#define GE      0x13
#define NOT     0x14
#define AND     0x15
#define OR      0x16
#define INPUT   0x17
#define OUTPUT  0x18
#define CLOCK   0x2a
#define CONS    0x30
#define HD      0x31
#define TL      0x32

#define STACK_SIZE 65535
#define PROGRAM_SIZE 65535

// These functions bellow are used for the stack.

int pop(int stack[], int *stack_counter, int *stack_element) {
    /*
    Pops an element from the stack, decrements the stack counter and returns the
    popped element in 'stack_element'.
    */
    if (*stack_counter == 0) {
        printf("The stack is empty. Cannot pop!\n");
        return -1;
    }

    *stack_element = stack[--(*stack_counter)];
    return 0;
}

int push(int stack[], int *stack_counter, int new_elem) {
    /*
    Pushes 'new_elem' in the stack and increments the stack counter.
    */
    if (*stack_counter == STACK_SIZE-1) {
        printf("The stack is full. Cannot push!\n");
        return -1;
    }
    
    stack[(*stack_counter)++] = new_elem;
    return 0;
}

int swap(int *x, int *y){
    int temp = *x;
    *x = *y;
    *y = temp;
}
