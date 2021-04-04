#include <stdio.h>
#include <stdint.h>
#include "vm.h"

// This function reads the program from the input file and 
// "writes" it in the program array.
int read_program(FILE *fp, uint8_t program[65536][5]) {
    int n;
    
    // This will be used to read the op-code of each command.
    uint8_t op;

    // This will be used to read the byte arguments of each command
    // (if they exist).
    uint8_t byte_args[4];

    // This counter keeps track of the number of commands.
    int pc = 0;

    // We read until we reach ___EOF___.
    while (fread(&op, 1, 1, fp) > 0) {
        
        // Pass the op-code in the first element of each row
        // of the program array.
        program[pc][0] = op;
        printf("%02X\n", op);
        
        // Fill the next elements of that row with the byte parameters (if they exist).
        switch(op) {
            case HALT:
                break;
            case JUMP:
                n = fread(byte_args, 1, 2, fp);
                if (n!=2){
                    printf("Error in JUMP!\n");
                    return 1;
                }
                program[pc][1] = byte_args[0];
                program[pc][2] = byte_args[1];
                break;
            case JNZ:
                n = fread(byte_args, 1, 2, fp);
                if (n!=2){
                    printf("Error in JNZ!\n");
                    return 1;
                }
                program[pc][1] = byte_args[0];
                program[pc][2] = byte_args[1];
                break;
            case DUP:
                n = fread(byte_args, 1, 1, fp);
                if (n!=1){
                    printf("Error in DUP!\n");
                    return 1;
                }
                program[pc][1] = byte_args[0];
                break;
            case SWAP:
                fread(byte_args, 1, 1, fp);
                if (n!=1){
                    printf("Error in SWAP!\n");
                    return 1;
                }
                program[pc][1] = byte_args[0];
                break;
            case DROP:
                break;
            case PUSH4:
                n = fread(byte_args, 1, 4, fp);
                if (n!=4){
                    printf("Error in PUSH4!\n");
                    return 1;
                }
                program[pc][1] = byte_args[0];
                program[pc][2] = byte_args[1];
                program[pc][3] = byte_args[2];
                program[pc][4] = byte_args[3];
                break;
            case PUSH2:
                n = fread(byte_args, 1, 2, fp);
                if (n!=2){
                    printf("Error in PUSH2!\n");
                    return 1;
                }
                program[pc][1] = byte_args[0];
                program[pc][2] = byte_args[1];
                break;
            case PUSH1:
                n = fread(byte_args, 1, 1, fp);
                if (n!=1){
                    printf("Error in PUSH1!\n");
                    return 1;
                }
                program[pc][1] = byte_args[0];
                break;
            case ADD:                
                break;
            case SUB:
                break;
            case MUL:
                break;
            case DIV:
                break;
            case MOD:
                break;
            case EQ:
                break;
            case NE:
                break;
            case LT:
                break;
            case GT:
                break;
            case LE:
                break;
            case GE:
                break;
            case NOT:
                break;
            case AND:
                break;
            case OR:
                break;
            case INPUT:
                break;
            case OUTPUT:
                break;
            case CLOCK:
                break;
            case CONS:
                break;
            case HD:
                break;
            case TL:
                break;
            default:
                printf("Error: Invalid operator.\n");
                return 0;
        }
        pc++;    
    }
    return 0;
}


// These functions bellow are used for the manipulation of the stack.

int pop(int stack[], int *stack_counter, int *stack_element) {
    if (*stack_counter == -1) {
        printf("The stack is empty. Cannot pop!\n");
        return -1;
    }

    *stack_element = stack[*stack_counter--];
    return 0;
}

int push(int stack[], int *stack_counter, int new_elem) {
    if (*stack_counter == STACK_SIZE-1) {
        printf("The stack is full. Cannot push!\n");
        return -1;
    }
    
    stack[++(*stack_counter)] = new_elem;
    return 0;
}

int swap(int *x, int *y){
    int temp = *x;
    *x = *y;
    *y = temp;
}

int run_program(uint8_t program[65536][5]){
    int pc = 0;
    int stack[STACK_SIZE];
    int stack_counter = -1, stack_index, stack_element, a, b;
    
    while(1) {
        switch (program[pc][0]) {
            case HALT:
                printf("HALT: Terminating program execution!\n");
                return 0;
            case JUMP:
                pc = program[pc][2]*16 + program[pc][1];
                printf("JUMP: Going at PC: %d\n", pc);
                break;
            case JNZ:
                if(pop(stack, &stack_counter, &stack_element) == -1){
                    printf("JNZ: Stack error!");
                    return -1;
                }
                if (stack_element != 0){
                    pc = program[pc][2]*16 + program[pc][1];
                }
                break;
            case DUP:
                stack_index = stack_counter - program[pc][1];
                stack_element = stack[stack_index];
                if (push(stack, &stack_counter, stack_element) == -1){
                    printf("DUP: Stack error!");
                    return -1; 
                }
                break;
            case SWAP:
                stack_index = stack_counter - program[pc][1];
                swap(&stack[stack_index], &stack[stack_counter]);
                break;
            case DROP:
                if(pop(stack, &stack_counter, &stack_element) == -1){
                    printf("DROP: Stack error!");
                    return -1;
                }
                break;
            case PUSH4:
                stack_element = program[pc][4]*4096 
                + program[pc][3]*256 
                + program[pc][2]*16 
                + program[pc][1];
                break;
            case PUSH2:
                stack_element = program[pc][2]*16 + program[pc][1];
                break;
            case PUSH1:
                stack_element = program[pc][1];
                break;
            case ADD:
                if(pop(stack, &stack_counter, &b) == -1){
                    printf("ADD: Stack error!");
                    return -1;
                }
                if(pop(stack, &stack_counter, &a) == -1){
                    printf("ADD: Stack error!");
                    return -1;
                }
                stack_element = a + b;
                if (push(stack, &stack_counter, stack_element) == -1){
                    printf("ADD: Stack error!");
                    return -1; 
                }
                break;
            case SUB:
                if(pop(stack, &stack_counter, &b) == -1){
                    printf("SUB: Stack error!");
                    return -1;
                }
                if(pop(stack, &stack_counter, &a) == -1){
                    printf("SUB: Stack error!");
                    return -1;
                }
                stack_element = a - b;
                if (push(stack, &stack_counter, stack_element) == -1){
                    printf("SUB: Stack error!");
                    return -1; 
                }
                break;
            case MUL:
                if(pop(stack, &stack_counter, &b) == -1){
                    printf("MUL: Stack error!");
                    return -1;
                }
                if(pop(stack, &stack_counter, &a) == -1){
                    printf("MUL: Stack error!");
                    return -1;
                }
                stack_element = a*b;
                if (push(stack, &stack_counter, stack_element) == -1){
                    printf("MUL: Stack error!");
                    return -1; 
                }
                break;
            case DIV:
                if(pop(stack, &stack_counter, &b) == -1){
                    printf("DIV: Stack error!");
                    return -1;
                }
                if(pop(stack, &stack_counter, &a) == -1){
                    printf("DIV: Stack error!");
                    return -1;
                }
                stack_element = a/b;
                if (push(stack, &stack_counter, stack_element) == -1){
                    printf("DIV: Stack error!");
                    return -1; 
                }
                break;
            case MOD:
                if(pop(stack, &stack_counter, &b) == -1){
                    printf("MOD: Stack error!");
                    return -1;
                }
                if(pop(stack, &stack_counter, &a) == -1){
                    printf("MOD: Stack error!");
                    return -1;
                }
                stack_element = a%b;
                if (push(stack, &stack_counter, stack_element) == -1){
                    printf("MOD: Stack error!");
                    return -1; 
                }
                break;
            case EQ:
                if(pop(stack, &stack_counter, &b) == -1){
                    printf("EQ: Stack error!");
                    return -1;
                }
                if(pop(stack, &stack_counter, &a) == -1){
                    printf("EQ: Stack error!");
                    return -1;
                }
                if (a == b) {
                    stack_element = 1;
                }
                else {
                    stack_element = 0;
                }
                if (push(stack, &stack_counter, stack_element) == -1){
                    printf("EQ: Stack error!");
                    return -1; 
                }
                break;
            case NE:
                if(pop(stack, &stack_counter, &b) == -1){
                    printf("NE: Stack error!");
                    return -1;
                }
                if(pop(stack, &stack_counter, &a) == -1){
                    printf("NE: Stack error!");
                    return -1;
                }
                if (a != b) {
                    stack_element = 1;
                }
                else {
                    stack_element = 0;
                }
                if (push(stack, &stack_counter, stack_element) == -1){
                    printf("NE: Stack error!");
                    return -1; 
                }
                break;
            case LT:
                if(pop(stack, &stack_counter, &b) == -1){
                    printf("LT: Stack error!");
                    return -1;
                }
                if(pop(stack, &stack_counter, &a) == -1){
                    printf("LT: Stack error!");
                    return -1;
                }
                if (a < b) {
                    stack_element = 1;
                }
                else {
                    stack_element = 0;
                }
                if (push(stack, &stack_counter, stack_element) == -1){
                    printf("LT: Stack error!");
                    return -1; 
                }
                break;
            case GT:
                if(pop(stack, &stack_counter, &b) == -1){
                    printf("GT: Stack error!");
                    return -1;
                }
                if(pop(stack, &stack_counter, &a) == -1){
                    printf("GT: Stack error!");
                    return -1;
                }
                if (a > b) {
                    stack_element = 1;
                }
                else {
                    stack_element = 0;
                }
                if (push(stack, &stack_counter, stack_element) == -1){
                    printf("GT: Stack error!");
                    return -1; 
                }
                break;
            case LE:
                if(pop(stack, &stack_counter, &b) == -1){
                    printf("LE: Stack error!");
                    return -1;
                }
                if(pop(stack, &stack_counter, &a) == -1){
                    printf("LE: Stack error!");
                    return -1;
                }
                if (a <= b) {
                    stack_element = 1;
                }
                else {
                    stack_element = 0;
                }
                if (push(stack, &stack_counter, stack_element) == -1){
                    printf("LE: Stack error!");
                    return -1; 
                }
                break;
            case GE:
                if(pop(stack, &stack_counter, &b) == -1){
                    printf("GE: Stack error!");
                    return -1;
                }
                if(pop(stack, &stack_counter, &a) == -1){
                    printf("GE: Stack error!");
                    return -1;
                }
                if (a >= b) {
                    stack_element = 1;
                }
                else {
                    stack_element = 0;
                }
                if (push(stack, &stack_counter, stack_element) == -1){
                    printf("GE: Stack error!");
                    return -1; 
                }
                break;
            case NOT:
                if(pop(stack, &stack_counter, &a) == -1){
                    printf("NOT: Stack error!");
                    return -1;
                }
                if(a == 0){
                    stack_element = 1;
                }
                else {
                    stack_element = 0;
                }
                if (push(stack, &stack_counter, stack_element) == -1){
                    printf("NOT: Stack error!");
                    return -1; 
                }
                break;
            case AND:
                if(pop(stack, &stack_counter, &b) == -1){
                    printf("AND: Stack error!");
                    return -1;
                }
                if(pop(stack, &stack_counter, &a) == -1){
                    printf("AND: Stack error!");
                    return -1;
                }
                if (a != 0 && b != 0) {
                    stack_element = 1;
                }
                else {
                    stack_element = 0;
                }
                if (push(stack, &stack_counter, stack_element) == -1){
                    printf("AND: Stack error!");
                    return -1; 
                }
                break;
            case OR:
                if(pop(stack, &stack_counter, &b) == -1){
                    printf("OR: Stack error!");
                    return -1;
                }
                if(pop(stack, &stack_counter, &a) == -1){
                    printf("OR: Stack error!");
                    return -1;
                }
                if (a == 0 && b == 0) {
                    stack_element = 0;
                }
                else {
                    stack_element = 1;
                }
                if (push(stack, &stack_counter, stack_element) == -1){
                    printf("OR: Stack error!");
                    return -1; 
                }
                break;
            default:
                printf("Something went wrong when running the program!\n");
        }
    }
}
/*
// Test run_program()
int main() {
    uint8_t program[65536][5];
    program[0][0] = JNZ;
    program[0][1] = 1;
    program[0][2] = 0;
    program[1][0] = HALT;
    run_program(program);
    return 0;
}
*/

int main(int argc, char **argv) {
    uint8_t program[65536][5];
    for (int i=0; i<65536; i++){
        for (int j=0; j<5; j++){
            program[i][j] = 0;
        }
    }
    
    FILE *fp;
    fp = fopen(argv[1], "rb");
    if (!fp) {
        printf("Error opening file!\n");
        return 1;
    }

    read_program(fp, program);
    printf("Got out successfully!\n");

    return 0;
}

/*
int main(int argc, char **argv) {
    // We store the program commands in this array.
    // Maximum number of commands is 65536.
    // Each command has 1 byte as an op-code, and 1 or more bytes as
    // integer parameters.
    uint8_t program[65536][5];
    
    FILE *fp;
    fp = fopen(argv[1], "rb");
    if (!fp) {
        printf("Error opening file!\n");
        return 1;
    }

    read_program(fp, program);

    // run_program(program);

    return 0;
}
*/