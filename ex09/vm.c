#include <stdio.h>
#include <stdint.h>
#include <stdlib.h>
#include <unistd.h>
#include <time.h>
#include "vm.h"

// This function reads the program from the input file: fp, and 
// "writes" it in the byte_program array.
int read_program(FILE *fp, uint8_t byte_program[65536]) {
    
    // This will be used to read a byte from the input.
    uint8_t b;

    // This counter keeps track of the number of bytes read.
    int pc = 0;

    // We read until we reach ___EOF___.
    while (fread(&b, 1, 1, fp) == 1) {
        byte_program[pc++] = b;
        printf("Byte: %02X\n", b);
        
    }

    return pc;
}

int run_program(int num_of_bytes, uint8_t byte_program[65535]){
    int pc = 0;
    int stack[STACK_SIZE];
    int stack_counter = 0, stack_index, stack_element, a, b, hd, tl, 
    upper_address, lower_address;
    clock_t time_start = clock();
    double time_duration;
    int *cons;
    uintptr_t address;
    
    while(pc < num_of_bytes) {
#ifdef DEBUG
        printf("Program counter: %d, command: %02X \n", pc, byte_program[pc]);
#endif
        switch (byte_program[pc]) {
            case HALT:
#ifdef DEBUG
                printf("HALT: Terminating program execution!\n");
#endif
                return 0;
            case JUMP:
                //pc = byte_program[pc+2]*(2^8) + byte_program[pc+1];
                pc = (int)((uint16_t)byte_program[pc+1]);
#ifdef DEBUG
                printf("JUMP: Going at PC: %d\n", pc);
#endif
                break;
            case JNZ:
                if(pop(stack, &stack_counter, &stack_element) == -1){
                    printf("JNZ: Stack error!");
                    return -1;
                }
                if (stack_element != 0){
                    //pc = byte_program[pc+2]*(2^8) + byte_program[pc+1];
                    pc = (int)((uint16_t)byte_program[pc+1]);
#ifdef DEBUG
                    printf("JNZ: Not Zero! Going at PC: %d\n", pc);
                    printf("Stack: ");
                    for(int i=0; i<stack_counter; i++)
                        printf("%d ", stack[i]);
                    printf("\n");
#endif
                    break;
                }
#ifdef DEBUG
                printf("JNZ: Zero! Going at PC: %d\n", pc);
                printf("Stack: ");
                for(int i=0; i<stack_counter; i++)
                    printf("%d ", stack[i]);
                printf("\n");
#endif
                pc += 3;
                break;
            case DUP:
                stack_index = stack_counter - 1 - byte_program[pc+1];

                if(stack_index < 0 || stack_index >= stack_counter){
                    printf("DUP: Stack error! Index out of bounds.\n");
                    printf("DUP: Stack counter was: %d, index was: %d.\n", stack_counter, (int)byte_program[pc+1]);
                    return -1;
                }

                stack_element = stack[stack_index];
                if (push(stack, &stack_counter, stack_element) == -1){
                    printf("DUP: Stack error!");
                    return -1; 
                }
#ifdef DEBUG
                printf("DUP: Duplicated stack element %d\n", stack_element);
                printf("It was at position %d\n", stack_index);
                printf("Number of stack elements now: %d\n", stack_counter);
                printf("Stack: ");
                for(int i=0; i<stack_counter; i++)
                    printf("%d ", stack[i]);
                printf("\n");
#endif        
                pc += 2;
                break;
            case SWAP:
                stack_index = stack_counter - byte_program[pc+1];
                swap(&stack[stack_index], &stack[stack_counter]);
#ifdef DEBUG
                printf("SWAP: Swapped %d and %d\n", stack[stack_index], stack[stack_counter]);
#endif  
                pc += 2;
                break;
            case DROP:
                if(pop(stack, &stack_counter, &stack_element) == -1){
                    printf("DROP: Stack error!");
                    return -1;
                }
#ifdef DEBUG
                printf("DROP: Dropped %d\n", stack_element);
#endif  
                pc++;
                break;
            case PUSH4:
                stack_element = (int)(*((uint32_t *) &byte_program[pc+1]));
                /*
                stack_element = byte_program[pc+4]*(2^24) 
                + byte_program[pc+3]*(2^16)
                + byte_program[pc+2]*(2^8) 
                + byte_program[pc+1];
                */
#ifdef DEBUG
                printf("PUSH4: Stack element %d\n", stack_element);
                printf("%x, %x, %x, %x\n", byte_program[pc+1], byte_program[pc+2], byte_program[pc+3], byte_program[pc+4]);
#endif          
                if (push(stack, &stack_counter, stack_element) == -1){
                    printf("PUSH4: Stack error!");
                    return -1; 
                }
#ifdef DEBUG
                printf("Stack: ");
                for(int i=0; i<stack_counter; i++)
                    printf("%d ", stack[i]);
                printf("\n");
#endif

                pc += 5;
                break;
            case PUSH2:
                // stack_element = byte_program[pc+2]*(2^16) + byte_program[pc+1]*(2^8);
                stack_element = *((uint16_t *) &byte_program[pc+1]);

#ifdef DEBUG
                printf("PUSH2: Stack element %d\n", stack_element);
#endif  
                
                if (push(stack, &stack_counter, stack_element) == -1){
                    printf("PUSH2: Stack error!");
                    return -1;
                }
#ifdef DEBUG
                printf("Stack: ");
                for(int i=0; i<stack_counter; i++)
                    printf("%d ", stack[i]);
                printf("\n");
#endif

                pc += 3;
                break;
            case PUSH1:
                stack_element = (int)byte_program[pc+1];
#ifdef DEBUG
                printf("PUSH1: Stack element %d\n", stack_element);
#endif 

                if (push(stack, &stack_counter, stack_element) == -1){
                    printf("PUSH1: Stack error!");
                    return -1; 
                }
#ifdef DEBUG
                printf("Stack: ");
                for(int i=0; i<stack_counter; i++)
                    printf("%d ", stack[i]);
                printf("\n");
#endif

                pc += 2;
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
#ifdef DEBUG
                printf("ADD: %d + %d = %d\n", a, b, stack_element);
#endif 
                if (push(stack, &stack_counter, stack_element) == -1){
                    printf("ADD: Stack error!");
                    return -1;
                }
#ifdef DEBUG
                printf("Stack: ");
                for(int i=0; i<stack_counter; i++)
                    printf("%d ", stack[i]);
                printf("\n");
#endif
                pc++;
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
#ifdef DEBUG
                printf("SUB: %d - %d = %d\n", a, b, stack_element);
#endif 
                if (push(stack, &stack_counter, stack_element) == -1){
                    printf("SUB: Stack error!");
                    return -1; 
                }
#ifdef DEBUG
                printf("Stack: ");
                for(int i=0; i<stack_counter; i++)
                    printf("%d ", stack[i]);
                printf("\n");
#endif
                pc++;
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
#ifdef DEBUG
                printf("MUL: %d * %d = %d\n", a, b, stack_element);
#endif 
                if (push(stack, &stack_counter, stack_element) == -1){
                    printf("MUL: Stack error!");
                    return -1; 
                }
                pc++;
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
#ifdef DEBUG
                printf("DIV: %d / %d = %d\n", a, b, stack_element);
#endif 
                if (push(stack, &stack_counter, stack_element) == -1){
                    printf("DIV: Stack error!");
                    return -1; 
                }
                pc++;
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
#ifdef DEBUG
                printf("ADD: %d mod %d = %d\n", a, b, stack_element);
#endif 
                if (push(stack, &stack_counter, stack_element) == -1){
                    printf("MOD: Stack error!");
                    return -1; 
                }
                pc++;
                break;
            case EQ:
                if(pop(stack, &stack_counter, &b) == -1){
                    printf("EQ: Stack error!\n");
                    return -1;
                }
                if(pop(stack, &stack_counter, &a) == -1){
                    printf("EQ: Stack error!\n");
                    return -1;
                }
                if (a == b) {
                    stack_element = 1;
                }
                else {
                    stack_element = 0;
                }
#ifdef DEBUG
                printf("EQ: Result: %d\n", stack_element);
#endif 
                if (push(stack, &stack_counter, stack_element) == -1){
                    printf("EQ: Stack error!\n");
                    return -1; 
                }
#ifdef DEBUG
                printf("Stack: ");
                for(int i=0; i<stack_counter; i++)
                    printf("%d ", stack[i]);
                printf("\n");
#endif
                pc++;
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
#ifdef DEBUG
                printf("NE: Result %d\n", stack_element);
#endif 
                if (push(stack, &stack_counter, stack_element) == -1){
                    printf("NE: Stack error!");
                    return -1; 
                }
                printf("Stack: ");
                for(int i=0; i<stack_counter; i++)
                    printf("%d ", stack[i]);
                printf("\n");
                pc++;
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
#ifdef DEBUG
                printf("LT: Result %d\n", stack_element);
#endif 
                if (push(stack, &stack_counter, stack_element) == -1){
                    printf("LT: Stack error!");
                    return -1; 
                }
                pc++;
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
#ifdef DEBUG
                printf("GT: Result %d\n", stack_element);
#endif 
                if (push(stack, &stack_counter, stack_element) == -1){
                    printf("GT: Stack error!");
                    return -1; 
                }
                pc++;
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
#ifdef DEBUG
                printf("LE: Result %d\n", stack_element);
#endif 
                if (push(stack, &stack_counter, stack_element) == -1){
                    printf("LE: Stack error!");
                    return -1;
                }
                pc++;
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
#ifdef DEBUG
                printf("GE: Result %d\n", stack_element);
#endif 
                if (push(stack, &stack_counter, stack_element) == -1){
                    printf("GE: Stack error!");
                    return -1; 
                }
                pc++;
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
#ifdef DEBUG
                printf("NOT: Result %d\n", stack_element);
#endif 
                if (push(stack, &stack_counter, stack_element) == -1){
                    printf("NOT: Stack error!");
                    return -1; 
                }
                pc++;
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
#ifdef DEBUG
                printf("AND: Result %d\n", stack_element);
#endif 
                if (push(stack, &stack_counter, stack_element) == -1){
                    printf("AND: Stack error!");
                    return -1; 
                }
                pc++;
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
#ifdef DEBUG
                printf("OR: Result %d\n", stack_element);
#endif 
                if (push(stack, &stack_counter, stack_element) == -1){
                    printf("OR: Stack error!\n");
                    return -1; 
                }
                pc++;
                break;
            case INPUT:
                scanf("%d", &stack_element);
                if (push(stack, &stack_counter, stack_element) == -1){
                    printf("INPUT: Stack error!\n");
                    return -1; 
                }
#ifdef DEBUG
                printf("INPUT\n");
#endif 
                pc++;
                break;
            case OUTPUT:
                if(pop(stack, &stack_counter, &stack_element) == -1){
                    printf("OUTPUT: Stack error!\n");
                    return -1;
                }
#ifdef DEBUG
                printf("OUTPUT: About to print: %d \n", stack_element);
                printf("Stack: ");
                for(int i=0; i<stack_counter; i++)
                    printf("%d ", stack[i]);
                printf("\n");
#endif 
                printf("%c", stack_element);
                pc++;
                break;
            case CLOCK:
                time_duration = (double)(clock() - time_start)/CLOCKS_PER_SEC;
                printf("%.6lf\n", time_duration);
#ifdef DEBUG
                printf("CLOCK\n");
#endif 
                pc++;
                break;
            case CONS:
                if(pop(stack, &stack_counter, &hd) == -1){
                    printf("CONS: Stack error!\n");
                    return -1;
                }
                if(pop(stack, &stack_counter, &tl) == -1){
                    printf("CONS: Stack error!\n");
                    return -1;
                }
#ifdef DEBUG
                printf("CONS\n");
#endif 
                cons = malloc(2*sizeof(int));
                cons[0] = hd;
                cons[1] = tl;
                // cons has size = 64bit = 2 ints = 2 stack_elements
                address = (uintptr_t)cons;
                upper_address = (int)((address & 0xFFFFFFFF00000000LL) >> 32);
                lower_address = (int)(address & 0xFFFFFFFFLL);

                if (push(stack, &stack_counter, upper_address) == -1){
                    printf("CONS: Stack error!\n");
                    return -1;
                }
                if (push(stack, &stack_counter, lower_address) == -1){
                    printf("CONS: Stack error!\n");
                    return -1;
                }
                pc++;
                break;
            case HD:
                if(pop(stack, &stack_counter, &lower_address) == -1){
                    printf("HD: Stack error!\n");
                    return -1;
                }
                if(pop(stack, &stack_counter, &upper_address) == -1){
                    printf("HD: Stack error!\n");
                    return -1;
                }
#ifdef DEBUG
                printf("HD\n");
#endif 
                address = ((uintptr_t)upper_address) << 32 | lower_address;

                cons = (int *)address;
                if (push(stack, &stack_counter, cons[0]) == -1){
                    printf("HD: Stack error!\n");
                    return -1;
                }
                pc++;
                break;
            case TL:
                if(pop(stack, &stack_counter, &lower_address) == -1){
                    printf("TL: Stack error!\n");
                    return -1;
                }
                if(pop(stack, &stack_counter, &upper_address) == -1){
                    printf("TL: Stack error!\n");
                    return -1;
                }
#ifdef DEBUG
                printf("TL\n");
#endif 
                address = ((uintptr_t)upper_address) << 32 | lower_address;

                cons = (int *)address;
                if (push(stack, &stack_counter, cons[1]) == -1){
                    printf("HD: Stack error!\n");
                    return -1;
                }
                pc++;
                break;
            default:
                printf("Something went wrong when running the program!\n");
                return 0;
        }
#ifdef DEBUG
        usleep(10000);
#endif
    }
}

int main(int argc, char **argv) {
    uint8_t *byte_program = malloc(PROGRAM_SIZE * sizeof(uint8_t));
    FILE *fp;
    fp = fopen(argv[1], "rb");
    if (!fp) {
        printf("Error while opening input file!\n");
        return -1;
    }

    int num_of_bytes = fread(byte_program, sizeof(uint8_t), PROGRAM_SIZE, fp);
    fclose(fp);
    
    run_program(num_of_bytes, byte_program);

    return 0;
}