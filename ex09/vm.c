#include <stdio.h>
#include <stdint.h>
#include "vm.h"

// This function reads the program from the input file and 
// "writes" it in the program array.
int read_program(FILE *fp, uint8_t program[65536][5]) {
    int n;
    uint8_t op;
    uint8_t byte_args[3];
    int pc = 0;
    while(fread(&op, 1, 1, fp) > 0) {
        
        // Pass the op-code in the first element of each line
        // of the program array.
        printf("PC: %d\n", pc);
        program[pc][0] = op;
        printf("%02X\n", op);

        // Fill the next elements of that program line with the integer
        // parameters that may exist.
        switch(op) {
            case HALT:
                break;
            case JUMP:
                n = fread(byte_args, 1, 2, fp);
                program[pc][1] = byte_args[0];
                program[pc][2] = byte_args[1];
                break;
            case JNZ:
                fread(byte_args, 1, 2, fp);
                program[pc][1] = byte_args[0];
                program[pc][2] = byte_args[1];
                break;
            case DUP:
                fread(byte_args, 1, 1, fp);
                program[pc][1] = byte_args[0];
                break;
            case SWAP:
                fread(byte_args, 1, 1, fp);
                program[pc][1] = byte_args[0];
                break;
            case DROP:
                break;
            case PUSH4:
                fread(byte_args, 1, 4, fp);
                program[pc][1] = byte_args[0];
                program[pc][2] = byte_args[1];
                program[pc][3] = byte_args[2];
                program[pc][4] = byte_args[3];
                break;
            case PUSH2:
                fread(byte_args, 1, 2, fp);
                program[pc][1] = byte_args[0];
                program[pc][2] = byte_args[1];
                break;
            case PUSH1:
                fread(byte_args, 1, 1, fp);
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
                printf("Error! Operator is not correct!");
        }
        pc++;    
    }
}

int pop(int stack[], int *sc) {
    if (sc == -1) {
        printf("Cannot pop from empty stack!\n");
        return -1;
    }
    return stack[*sc--];
}

int push(int stack[], int *sc, int new_elem) {
    if (*sc == STACK_SIZE-1) {
        printf("Cannot push in full stack!\n");
        return -1;
    }
    
    stack[++(*sc)] = new_elem;
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
    int temp, stack_counter = -1, stack_index, stack_element;
    
    while(1) {
        switch (program[pc][0]) {
            case HALT:
                return 0;
            case JUMP:
                pc = program[pc][2]*8 + program[pc][1];
                break;
            case JNZ:
                if(pop(stack, &stack_counter) != 0){
                    pc = program[pc][2]*8 + program[pc][1];
                }
                break;
            case DUP:
                stack_index = stack_counter - program[pc][1];
                stack_element = stack[stack_index];
                push(stack, stack_counter, stack_element);
                pc++;
                break;
            case SWAP:
                stack_index = stack_counter - program[pc][1];
                swap(&stack[stack_index], &stack[stack_counter]);
                break;
            default:
                printf("Something went wrong when running the program!\n");
        }
    }
}

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