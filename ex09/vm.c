#include <stdio.h>
#include <stdint.h>
#include <stdlib.h>
#include <unistd.h>
#include <time.h>
#include "vm.h"

int run_program(int num_of_bytes, uint8_t byte_program[65535]){
    int pc = 0, stack[STACK_SIZE], stack_counter = 0, stack_index, stack_element, 
        a, b, hd, tl, upper_address, lower_address;
    
    /* Heap related */
    bool is_ptr[STACK_SIZE];
    for(int i=0; i<STACK_SIZE; i++){
        is_ptr[i] = false;            
    }
    Heap *heap = alloc_heap();

    int offset;       
    cons_cell * cons;
    /* Heap relatesd */
    
    clock_t time_start = clock();
    double time_duration;
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
                pc = (int)((uint16_t)byte_program[pc+1]);
#ifdef DEBUG
                printf("JUMP: Going at PC: %d\n", pc);
#endif
                break;
            case JNZ:
                if(pop(stack, &stack_counter, &stack_element) == -1){
                    printf("JNZ: Stack error!\n");
                    return -1;
                }
                if (stack_element != 0){
                    pc = (int)((uint16_t)byte_program[pc+1]);
#ifdef DEBUG
                    printf("JNZ: Not zero, going at PC: %d\n", pc);
#endif
                    break;
                }
#ifdef DEBUG
                printf("JNZ: Is zero, going at PC: %d\n", pc);
#endif
                pc += 3;
                break;
            case DUP:
                // The most recent element that was pushed in the stack array is at position:
                // stack_counter-1. So, 'stack_index' is the position (in the stack array)
                // of the element that is specified in 'byte_program[pc+1]'.
                stack_index = stack_counter - 1 - byte_program[pc+1];

                if(stack_index < 0 || stack_index >= stack_counter){
                    printf("DUP: Stack error! Index out of bounds.\n");
                    printf("DUP: Stack counter: %d, index: %d.\n", stack_counter, (int)byte_program[pc+1]);
                    return -1;
                }

                stack_element = stack[stack_index];
                if (push(stack, &stack_counter, stack_element) == -1){
                    printf("DUP: Stack error!\n");
                    return -1; 
                }
#ifdef DEBUG
                printf("DUP: Duplicated stack element %d from position: %d\n", stack_element, stack_index);
#endif        
                pc += 2;
                break;
            case SWAP:
                stack_index = stack_counter - 1 - byte_program[pc+1];
                swap(&stack[stack_index], &stack[stack_counter-1]);
#ifdef DEBUG
                printf("SWAP: Swapped %d and %d\n", stack[stack_index], stack[stack_counter]);
#endif  
                pc += 2;
                break;
            case DROP:
                if(pop(stack, &stack_counter, &stack_element) == -1){
                    printf("DROP: Stack error!\n");
                    return -1;
                }
#ifdef DEBUG
                printf("DROP: Dropped element: %d\n", stack_element);
#endif  
                pc++;
                break;
            case PUSH4:
                stack_element = (int)(*((uint32_t *) &byte_program[pc+1]));
#ifdef DEBUG
                printf("PUSH4: Stack element %d\n", stack_element);
#endif          
                if (push(stack, &stack_counter, stack_element) == -1){
                    printf("PUSH4: Stack error!\n");
                    return -1; 
                }

                pc += 5;
                break;
            case PUSH2:
                stack_element = *((uint16_t *) &byte_program[pc+1]);

#ifdef DEBUG
                printf("PUSH2: Stack element %d\n", stack_element);
#endif  
                
                if (push(stack, &stack_counter, stack_element) == -1){
                    printf("PUSH2: Stack error!\n");
                    return -1;
                }

                pc += 3;
                break;
            case PUSH1:
                stack_element = (int)byte_program[pc+1];
#ifdef DEBUG
                printf("PUSH1: Stack element %d\n", stack_element);
#endif 

                if (push(stack, &stack_counter, stack_element) == -1){
                    printf("PUSH1: Stack error!\n");
                    return -1; 
                }

                pc += 2;
                break;
            case ADD:
                if(pop(stack, &stack_counter, &b) == -1){
                    printf("ADD: Stack error!\n");
                    return -1;
                }
                if(pop(stack, &stack_counter, &a) == -1){
                    printf("ADD: Stack error!\n");
                    return -1;
                }
                stack_element = a + b;

                if (push(stack, &stack_counter, stack_element) == -1){
                    printf("ADD: Stack error!\n");
                    return -1;
                }
#ifdef DEBUG
                printf("ADD: %d + %d = %d\n", a, b, stack_element);
#endif 

                pc++;
                break;
            case SUB:
                if(pop(stack, &stack_counter, &b) == -1){
                    printf("SUB: Stack error!\n");
                    return -1;
                }
                if(pop(stack, &stack_counter, &a) == -1){
                    printf("SUB: Stack error!\n");
                    return -1;
                }
                stack_element = a - b;

                if (push(stack, &stack_counter, stack_element) == -1){
                    printf("SUB: Stack error!\n");
                    return -1; 
                }
#ifdef DEBUG
                printf("SUB: %d - %d = %d\n", a, b, stack_element);
#endif 
                
                pc++;
                break;
            case MUL:
                if(pop(stack, &stack_counter, &b) == -1){
                    printf("MUL: Stack error!\n");
                    return -1;
                }
                if(pop(stack, &stack_counter, &a) == -1){
                    printf("MUL: Stack error!\n");
                    return -1;
                }
                stack_element = a*b;

                if (push(stack, &stack_counter, stack_element) == -1){
                    printf("MUL: Stack error!\n");
                    return -1; 
                }
#ifdef DEBUG
                printf("MUL: %d * %d = %d\n", a, b, stack_element);
#endif 

                pc++;
                break;
            case DIV:
                if(pop(stack, &stack_counter, &b) == -1){
                    printf("DIV: Stack error!\n");
                    return -1;
                }
                if(pop(stack, &stack_counter, &a) == -1){
                    printf("DIV: Stack error!\n");
                    return -1;
                }
                stack_element = a/b;

                if (push(stack, &stack_counter, stack_element) == -1){
                    printf("DIV: Stack error!\n");
                    return -1; 
                }
#ifdef DEBUG
                printf("DIV: %d / %d = %d\n", a, b, stack_element);
#endif 
                pc++;
                break;
            case MOD:
                if(pop(stack, &stack_counter, &b) == -1){
                    printf("MOD: Stack error!\n");
                    return -1;
                }
                if(pop(stack, &stack_counter, &a) == -1){
                    printf("MOD: Stack error!\n");
                    return -1;
                }
                stack_element = a%b;

                if (push(stack, &stack_counter, stack_element) == -1){
                    printf("MOD: Stack error!\n");
                    return -1; 
                }
#ifdef DEBUG
                printf("MOD: %d mod %d = %d\n", a, b, stack_element);
#endif 
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

                pc++;
                break;
            case NE:
                if(pop(stack, &stack_counter, &b) == -1){
                    printf("NE: Stack error!\n");
                    return -1;
                }
                if(pop(stack, &stack_counter, &a) == -1){
                    printf("NE: Stack error!\n");
                    return -1;
                }
                if (a != b) {
                    stack_element = 1;
                }
                else {
                    stack_element = 0;
                }
#ifdef DEBUG
                printf("NE: Result: %d\n", stack_element);
#endif 
                if (push(stack, &stack_counter, stack_element) == -1){
                    printf("NE: Stack error!\n");
                    return -1; 
                }

                pc++;
                break;
            case LT:
                if(pop(stack, &stack_counter, &b) == -1){
                    printf("LT: Stack error!\n");
                    return -1;
                }
                if(pop(stack, &stack_counter, &a) == -1){
                    printf("LT: Stack error!\n");
                    return -1;
                }
                if (a < b) {
                    stack_element = 1;
                }
                else {
                    stack_element = 0;
                }
#ifdef DEBUG
                printf("LT: Result: %d\n", stack_element);
#endif 
                if (push(stack, &stack_counter, stack_element) == -1){
                    printf("LT: Stack error!\n");
                    return -1; 
                }
                pc++;
                break;
            case GT:
                if(pop(stack, &stack_counter, &b) == -1){
                    printf("GT: Stack error!\n");
                    return -1;
                }
                if(pop(stack, &stack_counter, &a) == -1){
                    printf("GT: Stack error!\n");
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
                    printf("GT: Stack error!\n");
                    return -1; 
                }
                pc++;
                break;
            case LE:
                if(pop(stack, &stack_counter, &b) == -1){
                    printf("LE: Stack error!\n");
                    return -1;
                }
                if(pop(stack, &stack_counter, &a) == -1){
                    printf("LE: Stack error!\n");
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
                    printf("LE: Stack error!\n");
                    return -1;
                }
                pc++;
                break;
            case GE:
                if(pop(stack, &stack_counter, &b) == -1){
                    printf("GE: Stack error!\n");
                    return -1;
                }
                if(pop(stack, &stack_counter, &a) == -1){
                    printf("GE: Stack error!\n");
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
                    printf("GE: Stack error!\n");
                    return -1; 
                }
                pc++;
                break;
            case NOT:
                if(pop(stack, &stack_counter, &a) == -1){
                    printf("NOT: Stack error!\n");
                    return -1;
                }
                if(a == 0){
                    stack_element = 1;
                }
                else {
                    stack_element = 0;
                }
#ifdef DEBUG
                printf("NOT: Result: %d\n", stack_element);
#endif 
                if (push(stack, &stack_counter, stack_element) == -1){
                    printf("NOT: Stack error!\n");
                    return -1; 
                }
                pc++;
                break;
            case AND:
                if(pop(stack, &stack_counter, &b) == -1){
                    printf("AND: Stack error!\n");
                    return -1;
                }
                if(pop(stack, &stack_counter, &a) == -1){
                    printf("AND: Stack error!\n");
                    return -1;
                }
                if (a != 0 && b != 0) {
                    stack_element = 1;
                }
                else {
                    stack_element = 0;
                }
#ifdef DEBUG
                printf("AND: Result: %d\n", stack_element);
#endif 
                if (push(stack, &stack_counter, stack_element) == -1){
                    printf("AND: Stack error!\n");
                    return -1; 
                }
                pc++;
                break;
            case OR:
                if(pop(stack, &stack_counter, &b) == -1){
                    printf("OR: Stack error!\n");
                    return -1;
                }
                if(pop(stack, &stack_counter, &a) == -1){
                    printf("OR: Stack error!\n");
                    return -1;
                }
                if (a == 0 && b == 0) {
                    stack_element = 0;
                }
                else {
                    stack_element = 1;
                }
#ifdef DEBUG
                printf("OR: Result: %d\n", stack_element);
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
                printf("INPUT: Just read: %d\n", stack_element);
#endif 
                pc++;
                break;
            case OUTPUT:
                if(pop(stack, &stack_counter, &stack_element) == -1){
                    printf("OUTPUT: Stack error!\n");
                    return -1;
                }
#ifdef DEBUG
                printf("OUTPUT: About to print: %c \n", stack_element);
#endif 
                printf("%c", stack_element);
                pc++;
                break;
            case CLOCK:
                time_duration = (double)(clock() - time_start)/CLOCKS_PER_SEC;
#ifdef DEBUG
                printf("CLOCK: Printing time duration since the start of the program.\n");
#endif 
                printf("%.6lf\n", time_duration);
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
                if((heap->from_space == heap->start && heap->heap_position == heap->middle) || 
                   (heap->from_space == heap->middle && heap->heap_position == heap->end)){
#ifdef DEBUG
                    printf("Garbage collection...\n");
#endif                  
                    if(collect(heap, stack, stack_counter, is_ptr) == -1){
                        printf("Something went wrong in the heap.\n");
                        return -1;
                    }

                    if((heap->from_space == heap->start && heap->heap_position >= heap->middle) || 
                       (heap->to_space == heap->middle && heap->heap_position >= heap->end)){
                        printf("Not enough space in the heap after GC.\n");
                        return -1;
                    }
                }
                offset = alloc_and_set_cons(heap, hd, tl);
                
                is_ptr[stack_counter] = true;
                if (push(stack, &stack_counter, offset) == -1){
                    printf("CONS: Stack error!\n");
                    return -1;
                }
                pc++;
                break;
            case HD:
                if(pop(stack, &stack_counter, &offset) == -1){
                    printf("HD: Stack error!\n");
                    return -1;
                }
                is_ptr[stack_counter] = false;
#ifdef DEBUG
                printf("HD\n");
#endif
                cons = get_cons(heap, offset);
                if (push(stack, &stack_counter, cons->hd) == -1){
                    printf("HD: Stack error!\n");
                    return -1;
                }
                pc++;
                break;
            case TL:
                if(pop(stack, &stack_counter, &offset) == -1){
                    printf("TL: Stack error!\n");
                    return -1;
                }
                
                is_ptr[stack_counter] = false;
#ifdef DEBUG
                printf("TL\n");
#endif 
                cons = get_cons(heap, offset);
                if (push(stack, &stack_counter, cons->tl) == -1){
                    printf("TL: Stack error!\n");
                    return -1;
                }
                pc++;
                break;
            default:
                printf("Encountered unknown command op-code.\n");
                return -1;
        }
#ifdef DEBUG
        usleep(10000);
#endif
    }
    return 0;
}

int main(int argc, char **argv) {

    uint8_t *byte_program = malloc(PROGRAM_SIZE * sizeof(uint8_t));
    FILE *fp;
    fp = fopen(argv[1], "rb");
    if (!fp) {
        printf("Could not open input that contains the byte program!\n");
        return -1;
    }

    int num_of_bytes = fread(byte_program, sizeof(uint8_t), PROGRAM_SIZE, fp);
    fclose(fp);
    run_program(num_of_bytes, byte_program);

    return 0;
}