#include <stdbool.h>

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

#define HEAP_SIZE 2097152

// -----------------------------------------------------------------------------
// Functions for the STACK.
// -----------------------------------------------------------------------------
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

// -----------------------------------------------------------------------------
// HEAP related.
// -----------------------------------------------------------------------------

typedef struct cons_cell {
    int hd, tl;
} cons_cell;

typedef struct Heap {
    cons_cell *from_space, *to_space, *middle, *start, *heap_position;
} Heap;

Heap * alloc_heap(int heap_size) {
    Heap * heap;
    heap->from_space = malloc(HEAP_SIZE*sizeof(cons_cell));
    heap->middle = heap->from_space + HEAP_SIZE/2;
    heap->to_space = heap->middle;
    heap->start = heap->from_space;
    heap->heap_position = heap->from_space;
    return heap;
}

int alloc_and_set_cons(Heap *heap, int hd, int tl){
    heap->heap_position->hd = hd;
    heap->heap_position->tl = tl;
    
    int offset = heap->heap_position - heap->from_space;
    
    heap->heap_position += sizeof(cons_cell);
    return offset;
}

cons_cell * get_cons(Heap * heap, int offset){
    cons_cell * cons;
    cons = (heap->from_space + offset);
    return cons;
}

/*
Heap collect(Heap heap, int *stack, int stack_counter, bool *is_ptr){

    for (int i=0; i<stack_counter; i++){
        if (is_ptr[stack[i]]) {
            stack[i] = ...;
        }
    }
    
    // swap spaces.

}
*/