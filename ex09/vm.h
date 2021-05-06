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
// HEAP related.
// -----------------------------------------------------------------------------

 typedef struct cons_cell {
    int hd, tl;
} cons_cell;

typedef struct Heap {
    cons_cell *from_space, *to_space, *middle, *start, *end, *heap_position;
} Heap;

Heap * alloc_heap() {
    Heap *heap = malloc(sizeof(Heap));
    heap->start = malloc(HEAP_SIZE*sizeof(cons_cell)); // Maintain the 1st cell in the heap
    heap->middle = heap->start + (div(HEAP_SIZE, 2).quot); // Maintain the middle cell in the heap
    heap->end = heap->start + HEAP_SIZE; // Maintain the last cell in the heap
    heap->from_space = heap->start;
    heap->to_space = heap->middle;
    heap->heap_position = heap->from_space;
    return heap;
}

int alloc_and_set_cons(Heap *heap, int hd, int tl){
    heap->heap_position->hd = hd;
    heap->heap_position->tl = tl;
    
    // We refer to cons cells in the heap using their offset from the
    // start of the heap, instead of using the whole 64bit address.
    int offset = heap->heap_position - heap->start;
    
    heap->heap_position++;
    return offset;
}

cons_cell * get_cons(Heap * heap, int offset){
    cons_cell * cons;
    cons = (heap->start + offset);
    return cons;
}

int to_ptr(int offset) {
/* 
 * Pointers will have their least significant bit set to 0.
 * We do this by shifting the input offset 1 bit to the left. 
 * The heap size is not very big, so the input offset is quite small.
 * So, no most significant bits are going to be lost after the shift.
 */
    return (offset << 1);
}

int to_non_ptr(int value) {
    /* 
     * Non pointer values will have their least significant bit set to 1.
     * We do this by shifting the input offset 1 bit to the left and setting the
     * least significant bit to 1.
     */
    return ((value << 1) | 0x01);
}

int get_original_value(int ptr) {
/* 
 * Shift a pointer or non-pointer 1 bit to the right, so that we obtain the
 * original value. 
 */
    return (ptr >> 1);
}

bool is_ptr(int value) {
    return ((value & 0x01) == 0);
}

bool points_at_to_space(Heap * heap, int offset) {
    cons_cell *address = heap->start + offset;
    return ((address >= heap->to_space) && (address < heap->to_space + div(HEAP_SIZE, 2).quot));
}

int collect(Heap * heap, int *stack, int stack_counter){
    //usleep(1000000);
    /*
    printf("ALL HEAP:\n");
    for(cons_cell * temp = heap->start; temp < heap->end; temp++){
        printf("HD=%d, TL=%d\n", temp->hd, temp->tl);
    }
    */ 
    cons_cell * cons, * to_space_position, * to_space_iterator;

    // Points at the next cons_cell in to-space that is free.
    // Will be used for copying cons_cells from "from-space" to "to-space".  
    to_space_position = heap->to_space;
    
    for (int i=0; i<stack_counter; i++){
        if (is_ptr(stack[i])) {
            //printf("Pointer at stack index == %d\n", i);
            
            // We get the "from-space" cell.
            cons = get_cons(heap, get_original_value(stack[i]));
            //printf("It points at: HD=%d, TL=%d\n", cons->hd, cons->tl);
            // FIXME
            if (points_at_to_space(heap, get_original_value(stack[i]))){
                //printf("I got here!\n");
                continue;
            }
            if (is_ptr(cons->hd) && points_at_to_space(heap, get_original_value(cons->hd))) {
                // If the head of the cons cell points at "to-space", this means that
                // we have already copied that specific cons cell to "to-space" 
                // (we always "mark" the head of the cons cell with the offset of the copied
                // cell in "to-space", so that we don't copy the same cell twice).
                stack[i] = cons->hd;
            } else {
                // In this case, we meet the "from-space" cell for the 1st time.
                // We copy the cell to "to_space".
                to_space_position->hd = cons->hd;
                to_space_position->tl = cons->tl;

                // We change the value of the stack element. Now it is an offset that points to
                // "to-space".
                stack[i] = to_ptr(to_space_position - heap->start);

                // We mark the head of the cons cell from "from-space". 
                // This way, we will know that we have already copied that cell.
                cons->hd = stack[i];
                to_space_position++;
            }
            /*
            printf("Now, it points at: HD=%d, TL=%d \n", cons->hd, cons->tl);
            printf("---PRINTING STACK---\n");
            for(int i=0; i<stack_counter; i++){
                printf("%d ", stack[i]);
            }
            printf("\n");
            */
        }
    }

    // Will be used to iterate over the newly allocated cons_cells in to-space and check
    // if their hd or tl are pointers to "from-space".
    to_space_iterator  = heap->to_space;
    //printf("Scanning to-space!\n");
    //printf("Initially, we have %d cons cells!\n", (int)(to_space_position - heap->to_space));

    while(to_space_iterator < to_space_position){
        //printf("Scanning %d of %d!\n", (int)(to_space_iterator - heap->to_space + 1), (int)(to_space_position - heap->to_space));        
        //printf("HD=%d, TL=%d\n", to_space_iterator->hd, to_space_iterator->tl);        
        if (is_ptr(to_space_iterator->hd)) {
            cons = get_cons(heap, get_original_value(to_space_iterator->hd));
            if(is_ptr(cons->hd) && points_at_to_space(heap, get_original_value(cons->hd))) {
                to_space_iterator->hd = cons->hd;
            } else {
                to_space_position->hd = cons->hd;
                to_space_position->tl = cons->tl;
                to_space_iterator->hd = to_ptr(to_space_position - heap->start);
                to_space_position++;
            }
        }

        if (is_ptr(to_space_iterator->tl)) {
            //printf("TL IS A POINTER!\n");
            //printf("%d \n", get_original_value(to_space_iterator->tl));

            if(points_at_to_space(heap, get_original_value(to_space_iterator->tl))){
                to_space_iterator++;
                //printf("TL POINTS AT TO SPACE AT OFFSET: %d\n", get_original_value(to_space_iterator->tl));
                //usleep(1000000);
                continue;
            }
            cons = get_cons(heap, get_original_value(to_space_iterator->tl));
            //printf("CONS OF TL: HD=%d, TL=%d \n", cons->hd, cons->tl);
            if(is_ptr(cons->hd) && points_at_to_space(heap, get_original_value(cons->hd))) {
                to_space_iterator->tl = cons->hd;
            } else {
                to_space_position->hd = cons->hd;
                to_space_position->tl = cons->tl;
                to_space_iterator->tl = to_ptr(to_space_position - heap->start);
                to_space_position++;
            }
        }
        to_space_iterator++;
    }

    // Swap spaces in heap.
    if(heap->to_space == heap->middle && heap->from_space == heap->start){
        heap->to_space = heap->start;
        heap->from_space = heap->middle;
    } else if (heap->to_space == heap->start && heap->from_space == heap->middle) {
        heap->from_space = heap->start;
        heap->to_space = heap->middle;
    }
    else {
        printf("Error while swapping spaces in heap.\n");
        return -1;
    }

    heap->heap_position = to_space_position;
    
    return 0;
}

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
    // *stack_element = get_original_value(*stack_element);
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
