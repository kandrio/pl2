#include <stdio.h>
#include <stdint.h>
#include "vm.h"

int main(){
    int stack_size = 20;
    int stack[stack_size];
    int stack_counter = 0;
    int stack_element;
    

    printf("Testing push()...\n");

    for(int i=0; i<stack_size; i++){
        if (push(stack, &stack_counter, i) == -1){
            printf("PUSH: Stack error!");
            return -1;
        }
    }

    printf("Stack: ");
    for(int i=0; i<stack_counter; i++)
        printf("%d ", stack[i]);
    printf("\n");


    printf("Testing pop()...\n");

    printf("Popped Elements: ");
    for(int i=0; i<stack_size; i++){
        if (pop(stack, &stack_counter, &stack_element) == -1){
            printf("POP: Stack error!");
            return -1;
        }
        printf("%d ", stack_element);
    }
    printf("\n");
}