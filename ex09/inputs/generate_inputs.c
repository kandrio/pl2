#include <stdio.h>
#include <stdint.h>
#include <stdlib.h>
#include <unistd.h>
#include <time.h>

int main(int argc, char **argv){
    FILE *fp;
    fp = fopen(argv[1], "wb");
    
    //PUSH1 't'
    char c = 0x08;
    fwrite(&c, 1, sizeof(c), fp);
    c = 't';
    fwrite(&c, 1, sizeof(c), fp);
    
    //PUSH1 'h'
    c = 0x08;
    fwrite(&c, 1, sizeof(c), fp);
    c = 'h';  
    fwrite(&c, 1, sizeof(c), fp);
    
    //PUSH1 'T'
    c = 0x08;
    fwrite(&c, 1, sizeof(c), fp);
    c = 'T';
    fwrite(&c, 1, sizeof(c), fp);
    
    //PUSH1 'H'
    c = 0x08;
    fwrite(&c, 1, sizeof(c), fp);
    c = 'H';  
    fwrite(&c, 1, sizeof(c), fp);
    
    //CONS (HD=H,TL=T)
    c = 0x30;
    fwrite(&c, 1, sizeof(c), fp);
    //HEAD 'H'
    c = 0x31;
    fwrite(&c, 1, sizeof(c), fp);
    //OUTPUT 'H'
    c = 0x18;
    fwrite(&c, 1, sizeof(c), fp);
    
    //CONS (HD=h,TL=t)
    c = 0x30;
    fwrite(&c, 1, sizeof(c), fp);
    //TL 't'
    c = 0x32;
    fwrite(&c, 1, sizeof(c), fp);
    //OUTPUT 't'
    c = 0x18;
    fwrite(&c, 1, sizeof(c), fp);
    
    //HALT
    c = 0x00;
    fwrite(&c, 1, sizeof(c), fp);
}