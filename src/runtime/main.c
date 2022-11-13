#include "tgc.h"

enum Type {INT, FLOAT, CHAR, BOOL, STRING, ARRAY, STRUCT};
// So think about a way to contain structs in here cause no fucking idea
typedef enum Type Type;

struct Array  {
  void* data;
  int len;
  int cap;
  Type type;
};

typedef struct Array Array;
static tgc_t gc;

int sizeArrType(Type type){
    switch(type){ // Yeah, won't work for Structs 
        case INT: return sizeof(int64_t);
        case FLOAT: return sizeof(double);
        case CHAR: return sizeof(char); // 1 byte should be
        case BOOL: return sizeof(char);
        case STRING:
        case ARRAY: return sizeof(Array);
        case STRUCT: return -1; // For now, but in future, generate additional array for sizes of all structs and get idx + STRUCT of it
    }
}

Array create(Type type ,int size, tgc_t* gc){
    return (Array){
        tgc_calloc(gc,size,sizeArrType(type)),
        0,
        size,
        type
    };
}

int main(){
  Array arr = create(INT, 2,&gc);
  return 0;
}