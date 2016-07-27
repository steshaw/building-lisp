#include <stdio.h>
#include <stdlib.h>
#include "lisp_config.h"

typedef enum {
  AtomType_Nil,
  AtomType_Pair,
  AtomType_Symbol,
  AtomType_Integer
} AtomType;

typedef struct Pair Pair;

typedef struct {
  AtomType type;
  union {
    Pair *pair;
    const char* symbol;
    long integer;
  } value;
} Atom;

struct Pair {
  Atom atom[2];
};

#define car(p) ((p).value.pair->atom[0])
#define cdr(p) ((p).value.pair->atom[1])
#define nilp(atom) ((atom).type == AtomType_Nil)

Atom cons(Atom car, Atom cdr) {
  Atom p;
  p.type = AtomType_Pair;
  p.value.pair = malloc(sizeof(Pair)); // TODO: malloc returning NULL is unchecked
  car(p) = car;
  cdr(p) = cdr;
  return p;
}

int main(int argc, const char* argv[]) {
  printf("lisp version %d.%d.%d\n", LISP_VERSION_MAJOR, LISP_VERSION_MINOR, LISP_VERSION_PATCH);
}
