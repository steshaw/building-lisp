#include <stdio.h>
#include <stdlib.h>
#include <string.h>
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

static const Atom nil = { AtomType_Nil };

Atom cons(Atom car, Atom cdr) {
  Atom p;
  p.type = AtomType_Pair;
  p.value.pair = malloc(sizeof(Pair)); // TODO: malloc returning NULL is unchecked
  car(p) = car;
  cdr(p) = cdr;
  return p;
}

Atom make_int(long i) {
  Atom a;
  a.type = AtomType_Integer;
  a.value.integer = i;
  return a;
}

static Atom sym_table = { AtomType_Nil };

Atom make_sym(const char s[]) {
  // Return symbol if it's already in the `sym_table`.
  Atom p = sym_table;
  while (!nilp(p)) {
    Atom a = car(p);
    if (strcmp(a.value.symbol, s) == 0) return a;
    p = cdr(p);
  }

  // Otherwise, create a new one and add it to the table.
  Atom a;
  a.type = AtomType_Symbol;
  a.value.symbol = strdup(s); // TODO: unchecked for NULL return.
  sym_table = cons(a, sym_table);

  return a;
}

void Atom_print(Atom atom) {
  switch (atom.type) {
    case AtomType_Nil: printf("NIL"); break;
    case AtomType_Pair:
      putchar('(');
      Atom_print(car(atom));
      atom = cdr(atom);
      while (!nilp(atom)) {
        if (atom.type == AtomType_Pair) {
          putchar(' ');
          Atom_print(car(atom));
          atom = cdr(atom);
        } else {
          printf(" . ");
          Atom_print(atom);
          break;
        }
      }
      putchar(')');
      break;
    case AtomType_Symbol:
      printf("%s", atom.value.symbol);
      break;
    case AtomType_Integer:
      printf("%ld", atom.value.integer);
      break;
  }
}

int main(int argc, const char* argv[]) {
  printf(
    "lisp version %d.%d.%d\n",
    LISP_VERSION_MAJOR,
    LISP_VERSION_MINOR,
    LISP_VERSION_PATCH
  );

  printf("> make_int(42)\n");
  Atom_print(make_int(42));
  putchar('\n');

  printf("> make_sym(\"FOO\")\n");
  Atom_print(make_sym("FOO"));
  putchar('\n');

  printf("> cons(make_sym(\"X\"), make_sym(\"Y\"))\n");
  Atom_print(cons(make_sym("X"), make_sym("Y")));
  putchar('\n');

  printf("> cons(make_int(1), cons(make_int(2), cons(make_int(3), nil)))\n");
  Atom_print(cons(make_int(1), cons(make_int(2), cons(make_int(3), nil))));
  putchar('\n');

  printf("> sym_table\n");
  Atom_print(sym_table);
  putchar('\n');
}
