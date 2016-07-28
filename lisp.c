#include "lisp_config.h"

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>
#include <stdbool.h>
#include <assert.h>

#include <readline/readline.h>
#include <readline/history.h>

// -----------------------------------------------------------------------------
// Atoms
// -----------------------------------------------------------------------------

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

// -----------------------------------------------------------------------------
// atom_print
// -----------------------------------------------------------------------------
void atom_print(Atom atom) {
  switch (atom.type) {
    case AtomType_Nil: printf("NIL"); break;
    case AtomType_Pair:
      putchar('(');
      atom_print(car(atom));
      atom = cdr(atom);
      while (!nilp(atom)) {
        if (atom.type == AtomType_Pair) {
          putchar(' ');
          atom_print(car(atom));
          atom = cdr(atom);
        } else {
          printf(" . ");
          atom_print(atom);
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

// -----------------------------------------------------------------------------
// Parser
// -----------------------------------------------------------------------------

typedef enum {
  Result_OK = 0,
  Error_Syntax,
  Error_Unbound,
  Error_Args,
  Error_Type
} Result;

Result lex(const char str[], const char **start, const char **end) {
  const char ws[] = " \t\n";
  const char delim[] = "() \t\n";
  const char prefix[] = "()";

  str += strspn(str, ws);
  if (str[0] == '\0') {
    *start = *end = NULL;
    return Error_Syntax;
  }

  *start = str;
  if (strchr(prefix, str[0]) != NULL) {
    *end = str + 1; // Recognises a "(" or "("
  } else {
    *end = str + strcspn(str, delim); // Recognise "other" token.
  }
  return Result_OK;
}

int read_expr(const char *input, const char **end, Atom *result);

int parse_simple(const char *start, const char *end, Atom *result) {
  // Is it an integer?
  char *p;
  long val = strtol(start, &p, 10);
  if (p == end) {
    result->type = AtomType_Integer;
    result->value.integer = val;
    return Result_OK;
  }

  // NIL or symbol
  char* buf = malloc(end - start + 1); // FIXME: NULL check.
  p = buf;
  while (start != end) {
    *p++ = toupper(*start);
    ++start;
  }
  *p = '\0';

  if (strcmp(buf, "NIL") == 0)
    *result = nil;
  else
    *result = make_sym(buf);

  free(buf);

  return Result_OK;
}

int read_list(const char *start, const char **end, Atom *result) {
  Atom p;

  *end = start;
  p = *result = nil;

  for (;;) {
    const char *token;
    Atom item;

    Result r = lex(*end, &token, end);
    if (r)
      return r;

    if (token[0] == ')')
      return Result_OK;

    if (token[0] == '.' && *end - token == 1) {
      // Improper list.
      if (nilp(p))
        return Error_Syntax;

      r = read_expr(*end, end, &item);
      if (r)
        return r;

      cdr(p) = item;

      // Read the closing ')'.
      r = lex(*end, &token, end);
      if (!r && token[0] != ')')
        r = Error_Syntax;

      return r;
    }

    r = read_expr(token, end, &item);
    if (r)
      return r;

    if (nilp(p)) {
      // First item.
      *result = cons(item, nil);
      p = *result;
    } else {
      cdr(p) = cons(item, nil);
      p = cdr(p);
    }
  }
}

int read_expr(const char *input, const char **end, Atom *result) {
  const char *token;
  Result r = lex(input, &token, end);
  if (r) return r;

  if (token[0] == '(')
    return read_list(*end, end, result);
  else if (token[0] == ')')
    return Error_Syntax;
  else
    return parse_simple(token, *end, result);
}

// -----------------------------------------------------------------------------
// environments
// -----------------------------------------------------------------------------

bool sym_eq(Atom sym1, Atom sym2) {
  assert(sym1.type == AtomType_Symbol);
  assert(sym2.type == AtomType_Symbol);

  return sym1.value.symbol == sym2.value.symbol;
}

Atom env_create(Atom parent) {
  return cons(parent, nil);
}

int env_get(Atom env, Atom symbol, Atom *result) {
  Atom parent = car(env);
  Atom bs = cdr(env);

  // Find in this environment's bindings (`bs`).
  while (!nilp(bs)) {
    Atom binding = car(bs);
    if (sym_eq(car(binding), symbol)) {
      *result = cdr(binding);
      return Result_OK;
    }
    bs = cdr(bs);
  }

  // Try parent environment.
  if (!nilp(parent)) return env_get(parent, symbol, result);

  return Error_Unbound;
}

int env_set(Atom env, Atom symbol, Atom value) {
  Atom bs = cdr(env);
  Atom binding = nil;

  while (!nilp(bs)) {
    binding = car(bs);
    if (sym_eq(car(binding), symbol)) {
      cdr(binding) = value;
      return Result_OK;
    }
    bs = cdr(bs);
  }

  // Binding not found -- create a new one.
  binding = cons(symbol, value);
  cdr(env) = cons(binding, cdr(env));

  return Result_OK;
}

// -----------------------------------------------------------------------------
// Evaluation
// -----------------------------------------------------------------------------

// Tests if the atom is a proper list.
bool listp(Atom a) {
   while (!nilp(a)) {
     if (a.type != AtomType_Pair) return false;
     a = cdr(a);
   }
   return true;
}

// -----------------------------------------------------------------------------
// Evaluation rules:
//   - a literal evaluates to itself.
//   - a symbol is looked up in the environment. It's an error if no binding exists.
//   - a list expression of one of the following is a _special form_:
//     - (QUOTE expr) => evaluates to expr (which is returned without evaluating).
//     - (DEFINE sym expr) => creates a binding for `sym` in the evaluation environment.
//                            The final result is `sym`.
// -----------------------------------------------------------------------------
Result eval_expr(Atom expr, Atom env, Atom *result) {
  if (expr.type == AtomType_Symbol) {
    return env_get(env, expr, result);
  } else if (expr.type != AtomType_Pair) {
    *result = expr;
    return Result_OK;
  }

  if (!listp(expr))
    return Error_Syntax;

  Atom op = car(expr);
  Atom args = cdr(expr);

  if (op.type == AtomType_Symbol) {
    if (strcmp(op.value.symbol, "QUOTE") == 0) {
      if (nilp(args) || !nilp(cdr(args)))
        return Error_Args;

      *result = car(args);
      return Result_OK;
    } else if (strcmp(op.value.symbol, "DEFINE") == 0) {
      if (nilp(args) || nilp(cdr(args)) || !nilp(cdr(cdr(args))))
        return Error_Args;

      Atom sym = car(args);
      if (sym.type != AtomType_Symbol) return Error_Type;

      Atom val;
      Result err = eval_expr(car(cdr(args)), env, &val);
      if (err) return err;

      *result = sym;
      return env_set(env, sym, val);
    }
  }

  return Error_Syntax;
}

// -----------------------------------------------------------------------------
// REPL
// -----------------------------------------------------------------------------
void repl() {
  Atom env = env_create(nil);
  char *input;
  while ((input = readline("λ> ")) != NULL) {
    if (strlen(input) != 0) {
      add_history(input);

      if (strcmp(input, ":q") == 0) {
        puts("bye");
        free(input);
        break;
      }

      const char* p = input;
      Atom expr;
      Result r = read_expr(p, &p, &expr);

      Atom result;
      if (!r) r = eval_expr(expr, env, &result);

      switch (r) {
        case Result_OK:
          atom_print(result);
          putchar('\n');
          break;
        case Error_Syntax:
          puts("Syntax error");
          break;
        case Error_Unbound:
          puts("Symbol not bound");
          break;
        case Error_Args:
          puts("Wrong number of arguments");
          break;
        case Error_Type:
          puts("Wrong type");
          break;
      }
    }
    free(input);
  }
}

// -----------------------------------------------------------------------------
// main
// -----------------------------------------------------------------------------
int main(int argc, const char* argv[]) {
  printf(
    "lisp version %d.%d.%d\n",
    LISP_VERSION_MAJOR,
    LISP_VERSION_MINOR,
    LISP_VERSION_PATCH
  );
  puts("bye");

  printf("> make_int(42)\n");
  atom_print(make_int(42));
  putchar('\n');

  printf("> make_sym(\"FOO\")\n");
  atom_print(make_sym("FOO"));
  putchar('\n');

  printf("> cons(make_sym(\"X\"), make_sym(\"Y\"))\n");
  atom_print(cons(make_sym("X"), make_sym("Y")));
  putchar('\n');

  printf("> cons(make_int(1), cons(make_int(2), cons(make_int(3), nil)))\n");
  atom_print(cons(make_int(1), cons(make_int(2), cons(make_int(3), nil))));
  putchar('\n');

  printf("> sym_table\n");
  atom_print(sym_table);
  putchar('\n');

  repl();
}
