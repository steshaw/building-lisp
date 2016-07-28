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
  AtomType_Integer,
  AtomType_Builtin,
  AtomType_Closure
} AtomType;

typedef struct Pair Pair;
typedef struct Atom Atom;

typedef int (*Builtin)(Atom args, Atom *result);

struct Atom {
  AtomType type;
  union {
    Pair *pair;
    const char* symbol;
    long integer;
    Builtin builtin;
  } value;
};

struct Pair {
  Atom atom[2];
};

#define car(p) ((p).value.pair->atom[0])
#define cdr(p) ((p).value.pair->atom[1])
#define nilp(atom) ((atom).type == AtomType_Nil)

static const Atom nil = { AtomType_Nil };

#define TRUE_SYM make_sym("T")

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

typedef enum {
  Result_OK = 0,
  Error_Syntax,
  Error_Unbound,
  Error_Args,
  Error_Type
} Result;

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

// Tests if the atom is a proper list.
bool listp(Atom a) {
   while (!nilp(a)) {
     if (a.type != AtomType_Pair) return false;
     a = cdr(a);
   }
   return true;
}

int make_closure(Atom env, Atom args, Atom body, Atom *result) {
  if (!listp(args) || (!listp(body))) return Error_Syntax;

  // Check argument names are all symbols.
  for (Atom p = args; !nilp(p); p = cdr(p)) {
    if (car(p).type != AtomType_Symbol) return Error_Type;
  }

  *result = cons(env, cons(args, body));
  result->type = AtomType_Closure; // Clobber AtomType_Pair with AtomType_Closure.

  return Result_OK;
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
    case AtomType_Builtin:
      printf("#<BUILTIN:%p>", atom.value.builtin);
      break;
    case AtomType_Closure:
      printf("#<CLOSURE>\n");
      Atom as_list = atom;
      as_list.type = AtomType_Pair;
      atom_print(as_list);
      break;
  }
}

// -----------------------------------------------------------------------------
// Parser
// -----------------------------------------------------------------------------

Result lex(const char str[], const char **start, const char **end) {
  const char ws[] = " \t\n";
  const char delim[] = "() \t\n";
  const char prefix[] = "()'";

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
  else if (token[0] == '\'') {
    *result = cons(make_sym("QUOTE"), cons(nil, nil));
    return read_expr(*end, end, &car(cdr(*result))); // XXX: Hmm, clobber previous pair.
  } else
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
// builtin functions
// -----------------------------------------------------------------------------

Atom make_builtin(Builtin f) {
  Atom a;
  a.type = AtomType_Builtin;
  a.value.builtin = f;
  return a;
}

// Create a _shallow_ copy of the argument list.
Atom copy_list(Atom list) {
  if (nilp(list)) return nil;

  Atom a = cons(car(list), nil);
  Atom p = a;
  list = cdr(list);

  while (!nilp(list)) {
    cdr(p) = cons(car(list), nil);
    p = cdr(p);
    list = cdr(list);
  }

  return a;
}

Result eval_expr(Atom expr, Atom env, Atom *result);

int apply(Atom f, Atom args, Atom *result) {
  if (f.type == AtomType_Builtin) {
    return (*f.value.builtin)(args, result);
  } else if (f.type == AtomType_Closure) {
    Atom env = env_create(car(f));
    Atom arg_names = car(cdr(f));
    Atom body = cdr(cdr(f));

    // Bind the arguments in the new environment.
    while (!nilp(arg_names)) {
      if (nilp(args)) return Error_Args;
      env_set(env, car(arg_names), car(args));
      arg_names = cdr(arg_names);
      args = cdr(args);
    }

    if (!nilp(args)) return Error_Args; // Too many args.

    // Evaluate the body (body is a sequence of expressions).
    while (!nilp(body)) {
      Result r = eval_expr(car(body), env, result);
      if (r) return r;
      body = cdr(body);
    }

    return Result_OK;
  }
  return Error_Type;
}

#define ENSURE_0_ARGS() \
  if (!nilp(args)) return Error_Args

#define ENSURE_1_ARG() \
  if (nilp(args) || !nilp(cdr(args))) return Error_Args

#define ENSURE_2_ARGS() \
  if (nilp(args) || nilp(cdr(args)) || !nilp(cdr(cdr(args)))) return Error_Args

#define ENSURE_3_ARGS() \
  if (nilp(args) || nilp(cdr(args)) || nilp(cdr(cdr(args))) || !nilp(cdr(cdr(cdr(args))))) \
    return Error_Args

int car_builtin(Atom args, Atom *result) {
  ENSURE_1_ARG();

  Atom arg = car(args);

  if (nilp(arg)) *result = nil;
  else if (arg.type != AtomType_Pair) return Error_Type;
  else *result = car(arg);
  return Result_OK;
}

int cdr_builtin(Atom args, Atom *result) {
  ENSURE_1_ARG();

  Atom arg = car(args);

  if (nilp(arg)) *result = nil;
  else if (arg.type != AtomType_Pair) return Error_Type;
  else *result = cdr(arg);
  return Result_OK;
}

int cons_builtin(Atom args, Atom *result) {
  ENSURE_2_ARGS();

  Atom a1 = car(args);
  Atom a2 = car(cdr(args));

  *result = cons(a1, a2);

  return Result_OK;
}

#define INTEGER_BINOP(FN_NAME, BINOP) \
  int FN_NAME(Atom args, Atom *result) { \
    ENSURE_2_ARGS(); \
\
    Atom i1 = car(args); \
    Atom i2 = car(cdr(args)); \
\
    if (i1.type != AtomType_Integer || i2.type != AtomType_Integer) return Error_Type; \
\
    *result = make_int(i1.value.integer BINOP i2.value.integer); \
\
    return Result_OK; \
  }

INTEGER_BINOP(add_builtin, +)
INTEGER_BINOP(sub_builtin, -)
INTEGER_BINOP(mul_builtin, *)
INTEGER_BINOP(div_builtin, /)

#define INTEGER_RELOP(FN_NAME, BINOP) \
  int FN_NAME(Atom args, Atom *result) { \
    ENSURE_2_ARGS(); \
\
    Atom i1 = car(args); \
    Atom i2 = car(cdr(args)); \
\
    if (i1.type != AtomType_Integer || i2.type != AtomType_Integer) return Error_Type; \
\
    *result = i1.value.integer BINOP i2.value.integer ? TRUE_SYM : nil; \
\
    return Result_OK; \
  }

INTEGER_RELOP(integer_eq_builtin, ==)
INTEGER_RELOP(integer_lt_builtin, <)
INTEGER_RELOP(integer_le_builtin, <=)
INTEGER_RELOP(integer_gt_builtin, >)
INTEGER_RELOP(integer_ge_builtin, >=)

// -----------------------------------------------------------------------------
// Evaluation
// -----------------------------------------------------------------------------

// -----------------------------------------------------------------------------
// Evaluation rules:
//   - a literal evaluates to itself.
//   - a symbol is looked up in the environment. It's an error if no binding exists.
//   - a list expression of one of the following is a _special form_:
//     - (QUOTE expr) => evaluates to expr (which is returned without evaluating).
//     - (DEFINE sym expr) => creates a binding for `sym` in the evaluation environment.
//                            The final result is `sym`.
//     - (LAMBDA args body...)
//     - (IF cond when_true when_false)
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
    } else if (strcmp(op.value.symbol, "LAMBDA") == 0) {
      if (nilp(args) || nilp(cdr(args))) return Error_Args;
      return make_closure(env, car(args), cdr(args), result);
    } else if (strcmp(op.value.symbol, "IF") == 0) {
      ENSURE_3_ARGS();

      Atom cond = car(args);
      Atom when_t = car(cdr(args));
      Atom when_f = car(cdr(cdr(args)));

      Result r = eval_expr(cond, env, &cond);
      if (r) return r;
      Atom e = nilp(cond) ? when_f : when_t; // NIL is falsy, all other values of truthy.
      return eval_expr(e, env, result);
    }
  }

  // Evaluate the operator.
  Result r = eval_expr(op, env, &op);
  if (r) return r;

  // Evaluate arguments.
  args = copy_list(args); // Shallow copy because we clobber the args with the
                          // evaluated args.
  for (Atom p = args; !nilp(p); p = cdr(p)) {
    r = eval_expr(car(p), env, &car(p));
    if (r) return r;
  }

  return apply(op, args, result);
}

// -----------------------------------------------------------------------------
// REPL
// -----------------------------------------------------------------------------
void repl(Atom env) {
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
// old unit test, now a builtin function
// -----------------------------------------------------------------------------

void unit_test_1() {
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
}

int unit_test_1_builtin(Atom args, Atom *result) {
  ENSURE_0_ARGS();

  unit_test_1();

  *result = TRUE_SYM;
  return Result_OK;
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

  // Initial environment.
  Atom env = env_create(nil);
  env_set(env, make_sym("CAR"), make_builtin(car_builtin));
  env_set(env, make_sym("CDR"), make_builtin(cdr_builtin));
  env_set(env, make_sym("CONS"), make_builtin(cons_builtin));
  env_set(env, make_sym("UNIT-TEST-1"), make_builtin(unit_test_1_builtin));

  env_set(env, make_sym("+"), make_builtin(add_builtin));
  env_set(env, make_sym("-"), make_builtin(sub_builtin));
  env_set(env, make_sym("*"), make_builtin(mul_builtin));
  env_set(env, make_sym("/"), make_builtin(div_builtin));

  env_set(env, make_sym("="), make_builtin(integer_eq_builtin));
  env_set(env, make_sym("<"), make_builtin(integer_lt_builtin));
  env_set(env, make_sym("<="), make_builtin(integer_le_builtin));
  env_set(env, make_sym(">"), make_builtin(integer_gt_builtin));
  env_set(env, make_sym(">="), make_builtin(integer_ge_builtin));

  env_set(env, TRUE_SYM, TRUE_SYM);

  repl(env);
}
