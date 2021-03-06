#include "lisp_config.h"

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <strings.h>
#include <ctype.h>
#include <stdbool.h>
#include <stddef.h>
#include <assert.h>

#include <readline/readline.h>
#include <readline/history.h>

typedef struct Pair Pair;
typedef struct Atom Atom;

void atom_print(Atom atom);

// -----------------------------------------------------------------------------
// Atoms
// -----------------------------------------------------------------------------

typedef enum {
  AtomType_Nil,
  AtomType_Pair,
  AtomType_Symbol,
  AtomType_Integer,
  AtomType_Builtin,
  AtomType_Closure,
  AtomType_Macro
} AtomType;

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

typedef struct Allocation Allocation;

struct Allocation {
  Pair pair;
  int mark : 1;
  Allocation *next;
};

static Allocation* last_allocation = NULL;

Atom cons(Atom car, Atom cdr) {
  // Track pair allocations on global linked list `last_allocation`.
  Allocation* a = malloc(sizeof(Allocation)); // TODO: NULL check.
  a->mark = 0;
  a->next = last_allocation;
  last_allocation = a;

  Atom p;
  p.type = AtomType_Pair;
  p.value.pair = &a->pair;
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
  if ((!listp(body))) return Error_Syntax;

  // Check argument names are all symbols.
  for (Atom p = args; !nilp(p); p = cdr(p)) {
    if (p.type == AtomType_Symbol) break; // Handle variadic arguments.
    else if (p.type != AtomType_Pair || car(p).type != AtomType_Symbol) {
      printf("Expected type pair or symbol in args");
      return Error_Type;
    }
  }

  *result = cons(env, cons(args, body));
  result->type = AtomType_Closure; // Clobber AtomType_Pair with AtomType_Closure.

  return Result_OK;
}

// -----------------------------------------------------------------------------
// Garbage collection.
// -----------------------------------------------------------------------------

void gc_mark(Atom root) {
  switch (root.type) {
    case AtomType_Pair:
    case AtomType_Closure:
    case AtomType_Macro: {
      Allocation* a = (Allocation*)root.value.pair - offsetof(Allocation, pair);

      if (a->mark) break;
      a->mark = 1;

      gc_mark(car(root));
      gc_mark(cdr(root));
      break;
    }
    default:
      break;
  }
}

void gc(Atom expr, Atom env, Atom stack) {
  gc_mark(expr);
  gc_mark(env);
  gc_mark(stack);
  gc_mark(sym_table);

  // Sweep - free unmarked allocations.
  Allocation **p = &last_allocation;
  while (*p != NULL) {
    Allocation* a = *p;
    if (!a->mark) {
      *p = a->next;

      // XXX: What happens when we free the `root` pair?
      // XXX: i.e. `last_allocation`
      if (a == last_allocation) {
        printf("a == last_allocation!!!\n");
      }

      free(a);
    } else {
      p = &a->next; // XXX: seems v.similar to *p = a->next;
    }
  }

  // Clear marks.
  // XXX: Couldn't we clear marks and sweep in one iteration?
  for (Allocation* a = last_allocation; a != NULL; a = a->next) {
    a->mark = 0;
  }
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
    case AtomType_Closure: {
      printf("#<CLOSURE>\n");
      Atom as_list = atom;
      as_list.type = AtomType_Pair;
      atom_print(as_list);
      break;
    }
    case AtomType_Macro: {
      printf("#<MACRO>\n");
      Atom as_list = atom;
      as_list.type = AtomType_Pair;
      atom_print(as_list);
      break;
    }
  }
}

// -----------------------------------------------------------------------------
// Parser
// -----------------------------------------------------------------------------

Result lex(const char str[], const char **start, const char **end) {
  const char ws[] = " \t\n";
  const char delim[] = "() \t\n";
  const char prefix[] = "()'`";

  str += strspn(str, ws);
  if (str[0] == '\0') {
    *start = *end = str;
    return Result_OK;
  }

  *start = str;

  if (strchr(prefix, str[0]) != NULL) {
    *end = str + 1; // Recognises any single prefix char as a token.
  } else if (str[0] == ',') {
    // Regcognise both unquote "," and unquote-splicing ",@".
    *end = str + (str[1] == '@'? 2 : 1);
  } else if (str[0] == ';') {
    const char *s = strchr(str, '\n');
    if (s != NULL) {
      return lex(s, start, end);
    } else {
      str = strchr(str, '\0');
      *start = *end = str;
      return Result_OK;
    }
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

  if (token[0] == '\0')
    return Error_Syntax;
  else if (token[0] == '(')
    return read_list(*end, end, result);
  else if (token[0] == ')')
    return Error_Syntax;
  else if (token[0] == '\'') {
    *result = cons(make_sym("QUOTE"), cons(nil, nil));
    return read_expr(*end, end, &car(cdr(*result))); // XXX: Hmm, clobber previous pair.
  } else if (token[0] == '`') {
    *result = cons(make_sym("QUASIQUOTE"), cons(nil, nil));
    Result r = read_expr(*end, end, &car(cdr(*result))); // XXX: Hmm, clobber previous pair.
    return r;
  } else if (token[0] == ',') {
    *result = cons(make_sym(token[1] == '@'? "UNQUOTE-SPLICING" : "UNQUOTE"),
                   cons(nil, nil));
    Result r = read_expr(*end, end, &car(cdr(*result))); // XXX: Hmm, clobber previous pair.
    return r;
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

  printf("Symbol '%s' is not bound\n", symbol.value.symbol);
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
      if (arg_names.type == AtomType_Symbol) {
        // Process in improper list which gets the rest of the args.
        Atom sym = arg_names;
        env_set(env, sym, args);
        args = nil; // Don't trip up below on "Too many args".
        break;
      } else {
        if (nilp(args)) return Error_Args;
        env_set(env, car(arg_names), car(args));
        arg_names = cdr(arg_names);
        args = cdr(args);
      }
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
  printf("Expecting type builtin or closure in apply");
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

int apply_builtin(Atom args, Atom *result) {
  ENSURE_2_ARGS();

  Atom fn = car(args);
  args = car(cdr(args));

  if (!listp(args)) return Error_Syntax;

  return apply(fn, args, result);
}

int car_builtin(Atom args, Atom *result) {
  ENSURE_1_ARG();

  Atom arg = car(args);

  if (nilp(arg) || arg.type != AtomType_Pair) *result = nil;
  else if (arg.type != AtomType_Pair) {
    printf("Expecting a pair in car\n");
    return Error_Type;
  } else *result = car(arg);
  return Result_OK;
}

int cdr_builtin(Atom args, Atom *result) {
  ENSURE_1_ARG();

  Atom arg = car(args);

  if (nilp(arg) || arg.type != AtomType_Pair) *result = nil;
  else if (arg.type != AtomType_Pair) {
    printf("Expecting a pair in cdr\n");
    return Error_Type;
  } else *result = cdr(arg);
  return Result_OK;
}

int cons_builtin(Atom args, Atom *result) {
  ENSURE_2_ARGS();

  Atom a1 = car(args);
  Atom a2 = car(cdr(args));

  *result = cons(a1, a2);

  return Result_OK;
}

Atom boolToTF(bool b) {
    return b ? TRUE_SYM : nil;
}

int pairp_builtin(Atom args, Atom *result) {
  ENSURE_1_ARG();

  Atom p = car(args);

  *result = boolToTF(p.type == AtomType_Pair);

  return Result_OK;
}

int eqp_builtin(Atom args, Atom *result) {
  ENSURE_2_ARGS();

  Atom a1 = car(args);
  Atom a2 = car(cdr(args));

  if (a1.type != a2.type) {
    *result = boolToTF(false);
    return Result_OK;
  } else {
    switch (a1.type) {
      case AtomType_Nil:
        *result = boolToTF(true);
        break;
      case AtomType_Pair:
      case AtomType_Closure:
      case AtomType_Macro:
        *result = boolToTF(a1.value.pair == a2.value.pair);
        break;
      case AtomType_Symbol:
        *result = boolToTF(a1.value.symbol == a2.value.symbol);
        break;
      case AtomType_Integer:
        *result = boolToTF(a1.value.integer == a2.value.integer);
        break;
      case AtomType_Builtin:
        *result = boolToTF(a1.value.builtin == a2.value.builtin);
        break;
    }
  }

  return Result_OK;
}

#define INTEGER_BINOP(FN_NAME, BINOP) \
  int FN_NAME(Atom args, Atom *result) { \
    ENSURE_2_ARGS(); \
\
    Atom i1 = car(args); \
    Atom i2 = car(cdr(args)); \
\
    if (i1.type != AtomType_Integer || i2.type != AtomType_Integer) { \
      printf("Expecting two integers in binop\n"); \
      return Error_Type; \
    } \
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
    if (i1.type != AtomType_Integer || i2.type != AtomType_Integer) { \
      printf("Expecting two integers in relop\n"); \
      return Error_Type; \
    } \
\
    *result = boolToTF(i1.value.integer BINOP i2.value.integer); \
\
    return Result_OK; \
  }

INTEGER_RELOP(integer_eq_builtin, ==)
INTEGER_RELOP(integer_lt_builtin, <)
INTEGER_RELOP(integer_le_builtin, <=)
INTEGER_RELOP(integer_gt_builtin, >)
INTEGER_RELOP(integer_ge_builtin, >=)

// -----------------------------------------------------------------------------
// Stack frames.
// -----------------------------------------------------------------------------

Atom list_get(Atom list, int k) {
  while (k--)
    list = cdr(list);
  return car(list);
}

void list_set(Atom list, int k, Atom value) {
  while (k--)
    list = cdr(list);
  car(list) = value;
}

void list_reverse(Atom *list) {
  Atom tail = nil;
  while (!nilp(*list)) {
    Atom p = cdr(*list);
    cdr(*list) = tail;
    tail = *list;
    *list = p;
  }
  *list = tail;
}

//
// Frames take the following form:
//
//   (parent env evaluated-op (pending-arg...) (evaluated-arg...) (body...))
//
Atom make_frame(Atom parent, Atom env, Atom tail) {
  return cons(
    parent,
    cons(env,
    cons(nil, // op
    cons(tail,
    cons(nil, // args
    cons(nil, // body
    nil))))));
}

// -----------------------------------------------------------------------------
// Evaluation
//   - a literal evaluates to itself.
//   - a symbol is looked up in the environment. It's an error if no binding exists.
//   - a list expression of one of the following is a _special form_:
//     - (QUOTE expr) => evaluates to expr (which is returned without evaluating).
//     - (DEFINE sym expr) => creates a binding for `sym` in the evaluation environment.
//                            The final result is `sym`.
//     - (DEFINE (name args...) body...) => (DEFINE name (LAMBDA (args...) body...)
//     - (LAMBDA args body...)
//     - (IF cond when_true when_false)
//     - (DEFMACRO (name arg...) body...)
// -----------------------------------------------------------------------------

int eval_do_exec(Atom *stack, Atom *expr, Atom *env) {
  *env = list_get(*stack, 1);
  Atom body = list_get(*stack, 5);
  *expr = car(body);
  body = cdr(body);
  if (nilp(body)) {
    // Finished function - pop the stack.
    *stack = car(*stack);
  } else {
    list_set(*stack, 5, body);
  }

  return Result_OK;
}

int eval_do_bind(Atom *stack, Atom *expr, Atom *env) {
  Atom body = list_get(*stack, 5);
  if (!nilp(body))
    return eval_do_exec(stack, expr, env);

  Atom op = list_get(*stack, 2);
  Atom args = list_get(*stack, 4);

  *env = env_create(car(op));
  Atom arg_names = car(cdr(op));
  body = cdr(cdr(op));
  list_set(*stack, 1, *env);
  list_set(*stack, 5, body);

  // Bind the arguments.
  while (!nilp(arg_names)) {
    if (arg_names.type == AtomType_Symbol) {
      env_set(*env, arg_names, args);
      args = nil;
      break;
    }

    if (nilp(args))
      return Error_Args;
    env_set(*env, car(arg_names), car(args));
    arg_names = cdr(arg_names);
    args = cdr(args);
  }
  if (!nilp(args))
    return Error_Args;

  list_set(*stack, 4, nil);

  return eval_do_exec(stack, expr, env);
}

int eval_do_apply(Atom *stack, Atom *expr, Atom *env, Atom *result) {
  Atom op = list_get(*stack, 2);
  Atom args = list_get(*stack, 4);

  if (!nilp(args)) {
    list_reverse(&args);
    list_set(*stack, 4, args);
  }

  if (op.type == AtomType_Symbol) {
    if (strcmp(op.value.symbol, "APPLY") == 0) {
      // Replace the current frame.
      *stack = car(*stack);
      *stack = make_frame(*stack, *env, nil);
      op = car(args);
      args = car(cdr(args));
      if (!listp(args))
        return Error_Syntax;

      list_set(*stack, 2, op);
      list_set(*stack, 4, args);
    }
  }

  if (op.type == AtomType_Builtin) {
    *stack = car(*stack);
    *expr = cons(op, args);
    return Result_OK;
  } else if (op.type != AtomType_Closure) {
    printf("Expecting closure\n");
    return Error_Type;
  }

  return eval_do_bind(stack, expr, env);
}

int eval_do_return(Atom *stack, Atom *expr, Atom *env, Atom *result) {
  *env = list_get(*stack, 1);
  Atom op = list_get(*stack, 2);
  Atom body = list_get(*stack, 5);

  if (!nilp(body)) {
    // Still running a procedure; ignore the result.
    return eval_do_apply(stack, expr, env, result);
  }

  Atom args;
  if (nilp(op)) {
    // Finished evaluating operator.
    op = *result;
    list_set(*stack, 2, op);

    if (op.type == AtomType_Macro) {
      // Don't evaluate macro arguments.
      args = list_get(*stack, 3);
      *stack = make_frame(*stack, *env, nil);
      op.type = AtomType_Closure;
      list_set(*stack, 2, op);
      list_set(*stack, 4, args);
      return eval_do_bind(stack, expr, env);
    }
  } else if (op.type == AtomType_Symbol) {
    // Finished working on special form.
    if (strcmp(op.value.symbol, "DEFINE") == 0) {
      Atom sym = list_get(*stack, 4);
      (void) env_set(*env, sym, *result);
      *stack = car(*stack);
      *expr = cons(make_sym("QUOTE"), cons(sym, nil));
      return Result_OK;
    } else if (strcmp(op.value.symbol, "IF") == 0) {
      args = list_get(*stack, 3);
      *expr = nilp(*result) ? car(cdr(args)) : car(args);
      *stack = car(*stack);
      return Result_OK;
    } else {
      goto store_arg;
    }
  } else if (op.type == AtomType_Macro) {
    // Finished evaluating macro.
    *expr = *result;
    *stack = car(*stack);
    return Result_OK;
  } else {
store_arg:
    // Store evaluated argument.
    args = list_get(*stack, 4);
    list_set(*stack, 4, cons(*result, args));
  }

  args = list_get(*stack, 3);
  if (nilp(args)) {
    // No more arguments left to evaluate.
    return eval_do_apply(stack, expr, env, result);
  }

  // Evaluate next argument.
  *expr = car(args);
  list_set(*stack, 3, cdr(args));
  return Result_OK;
}

Result eval_expr(Atom expr, Atom env, Atom *result) {
  static int count = 0;
  Result err = Result_OK;
  Atom stack = nil;

  do {
    if (++count == 10000) {
      gc(expr, env, stack);
      count = 0;
    }
    if (expr.type == AtomType_Symbol) {
      err = env_get(env, expr, result);
    } else if (expr.type != AtomType_Pair) {
      *result = expr;
    } else if (!listp(expr)) {
      return Error_Syntax;
    } else {
      Atom op = car(expr);
      Atom args = cdr(expr);

      if (op.type == AtomType_Symbol) {
        // Handle special forms.
        if (strcmp(op.value.symbol, "GC") == 0) {
          ENSURE_0_ARGS();
          gc(expr, env, stack);
          *result = TRUE_SYM;
          return Result_OK;
        } else if (strcmp(op.value.symbol, "QUOTE") == 0) {
          ENSURE_1_ARG();
          *result = car(args);
        } else if (strcmp(op.value.symbol, "DEFINE") == 0) {
          if (nilp(args) || nilp(cdr(args))) return Error_Args; // At least two args.

          Atom sym = car(args);
          if (sym.type == AtomType_Pair) {
            // (DEFINE (name args...) body...)
            err = make_closure(env, cdr(sym), cdr(args), result);
            sym = car(sym);
            if (sym.type != AtomType_Symbol) {
              printf("DEFINE expecting symbol\n");
              return Error_Type;
            }
            (void) env_set(env, sym, *result);
            *result = sym;
          } else if (sym.type == AtomType_Symbol) {
            // (DEFINE sym expr)
            ENSURE_2_ARGS();
            stack = make_frame(stack, env, nil);
            list_set(stack, 2, op);
            list_set(stack, 4, sym);
            expr = car(cdr(args));
            continue;
          } else {
            printf("Invalid type for DEFINE\n");
            return Error_Type;
          }
        } else if (strcmp(op.value.symbol, "LAMBDA") == 0) {
          if (nilp(args) || nilp(cdr(args))) return Error_Args;
          err = make_closure(env, car(args), cdr(args), result);
        } else if (strcmp(op.value.symbol, "IF") == 0) {
          ENSURE_3_ARGS();

          stack = make_frame(stack, env, cdr(args));
          list_set(stack, 2, op);
          expr = car(args);
          continue;
        } else if (strcmp(op.value.symbol, "DEFMACRO") == 0) {

          if (nilp(args) || nilp(cdr(args)))
            return Error_Args;

          if (car(args).type != AtomType_Pair) {
            printf("Expecting symbol in DEFMACRO\n");
            return Error_Syntax;
          }

          Atom name = car(car(args));
          if (name.type != AtomType_Symbol) {
            printf("DEFMACRO expecting symbol\n");
            return Error_Type;
          }

          Atom macro;
          err = make_closure(env, cdr(car(args)), cdr(args), &macro);
          if (!err) {
            macro.type = AtomType_Macro; // Clobber AtomType_Closure.
            *result = name;
            (void) env_set(env, name, macro);
          }
        } else if (strcmp(op.value.symbol, "APPLY") == 0) {
          if (nilp(args) || nilp(cdr(args)) || !nilp(cdr(cdr(args))))
            return Error_Args;

          stack = make_frame(stack, env, cdr(args));
          list_set(stack, 2, op);
          expr = car(args);
          continue;
        } else {
          goto push;
        }
      } else if (op.type == AtomType_Builtin) {
        err = (*op.value.builtin)(args, result);
      } else {
      push:
        // Handle function application.
        stack = make_frame(stack, env, args);
        expr = op;
        continue;
      }
    }

    if (nilp(stack))
      break;

    if (!err)
      err = eval_do_return(&stack, &expr, &env, result);
  } while (!err);

  gc(expr, env, stack);

  return err;
}

// -----------------------------------------------------------------------------
// REPL
// -----------------------------------------------------------------------------

static Atom generator_env;

// GNU readline function for tab completion.
// Looks for symbols in the current environment.
char* symbol_generator(const char* text, int state) {
  static Atom parent;
  static Atom bindings; // Note the statics.
  static int len;
  if (state == 0) {
    parent = car(generator_env);
    bindings = cdr(generator_env);
    len = strlen(text);
  }

  // Find `text` in the bindings.
  while (!nilp(bindings)) {
    Atom binding = car(bindings);
    Atom sym = car(binding);
    const char *name = sym.value.symbol;
    if (strncasecmp(name, text, len) == 0) {
      bindings = cdr(bindings);
      return strdup(name);
    }
    bindings = cdr(bindings);
  }
  return NULL;
}

static char history_file[] = ".lisp_history";

void repl(Atom env) {
  using_history();
  read_history(history_file);
  generator_env = env;
  rl_completion_entry_function = symbol_generator;
  char *input;
  while ((input = readline("λ> ")) != NULL) {

    // Continue if all ws (or comments).
    const char* start;
    const char* end;
    if (lex(input, &start, &end) == Result_OK && *start == '\0') continue;

    add_history(input);
    write_history(history_file);

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
// library
// -----------------------------------------------------------------------------
char* slurp(const char path[]) {
  FILE * file = fopen(path, "r");
  if (!file) return NULL;
  fseek(file, 0, SEEK_END);
  long len = ftell(file);
  fseek(file, 0, SEEK_SET);

  char* buf = malloc(len + 1);
  if (buf == NULL) return NULL;

  fread(buf, 1, len, file);
  buf[len] = '\0';
  fclose(file);

  return buf;
}

void load_file(Atom env, const char path[]) {
  printf("Loading '%s' ...\n", path);
  char *text = slurp(path);
  if (text) {
    const char *p = text;
    Atom expr;
    while (read_expr(p, &p, &expr) == Result_OK) {
      Atom result;
      Result r = eval_expr(expr, env, &result);
      if (r) {
        printf("Error in expression:\n\t");
        atom_print(expr);
        putchar('\n');
      } else {
        atom_print(result);
        putchar('\n');
      }
    }
    free(text);
  }
}

Atom initial_env() {
  Atom env = env_create(nil);
  env_set(env, make_sym("APPLY"), make_builtin(apply_builtin));

  env_set(env, make_sym("CAR"), make_builtin(car_builtin));
  env_set(env, make_sym("CDR"), make_builtin(cdr_builtin));
  env_set(env, make_sym("CONS"), make_builtin(cons_builtin));
  env_set(env, make_sym("PAIR?"), make_builtin(pairp_builtin));
  env_set(env, make_sym("EQ?"), make_builtin(eqp_builtin));

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

  return env;
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

  Atom env = initial_env();
  load_file(env, "library.lisp");
  repl(env);
}
