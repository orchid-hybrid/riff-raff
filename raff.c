#include <stdio.h>
#include <stdlib.h>
#include <assert.h>

enum scm_type {
  scm_nil,
  scm_int,
  scm_sym,
  scm_pair
};

struct scm {
  enum scm_type t;
  union {
    int i;
    struct scm_pair *p;
  } v;
};

struct scm_pair {
  int ref;
  struct scm car;
  struct scm cdr;
};

struct scm program_code;

#include "out.c"

void display(struct scm s) {
  switch(s.t) {
  case scm_nil:
    printf("()");
    break;
  case scm_int:
    printf("%d", s.v.i);
    break;
  case scm_sym:
    printf("%s", symbols[s.v.i]);
    break;
  case scm_pair:
    printf("(");
    do {
      display(s.v.p->car);
      if(s.v.p->cdr.t == scm_nil) {
        break;
      }
      else if(s.v.p->cdr.t == scm_pair) {
        printf(" ");
        s = s.v.p->cdr;
      }
      else {
        printf(" . ");
        display(s.v.p->cdr);
        break;
      }
    } while(1);
    printf(")");
    break;
  }
}

struct scm intern_symbol(char *s) {
  int i = 0;
  
  while(symbols[i]) {
    if(!strcmp(symbols[i],s)) {
      return (struct scm){ .t = scm_sym, .v.i = i };
    }
    i++;
  }

  return (struct scm){ .t = scm_nil };
}

struct scm assoc(struct scm s, struct scm t) {
  // s must be a symbol
  
  struct scm e;
  
  while(t.t == scm_pair) {
    e = t.v.p->car;
    if(e.t == scm_pair && e.v.p->car.t == scm_sym && e.v.p->car.v.i == s.v.i) {
      return e;
    }
    
    t = t.v.p->cdr;
  }
  
  return (struct scm){ .t = scm_nil };
}

#define LARGENUMBER 512
#define BIGNUMBER 24
int environment_stack_top;
struct scm environment_vars[LARGENUMBER][BIGNUMBER];
struct scm environment_vals[LARGENUMBER][BIGNUMBER];
int        environment_size[LARGENUMBER];

struct scm apply(struct scm f, struct scm args);
struct scm eval(struct scm* vars, struct scm* vals, int size, struct scm e);

int is_builtin(struct scm f) {
  assert(f.t == scm_sym);
  return
    (!strcmp("+",symbols[f.v.i])) ||
    (!strcmp("print",symbols[f.v.i]));
}

struct scm apply_builtin(struct scm f, struct scm args) {
  assert(f.t == scm_sym);
  if(!strcmp("+", symbols[f.v.i])) {
    assert(args.t == scm_pair);
    assert(args.v.p->cdr.t == scm_pair);
    struct scm x = args.v.p->car;
    struct scm y = args.v.p->cdr.v.p->car;
    assert(x.t == scm_int);
    assert(y.t == scm_int);
    return (struct scm){ .t = scm_int, .v.i = x.v.i + y.v.i };
  }
  else if(!strcmp("print", symbols[f.v.i])) {
    assert(args.t == scm_pair);
    struct scm x = args.v.p->car;
    display(x);
    puts("!!!!!!!!!!!!!!!!!");
    return (struct scm){ .t = scm_nil };
  }
  assert(0);
}

struct scm apply(struct scm f, struct scm args) {
  int i;
  struct scm e;
  struct scm argl, body;
  struct scm *myvars, *myvals;
  
  assert(f.t == scm_sym);
  puts(symbols[f.v.i]);

  // check if f is builtin first

  if(is_builtin(f)) {
    return apply_builtin(f, args);
  }
  
  e = assoc(f, program_code);
  assert(e.t != scm_nil);
  assert(e.t == scm_pair);

  assert(e.v.p->cdr.t == scm_pair);
  argl = e.v.p->cdr.v.p->car;
  
  assert(e.v.p->cdr.v.p->cdr.t == scm_pair);
  body = e.v.p->cdr.v.p->cdr.v.p->car;

  puts("APPLY");
  display(argl);
  puts("");
  display(body);
  puts("");
  puts("");
  
  environment_size[environment_stack_top] = 0;
  myvars = environment_vars[environment_stack_top];
  myvals = environment_vals[environment_stack_top];
  while(argl.t == scm_pair && args.t == scm_pair) {
    myvars[environment_size[environment_stack_top]] = argl.v.p->car;
    myvals[environment_size[environment_stack_top]] = args.v.p->car;
    argl = argl.v.p->cdr;
    args = args.v.p->cdr;
    environment_size[environment_stack_top]++;
  }
  environment_stack_top++;

  return eval(myvars, myvals, environment_size[environment_stack_top-1], body);
}

struct scm eval(struct scm* vars, struct scm* vals, int size, struct scm e) {
  int i;
  
  puts("EVAL");
  display(e);
  puts("");

  switch(e.t) {
  case scm_nil:
    return e;
    break;
    
  case scm_int:
    return e;
    break;
    
  case scm_sym:
    // variable lookup in environment
    puts("LOOKUP");
    for(i = 0; i < size; i++) {
      display(vars[i]);
      if(vars[i].t == scm_sym && vars[i].v.i == e.v.i) {
        return vals[i];
      }
    }
    assert(0);
    
    break;
    
  case scm_pair:
    // function application (probably)
    // eval all the arguments and then apply
    {
    struct scm f = e.v.p->car;
    assert(f.t == scm_sym);
    struct scm l = (struct scm){ .t = scm_nil };
    e = e.v.p->cdr;
    while(e.t == scm_pair) {
      struct scm h  = (struct scm){ .t = scm_pair, .v.p = malloc(sizeof(struct scm_pair)) };
      h.v.p->ref = 1;
      h.v.p->car = eval(vars, vals, size, e.v.p->car);
      h.v.p->cdr = l;
      l = h;
      
      e = e.v.p->cdr;
    }
    
    return apply(f, l);
    }
    break;
  }
}

int main(void) {
  environment_stack_top = 0;
  
  program_code = build_sexp();
  display(program_code);
  puts("");
  
  //display(assoc(intern_symbol("foo"), program_code));
  //puts("");
  
  apply(intern_symbol("main"),(struct scm){ .t = scm_nil });
  
  return EXIT_SUCCESS;
}
