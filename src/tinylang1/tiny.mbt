enum Expr {
  Cst(Int)          // i
  Add(Expr, Expr)   // a + b
  Mul(Expr, Expr)   // a * b
  Var(String)
  Let(String, Expr, Expr)
} derive(Show)

type ExprEnv @immut/list.T[(String, Int)]
fn interpreter_expr(expr : Expr, env : ExprEnv) -> Int {
  match (expr, env) {
    (Cst(i), _) => i
    (Add(a, b), _) => interpreter_expr(a, env) + interpreter_expr(b, env)
    (Mul(a, b), _) => interpreter_expr(a, env) * interpreter_expr(b, env)
    (Var(x), ExprEnv(env)) => assoc(x, env).unwrap()
    (Let(x, e1, e2), ExprEnv(env)) => interpreter_expr(e2, Cons((x, interpreter_expr(e1, env)), env))
  }
}

fn assoc(x: String, env: ExprEnv) ->  Option[Int] {
  let mut i = 0;
  let n = env._.length();
  while i < n {
    let (k, v) = env._.nth(i).unwrap();
    if k == x {
      return Some(v);
    }
    i = i + 1;
  }
  return None;
}

type Cenv @immut/list.T[String] derive(Show)
fn comp(e : Expr, cenv : Cenv) -> ExprNameless {
  match e {
    Cst(i) => Cst(i)
    Add(a, b) => Add(comp(a, cenv), comp(b, cenv))
    Mul(a, b) => Mul(comp(a, cenv), comp(b, cenv))
    Var(x) => Var(index(cenv._, x).unwrap())
    Let(x, e1, e2) => Let(comp(e1, cenv), comp(e2, Cons(x, cenv._)))
  }
}

fn index(xs : Cenv, x : String) -> Option[Int] {
  let mut i = 0;
  let n = xs._.length();
  while i < n {
    if xs._.nth(i).unwrap() == x {
      return Some(i);
    }
    i = i + 1;
  }
  return None;
}

enum ExprNameless {
  Cst(Int)     
  Add(ExprNameless, ExprNameless)
  Mul(ExprNameless, ExprNameless)
  Var(Int)
  Let(ExprNameless, ExprNameless)
} derive(Show)

type Env @immut/list.T[Int]
fn comp2(e : ExprNameless, env : Env) -> Int {
  match e {
    Cst(i) => i
    Add(a, b) => comp2(a, env) + comp2(b, env)
    Mul(a, b) => comp2(a, env) * comp2(b, env)
    Var(n) => env._.nth(n).unwrap()
    Let(e1, e2) => comp2(e2, Cons(comp2(e1, env), env._))
  }
}

enum Instr {
  Cst(Int)     
  Add(Instr, Instr)
  Mul(Instr, Instr)
  Var(Int)
  Pop
  Swap
}


test {
  let e:Expr = Let("x", Cst(3), Add(Var("x"), Var("x")));
  let r1 = interpreter_expr(e, Nil);
  println(r1);

  let a1 = comp(e, Nil);
  println(a1);
  let a2 = comp2(a1, Nil);
  println(a2);

  let e2: Expr = Let("x", Cst(3), Let("y", Cst(4), Mul(Var("x"), Var("y"))));
  let r2 = interpreter_expr(e2, Nil);
  println(r2);
}