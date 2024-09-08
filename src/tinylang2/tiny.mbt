enum Expr {
  Cst(Int) // i
  Add(Expr, Expr) // a + b
  Mul(Expr, Expr) // a * b
  Sub(Expr, Expr)
  Div(Expr, Expr)
  Var(String)
  Let(String, Expr, Expr)
} derive(Show)

type ExprEnv @immut/list.T[(String, Int)]

fn interpreter_expr(expr : Expr, env : ExprEnv) -> Int {
  match (expr, env) {
    (Cst(i), _) => i
    (Add(a, b), _) => interpreter_expr(a, env) + interpreter_expr(b, env)
    (Mul(a, b), _) => interpreter_expr(a, env) * interpreter_expr(b, env)
    (Sub(a, b), _) => interpreter_expr(a, env) - interpreter_expr(b, env)
    (Div(a, b), _) => interpreter_expr(a, env) / interpreter_expr(b, env)
    (Var(x), ExprEnv(env)) => assoc(x, env).unwrap()
    (Let(x, e1, e2), ExprEnv(env)) =>
      interpreter_expr(e2, Cons((x, interpreter_expr(e1, env)), env))
  }
}

fn assoc(x : String, env : ExprEnv) -> Int? {
  let mut i = 0
  let n = env._.length()
  while i < n {
    let (k, v) = env._.nth(i).unwrap()
    if k == x {
      return Some(v)
    }
    i = i + 1
  }
  return None
}

type Cenv @immut/list.T[String] derive(Show)

fn comp(e : Expr, cenv : Cenv) -> ExprNameless {
  match e {
    Cst(i) => Cst(i)
    Add(a, b) => Add(comp(a, cenv), comp(b, cenv))
    Mul(a, b) => Mul(comp(a, cenv), comp(b, cenv))
    Sub(a, b) => Sub(comp(a, cenv), comp(b, cenv))
    Div(a, b) => Div(comp(a, cenv), comp(b, cenv))
    Var(x) => Var(index(cenv._, x).unwrap())
    Let(x, e1, e2) => Let(comp(e1, cenv), comp(e2, Cons(x, cenv._)))
  }
}

fn index(xs : Cenv, x : String) -> Int? {
  let mut i = 0
  let n = xs._.length()
  while i < n {
    if xs._.nth(i).unwrap() == x {
      return Some(i)
    }
    i = i + 1
  }
  return None
}

enum ExprNameless {
  Cst(Int)
  Add(ExprNameless, ExprNameless)
  Mul(ExprNameless, ExprNameless)
  Sub(ExprNameless, ExprNameless)
  Div(ExprNameless, ExprNameless)
  Var(Int)
  Let(ExprNameless, ExprNameless)
} derive(Show)

type Env @immut/list.T[Int] derive(Show)

fn eval2(e : ExprNameless, env : Env) -> Int {
  match e {
    Cst(i) => i
    Add(a, b) => eval2(a, env) + eval2(b, env)
    Mul(a, b) => eval2(a, env) * eval2(b, env)
    Sub(a, b) => eval2(a, env) - eval2(b, env)
    Div(a, b) => eval2(a, env) / eval2(b, env)
    Var(n) => env._.nth(n).unwrap()
    Let(e1, e2) => eval2(e2, Cons(eval2(e1, env), env._))
  }
}

enum Instr {
  Cst(Int)
  Add
  Mul
  Sub
  Div
  Var(Int)
  Pop
  Swap
} derive(Show)

fn comp3(e : ExprNameless) -> Array[Instr] {
  match e {
    Cst(i) => [Instr::Cst(i)]
    Add(a, b) => [..comp3(a), ..comp3(b), Add]
    Mul(a, b) => [..comp3(a), ..comp3(b), Mul]
    Sub(a, b) => [..comp3(a), ..comp3(b), Sub]
    Div(a, b) => [..comp3(a), ..comp3(b), Div]
    Let(e1, e2) => [..comp3(e1), ..comp3(e2), Swap, Pop]
    Var(n) => [Var(n)]
  }
}

typealias Instrs = @immut/list.T[Instr]

typealias Operand = Int

typealias Stack = @immut/list.T[Operand]

fn run(instrs : Instrs) -> Int {
  let stack : Stack = Nil
  loop instrs, stack {
    Cons(Add, Nil), Cons(a, Cons(b, Nil)) => a + b
    Cons(Sub, Nil), Cons(a, Cons(b, Nil)) => a - b
    Cons(Mul, Nil), Cons(a, Cons(b, Nil)) => a * b
    Cons(Div, Nil), Cons(a, Cons(b, Nil)) => a / b
    Nil, stack => stack.nth(0).unwrap()
    Cons(Cst(i), rest), stack => continue rest, Cons(i, stack)
    Cons(Add, rest), Cons(a, Cons(b, stack)) => continue rest, Cons(a + b, stack)
    Cons(Sub, rest), Cons(a, Cons(b, stack)) =>
      continue rest, Cons(a - b, stack)
    Cons(Mul, rest), Cons(a, Cons(b, stack)) =>
      continue rest, Cons(a * b, stack)
    Cons(Div, rest), Cons(a, Cons(b, stack)) =>
      continue rest, Cons(a / b, stack)
    Cons(Var(n), rest), stack =>
      continue rest, Cons(stack.nth(n).unwrap(), stack)
    Cons(Pop, rest), Cons(_, stack) => continue rest, stack
    Cons(Swap, rest), Cons(a, Cons(b, stack)) =>
      continue rest, Cons(b, Cons(a, stack))
    
    _, _ => abort("Matched none")
  }
}

test {
  let e : Expr = Let("x", Cst(3), Add(Var("x"), Var("x")))
  let r1 = interpreter_expr(e, Nil)
  println(r1)
  let a1 = comp(e, Nil)
  println(a1)
  let a2 = eval2(a1, Nil)
  println(a2)
  let e2 : Expr = Let("x", Cst(17), Add(Var("x"), Var("x")))
  println(e2)
  let e3 = comp(e2, Nil)
  println(e3)
  let r2 = interpreter_expr(e2, Nil)
  println(r2)
  let a3 = comp3(e3)
  println(a3)

  // x = 17, y=13 , z=2, (x+y)/z
  let e4 : Expr = Let(
    "z",
    Cst(2),
    Mul(Let("y", Cst(13), Let("x", Cst(17), Add(Var("x"), Var("y")))), Var("z")),
  )
  let r4 = comp(e4, Nil)
  let r42 = comp3(r4)
  println(r42)

  // let x = 2; 1 + ( x + 7 )
  let e5 : Expr = Add(Cst(1), Let("x", Cst(2), Add(Var("x"), Cst(7))))
  let r5 = comp(e5, Nil)
  println(r5)
  let r51 = comp3(r5)
  println(r51)

  let r6: Instrs = Instrs::from_array(r51)
  println(run(r6))
}
