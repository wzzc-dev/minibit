enum Instr {
  Cst(Int)
  Add
  Sub
  Mul
  Div
} // non-recursive

typealias Instrs = @immut/list.T[Instr]
typealias Operand = Int
typealias Stack = @immut/list.T[Operand]


fn loop_eval(instrs : Instrs, stk : Stack) -> Int {
  loop instrs, stk {
    Cons(Cst(i), rest), stk => continue rest, Cons(i, stk)
    Cons(Add, rest), Cons(a, Cons(b, stk)) => continue rest, Cons(a + b, stk)
    Cons(Sub, rest), Cons(a, Cons(b, stk)) => continue rest, Cons(a - b, stk)
    Cons(Mul, rest), Cons(a, Cons(b, stk)) => continue rest, Cons(a * b, stk)
    Cons(Div, rest), Cons(a, Cons(b, stk)) => continue rest, Cons(a / b, stk)
    Nil, Cons(a, _) => a
    _, _ => abort("Matched none")
  }
}

test {
  let instrs2 = @immut/list.of([Cst(2), Cst(2), Cst(2), Add, Mul]);
  // 打印 instrs
  println(loop_eval(instrs2, Nil));
  assert_eq!(loop_eval(instrs2, Nil), 8);
  assert_eq!(loop_eval(@immut/list.of([Cst(2), Cst(2), Cst(2), Add, Mul]), Nil), 8);
  assert_eq!(loop_eval(@immut/list.of([Cst(2), Cst(2), Cst(4), Sub, Mul]), Nil), 4);
  assert_eq!(loop_eval(@immut/list.of([Cst(2), Cst(2), Cst(8), Div, Mul]), Nil), 8);
}