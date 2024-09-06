
test "name" {
  let instrs2 = @immut/list.of([Cst(2), Cst(2), Cst(2), Add, Mul]);
  // 打印 instrs
  println(loop_eval(instrs2, Nil));
}