test "compile" {
  // a+b+
  // a+ , b+
  let regex:Json = ["seq", ["plus", "a"], ["plus", "b"]]
  let r = parse!(regex)
  let p = compile_aux(r)
  p.push(Match)
  println("r: \{r}")
  println("p: \{p}")
  let i: MProgram = assemble(p)
  println("i: \{i}")
  inspect!(i, content = "[MChar('a'), MSplit(0, 2), MChar('b'), MSplit(2, 4), MMatch]")
  let i = #|0 : MChar('a')
    #|1 : MSplit(0, 2)
    #|2 : MChar('b')
    #|3 : MSplit(2, 4)
    #|4 : MMatch
    #|
  inspect!(i, content=
    #|0 : MChar('a')
    #|1 : MSplit(0, 2)
    #|2 : MChar('b')
    #|3 : MSplit(2, 4)
    #|4 : MMatch
    #|
  )
  let state: State ={..State::default(), data:"ab"}
  let result: Bool = interpret(state, assemble(p))
  inspect!(result,content=
    "true"
  )
}