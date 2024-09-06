
enum Regex {
  Seq(Regex, Regex)
  Char(Char)
  Alt(Regex, Regex)
  Star(Regex)
  Plus(Regex)
  Opt(Regex)
  Empty
} derive(Show)

let label_counter : Ref[Int] = { val :0 }

fn gen_label() -> Int {
  label_counter.val += 1
  label_counter.val
}
fn parse(cst : Json) -> Regex! {
  match cst {
    String(s) => {
      // TODO: reduce the Empty usage  
      let mut acc = Empty
      for i in s {
        acc = Seq(acc, Char(i))
      }
      acc
    }
    ["or", a, b] => Alt(parse!(a), parse!(b))
    ["seq", a, b] => Seq(parse!(a), parse!(b))
    ["star", a] => Star(parse!(a))
    ["plus", a] => Plus(parse!(a))
    ["opt", a] => Opt(parse!(a))
    _ => fail!("invalid cst \{cst}")
  }
}

enum Instr {
  Char(Char)
  Match
  Split(Int, Int)
  Jmp(Int)
  Label(Int)
} derive(Show)

fn compile_aux(regex : Regex) -> Array[Instr] {
  match regex {
    Opt(r) => {
      let l1 = gen_label()
      let l2 = gen_label()
      [
        Label(l1),
        ..compile_aux(r),
        Label(l2),
      ]
    }
    Empty => []
    Seq(r1, r2) => [..compile_aux(r1), ..compile_aux(r2)]
    Alt(r1, r2) => {
      let l1 = gen_label()
      let l2 = gen_label()
      let l3 = gen_label()
      [
        Split(l1, l2),
        Label(l1),
        ..compile_aux(r1),
        Jmp(l3),
        Label(l2),
        ..compile_aux(r2),
        Label(l3),
      ]
    }
    Char(c) => [Char(c)]
    Star(r) => {
      let l1 = gen_label()
      let l2 = gen_label()
      let l3 = gen_label()
      [
        Label(l1),
        Split(l1, l2),
        Label(l2),
        ..compile_aux(r),
        Jmp(l1),
        Label(l3),
      ]
    }
    Plus(r) => {
      let l1 = gen_label()
      let l2 = gen_label()
      [
        Label(l1),
        ..compile_aux(r),
        Split(l1, l2),
        Label(l2),
      ]
    }
  }
}


type Program Array[Instr]
fn collect_labels(p : Program) -> Array[Option[Int]] {
  let labels = Array::make(p._.length(), Some(0))
  let mut i = 0
  for instr in p._ {
    match instr {
      Label(l) => labels[l] = Some(i)
      _ => i += 1
    }
  }
  labels
}

enum MInstr {
  MChar(Char)
  MMatch
  MSplit(Int, Int)
  MJmp(Int)
} derive(Show)

typealias MProgram = Array[MInstr]

fn assemble(p : Program) -> MProgram {
  let labels = collect_labels(p)
  let instrs =  []
  for instr in p._ {
    match instr {
      Char(c) => instrs.push(MChar(c))
      Match => instrs.push(MMatch)
      Jmp(l) => instrs.push(MJmp(labels[l].unwrap()))
      Split(l1, l2) =>
        instrs.push(MSplit(labels[l1].unwrap(), labels[l2].unwrap()))
      Label(_) => ()
    }
  }
  instrs
}

struct State {
  pc : Int
  data: String
  offset: Int
} derive(Show, Default)
fn interpret(state : State, p : MProgram) -> Bool {
  let stack = []
  stack.push(state)
  while not(stack.is_empty()) {
    let state = stack.pop()
    match p[state.unwrap().pc] {
      MChar(c) => {
        if state.unwrap().offset < state.unwrap().data.length() &&  state.unwrap().data[state.unwrap().offset] == c {
          let new_state: State = {..State::default(), data:state.unwrap().data, offset:state.unwrap().offset + 1, pc:state.unwrap().pc + 1}
          stack.push(new_state);
        }
      }
      MJmp(l) => {
        let new_state: State = {..State::default(), data:state.unwrap().data, offset:state.unwrap().offset, pc:l}
        stack.push(new_state)
      }
      MSplit(l1, l2) => {
        let new_state1: State = {..State::default(), data:state.unwrap().data, offset:state.unwrap().offset, pc:l1}
        let new_state2: State = {..State::default(), data:state.unwrap().data, offset:state.unwrap().offset, pc:l2}
        stack.push(new_state1)
        stack.push(new_state2)
      }
      MMatch => {
        if state.unwrap().offset == state.unwrap().data.length() {
          return true
        }
        return false
      }
    }
  }
  false
}
test "compile" {
  // a+b+
  // a+ , b+
  let regex:Json = ["seq", ["plus", "a"], ["plus", "b"]]
  let r = parse!(regex)
  let p = compile_aux(r)
  p.push(Match)
  println("r: \{r}")
  println("p: \{p}")
  // inspect!(p, content="[Label(1), Char('a'), Split(1, 2), Label(2), Match, Label(3), Char('b'), Split(3, 4), Label(4), Match]")
  // let regex:Json = ["or", "a", ["seq", "b", ["star", "c"]]]
  // let r = parse!(regex)
  // let p = compile_aux(r)
  // println("r: \{r}")
  // println("p: \{p}")
  // inspect!(p, content="[Split(1, 2), Label(1), Char('a'), Jmp(3), Label(2), Char('b'), Label(4), Char('c'), Split(4, 5), Label(5), Label(3)]")
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