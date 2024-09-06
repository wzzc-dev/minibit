fn main {
  println(@lib.hello())
  
  let arr = Array::new(capacity = 10)
  
  arr[0] = 1
  
  println(arr)
}
