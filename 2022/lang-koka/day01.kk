import std/os/env
import std/os/file
import std/text/parse
import std/os/path


named effect file
  fun read-line() : string   // `:(file)   -> <exn> a`


fun file(fname,action) 
  var content := read-text-file(fname.path).lines
  with f <- named handler 
    fun read-line() 
      match content  
        Nil -> "" 
        Cons(x,xx) -> { content := xx; x }
  action(f)


pub fun main()
  match get-args()
    Nil -> println("please provide a file path")
    Cons(p, _) -> {
      with part1Txt <- file(p)
      val l = part1Txt.read-line()
      val n = parse(l, pint)
      println(n.show)
    }