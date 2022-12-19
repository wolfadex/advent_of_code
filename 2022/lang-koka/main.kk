import std/os/env

pub fun main()
  with arg <- get-args().map
  arg.println()
