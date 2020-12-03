defmodule LangElixirTest do
  use ExUnit.Case
  doctest LangElixir

  test "greets the world" do
    assert LangElixir.hello() == :world
  end
end
