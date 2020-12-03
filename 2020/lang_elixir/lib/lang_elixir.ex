!defmodule LangElixir do
  require Day01

  def solve(inputPath, day, part) do
    contents = File.read!(inputPath)

    case {day, part} do
      {"01", 1} ->
        Day01.part1(contents)

      _ ->
        throw(:dayPartNotFound)
    end
  end
end
