defmodule Day01 do
  def part1(input) do
    nums =
      input
      |> String.split("\n")
      |> Enum.map(&String.trim/1)
      |> Enum.map(&String.to_integer/1)

    {a, b} = findNums2(nums, 2020)
    a * b
  end

  def findNums2(nums, goal) do
    if Enum.empty?(nums) do
      throw(:numsEmpty)
    end

    head = List.first(nums)
    rest = List.last(nums)

    if Enum.member?(rest, goal - head) do
      {head, goal - head}
    else
      findNums2(rest, goal)
    end
  end
end
