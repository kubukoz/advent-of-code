defmodule Aoc2022Test do
  use ExUnit.Case
  doctest Day1

  test "part 1" do
    assert Day1.part1() == 75622
  end

  test "part 2" do
    assert Day1.part2() == 213_159
  end
end
