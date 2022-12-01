defmodule Day1 do
  defp elfCalories() do
    input =
      File.read!("input/day1.txt")
      |> String.trim()

    results =
      input
      |> String.split("\n\n")
      |> Enum.map(fn elf ->
        String.split(elf)
        |> Enum.map(fn cal ->
          cal
          |> Integer.parse()
          |> elem(0)
        end)
      end)

    results |> Enum.map(&Enum.sum/1)
  end

  def part1() do
    elfCalories()
    |> Enum.max()
  end

  def part2() do
    elfCalories()
    |> Enum.sort()
    |> Enum.reverse()
    |> Enum.take(3)
    |> Enum.sum()
  end
end
