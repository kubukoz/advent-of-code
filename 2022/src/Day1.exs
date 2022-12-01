input = String.trim(File.read!("input/day1.txt"))

# [[int]]
results =
  Enum.map(String.split(input, "\n\n"), fn elf ->
    Enum.map(String.split(elf), fn cal -> elem(Integer.parse(cal), 0) end)
  end)

elfCalories = Enum.map(results, fn elf -> Enum.sum(elf) end)

# day1
IO.puts(Enum.max(elfCalories))

# day2
IO.puts(
  Enum.sum(
    Enum.take(
      Enum.reverse(Enum.sort(elfCalories)),
      3
    )
  )
)
