# Advent of Code, 2020 - Elm

To run the code you'll need [Deno](https://deno.land/). Once installed you can run the desired solution with:

```
deno run --allow-read --allow-write --allow-env --allow-run ./src/driver.ts ../input/day<NN>.txt ./src/Day<NN>.elm DayNN #
```
where `NN` is the day you're wokring on, e.g. `day01.txt` or `Day01.elm` and `#` is the part of the day, e.g. `1` or `2`.

The Deno flags are for reading, writing, and compiling the Elm code.

[Day 1](./src/Day01.elm)