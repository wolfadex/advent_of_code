#ifndef _AOC_RUNNER_
#define _AOC_RUNNER_

typedef int solver(FILE *filePointer);

int run_aoc_solution(int argc, char *argv[], solver *part1, solver *part2);

#endif