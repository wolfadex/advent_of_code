#include <stdio.h>
#include <string.h>
#include <stdlib.h>

#include "runner.h"

int run_aoc_solution(int argc, char *argv[], solver *part1, solver *part2)
{
    if (argc < 2)
    {
        printf("Too few arguments. Expected to be run as: aoc_dayN <path to input file>\n");
        exit(EXIT_FAILURE);
    }

    if (argc > 2)
    {
        printf("Too few arguments. Expected to be run as: aoc_dayN <path to input file>\n");
        exit(EXIT_FAILURE);
    }

    FILE *filePointer;
    filePointer = fopen(argv[1], "r");

    if (filePointer == NULL)
    {
        printf("Failed to open %s\n", argv[1]);
        exit(EXIT_FAILURE);
    }

    int answer1 = part1(filePointer);
    rewind(filePointer);
    int answer2 = part2(filePointer);

    fclose(filePointer);

    // TODO
    printf("Part 1: %d\n", answer1);
    printf("Part 2: %d\n", answer2);

    return 0;
}