#include <stdio.h>
#include <string.h>
#include <stdlib.h>

#include "runner.h"

int solvePart1(FILE *filePointer)
{
    int previous = 0;
    int increases = -1;

    char dataToBeRead[50];
    while (fgets(dataToBeRead, 50, filePointer))
    {
        int current = atoi(dataToBeRead);

        if (increases == -1)
        {
            increases = 0;
        }
        else if (current > previous)
        {
            increases++;
        }

        previous = current;
    }

    return increases;
}

int solvePart2(FILE *filePointer)
{
    int previous1 = -1;
    int previous2 = -1;
    int previous3 = -1;
    int increases = 0;

    char dataToBeRead[50];
    while (fgets(dataToBeRead, 50, filePointer))
    {
        int current = atoi(dataToBeRead);

        if (previous1 != -1)
        {
            int previousSum = previous1 + previous2 + previous3;
            int currentSum = previous2 + previous3 + current;

            if (previousSum < currentSum)
            {
                increases++;
            }
        }

        previous1 = previous2;
        previous2 = previous3;
        previous3 = current;
    }

    return increases;
}

int main(int argc, char *argv[])
{
    run_aoc_solution(argc, argv, solvePart1, solvePart2);
    return 0;
}
