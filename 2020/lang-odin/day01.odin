package day01

import "core:fmt"
import "core:log"
import "core:mem"
import "core:os"
import "core:strconv"
import "core:strings"

main :: proc() {
	when ODIN_DEBUG {
		logger := log.create_console_logger()
		context.logger = logger
	}

	if len(os.args) == 1 {
		os.exit(1)
	}

	filePath := os.args[1]

	if data, success := os.read_entire_file_from_filename(filePath); success {
		input_data, _ := strings.clone_from_bytes(data)
		defer delete(input_data)
		lines, err := strings.split_lines(input_data)
		defer delete(lines)

		answer1: int = -1
		answer2: int = -1

		processedInts: [dynamic]int = {}

		lineProcessor: for line in lines {
			i, _ := strconv.parse_int(line, 10)

			if ans1, found1 := find2Nums(i, processedInts[:]); found1 {
				answer1 = ans1
			}

			if ans2, found2 := find3Nums(i, processedInts[:]); found2 {
				answer2 = ans2
			}

			if answer1 > -1 && answer2 > -1 {
				break lineProcessor
			}

			append(&processedInts, i)
		}

		fmt.printfln("Answer 1: %d", answer1)
		fmt.printfln("Answer 2: %d", answer2)
	}
}


find2Nums :: proc(num: int, availableNums: []int) -> (int, bool) {
	lookingFor := 2020 - num

	for availNum in availableNums {
		if lookingFor == availNum {
			return (num * lookingFor), true
		}
	}

	return -1, false
}


find3Nums :: proc(num1: int, availableNums: []int) -> (int, bool) {
	for num2, i in availableNums {
		lookingFor := 2020 - num1 - num2

		for num3 in availableNums[i:] {
			if lookingFor == num3 {
				return (num1 * num2 * num3), true
			}
		}
	}

	return -1, false
}
