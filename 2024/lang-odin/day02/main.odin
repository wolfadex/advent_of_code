package day02

import "core:fmt"
import "core:log"
import "core:mem"
import "core:os"
import "core:slice"
import "core:strconv"
import "core:strings"


main :: proc() {
	when ODIN_DEBUG {
		// setup debug logging
		logger := log.create_console_logger()
		context.logger = logger

		// setup tracking allocator for making sure all memory is cleaned up
		default_allocator := context.allocator
		tracking_allocator: mem.Tracking_Allocator
		mem.tracking_allocator_init(&tracking_allocator, default_allocator)
		context.allocator = mem.tracking_allocator(&tracking_allocator)

		reset_tracking_allocator :: proc(a: ^mem.Tracking_Allocator) -> bool {
			err := false

			for _, value in a.allocation_map {
				fmt.printfln("%v: Leaked %v bytes", value.location, value.size)
				err = true
			}

			mem.tracking_allocator_clear(a)

			return err
		}

		defer reset_tracking_allocator(&tracking_allocator)
	}

	if len(os.args) == 1 {
		os.exit(1)
	}

	filePath := os.args[1]

	if bytes, success := os.read_entire_file_from_filename(filePath); success {
		defer delete(bytes)
		input_data := string(bytes)

		answer1: int
		answer2: int

		reports_iter: for line in strings.split_lines_iterator(&input_data) {
			numStrs := strings.split(line, " ")
			defer delete(numStrs)

			prev: int = ---
			increasing: bool

			levels_iter: for numStr, i in numStrs {
				num := strconv.atoi(numStr)

				if i == 0 {
					prev = num
					continue levels_iter
				}

				diff := prev - num

				if i == 1 {
					increasing = prev < num
				}

				if i > 1 {
					if increasing && diff > 0 || !increasing && diff < 0 {
						continue reports_iter
					}
				}

				if abs(diff) < 1 || abs(diff) > 3 || diff == 0 {
					continue reports_iter
				}

				prev = num
			}

			answer1 += 1
		}

		fmt.printfln("Answer 1: %d", answer1)
		fmt.printfln("Answer 2: %d", answer2)
	}
}
