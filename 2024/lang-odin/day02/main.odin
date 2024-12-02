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

			if is_safe(numStrs[:], false) {
				answer1 += 1
			}

			if is_safe(numStrs[:], true) {
				answer2 += 1
			}
		}

		fmt.printfln("Answer 1: %d", answer1)
		fmt.printfln("Answer 2: %d", answer2)
	}
}

is_safe :: proc(numStrs: []string, allow_faults: bool) -> bool {
	safe_check := proc(numStrs: []string, skip_index: bool = false, index: int = 0) -> bool {
		prev: int = ---
		increasing: bool

		for numStr, i in numStrs {
			num := strconv.atoi(numStr)

			if skip_index && i == index {
				continue
			}

			if i == 0 || i == 1 && skip_index && index == 0 {
				prev = num
				continue
			}

			if skip_index {
				if index < 2 {
					if i == 2 {
						increasing = prev < num
					}
				} else if i == 1 {
					increasing = prev < num
				}
			} else if i == 1 {
				increasing = prev < num
			}

			diff := prev - num

			if skip_index {
				if index < 2 {
					if i > 2 {
						if increasing && diff > 0 || !increasing && diff < 0 {
							return false
						}
					}
				} else if i > 1 {
					if increasing && diff > 0 || !increasing && diff < 0 {
						return false
					}
				}
			} else {
				if i > 1 {
					if increasing && diff > 0 || !increasing && diff < 0 {
						return false
					}
				}
			}

			if abs(diff) < 1 || abs(diff) > 3 || diff == 0 {
				return false
			}

			prev = num
		}

		return true
	}


	if allow_faults {
		if safe_check(numStrs) {
			return true
		}

		for _, i in numStrs {
			if safe_check(numStrs, true, i) {
				return true
			}
		}

		return false
	}

	return safe_check(numStrs)
}
