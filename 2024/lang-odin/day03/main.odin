package day03

import "core:fmt"
import "core:log"
import "core:mem"
import "core:os"
import "core:slice"
import "core:strconv"
import "core:strings"
import "core:text/regex"
import regex_common "core:text/regex/common"
import "core:unicode"
import "core:unicode/utf8"


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


		answer1 = gather_muls(input_data[:])
		answer2 = gather_muls_with_pause(input_data[:])

		fmt.printfln("Answer 1: %d", answer1)
		fmt.printfln("Answer 2: %d", answer2)
	}
}

gather_muls :: proc(data: string) -> (total: int) {
	reg, _ := regex.create("mul\\((\\d+),(\\d+)\\)", {regex_common.Flag.Global})
	defer regex.destroy_regex(reg)
	offset := 0
	paused: bool

	for {
		cap, found := regex.match_and_allocate_capture(reg, data[offset:])
		defer regex.destroy_capture(cap)

		if found {
			// log.debug("CAP", cap)
			left := strconv.atoi(cap.groups[1])
			right := strconv.atoi(cap.groups[2])
			total += left * right
			// log.debug("MATH", total)
			offset += cap.pos[0][1]
		} else {
			break
		}
	}

	return total
}

gather_muls_with_pause :: proc(data: string) -> (total: int) {
	reg, _ := regex.create(
		"(mul\\((\\d+),(\\d+)\\)|do\\(\\)|don't\\(\\))",
		{regex_common.Flag.Global},
	)
	defer regex.destroy_regex(reg)
	offset := 0
	paused: bool

	for {
		cap, found := regex.match_and_allocate_capture(reg, data[offset:])
		defer regex.destroy_capture(cap)

		if found {
			// log.debug("CAP", cap)
			switch cap.groups[0] {
			case "do()":
				// log.debug("UNPAUSE")
				paused = false
			case "don't()":
				// log.debug("PAUSE")
				paused = true
			case:
				if strings.starts_with(cap.groups[0], "mul(") && !paused {
					left := strconv.atoi(cap.groups[2])
					right := strconv.atoi(cap.groups[3])
					total += left * right
					// log.debug("MATH", total)
				}
			}
			offset += cap.pos[0][1]
		} else {
			break
		}
	}

	return total
}
