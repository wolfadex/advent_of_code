package day01

import "core:fmt"
import "core:log"
import "core:mem"
import "core:os"
import "core:slice"
import "core:strconv"
import "core:strings"
import "core:text/regex"


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

		column1: [dynamic]int = {}
		column2: [dynamic]int = {}
		defer delete(column1)
		defer delete(column2)

		reg, _ := regex.create("^(\\d+)[ ]+(\\d+)$")
		defer regex.destroy_regex(reg)

		for line in strings.split_lines_iterator(&input_data) {
			cap, found := regex.match_and_allocate_capture(reg, line)
			defer regex.destroy_capture(cap)

			if !found {
				break
			}

			col1 := strconv.atoi(line[cap.pos[1][0]:cap.pos[1][1]])
			col2 := strconv.atoi(line[cap.pos[2][0]:cap.pos[2][1]])
			append(&column1, col1)
			append(&column2, col2)
		}

		slice.stable_sort(column1[:])
		slice.stable_sort(column2[:])

		counts: map[int]int = {}
		defer delete(counts)

		for col1, i in column1 {
			answer1 += abs(col1 - column2[i])

			count, found := counts[col1]

			if found {
				answer2 += count
			} else {
				c := slice.count(column2[:], col1)
				counts[col1] = col1 * c
				answer2 += col1 * c
			}
		}

		fmt.printfln("Answer 1: %d", answer1)
		fmt.printfln("Answer 2: %d", answer2)
	}
}
