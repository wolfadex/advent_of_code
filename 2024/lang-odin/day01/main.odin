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
		input_data, _ := strings.clone_from_bytes(bytes)
		defer delete(input_data)
		lines, err := strings.split_lines(input_data)
		defer delete(lines)

		answer1: int
		answer2: int

		column1: [dynamic]int = {}
		column2: [dynamic]int = {}
		defer delete(column1)
		defer delete(column2)

		reg, _ := regex.create("^(\\d+)[ ]+(\\d+)$")
		defer regex.destroy_regex(reg)

		for line, n in lines {
			cap, found := regex.match_and_allocate_capture(reg, line)
			defer regex.destroy_capture(cap)
			// log.debug("line", n, found, cap)

			if !found {
				break
			}

			col1, _ := strconv.parse_int(line[cap.pos[1][0]:cap.pos[1][1]], 10)
			col2, _ := strconv.parse_int(line[cap.pos[2][0]:cap.pos[2][1]], 10)
			append(&column1, col1)
			append(&column2, col2)
		}

		slice.stable_sort(column1[:])
		slice.stable_sort(column2[:])

		counts: map[int]int = {}
		defer delete(counts)

		for i := 0; i < len(column1); i += 1 {
			answer1 += abs(column1[i] - column2[i])

			count, found := counts[column1[i]]
			log.debug("count", count, found)

			if found {
				answer2 += count
			} else {
				c := slice.count(column2[:], column1[i])
				log.debug("new count", c)
				counts[column1[i]] = c
				answer2 += c
			}
		}

		fmt.printfln("Answer 1: %d", answer1)
		fmt.printfln("Answer 2: %d", answer2)
	}
}
