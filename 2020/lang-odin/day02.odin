package day01

import "core:fmt"
import "core:log"
import "core:mem"
import "core:os"
import "core:strconv"
import "core:strings"
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
		input_data, _ := strings.clone_from_bytes(bytes)
		delete(bytes)
		defer delete(input_data)
		lines, err := strings.split_lines(input_data)
		defer delete(lines)

		answer1: int = 0
		answer2: int = 0

		for line in lines {
			parts := strings.split(line, " ")
			defer delete(parts)

			if len(parts) != 3 {
				break
			}

			lowHigh := strings.split(parts[0], "-")
			defer delete(lowHigh)

			low, _ := strconv.parse_int(lowHigh[0], 10)
			high, _ := strconv.parse_int(lowHigh[1], 10)
			to_match, _ := strings.remove(parts[1], ":", 1)
			defer delete(to_match)

			match_count := strings.count(parts[2], to_match)

			if match_count >= low && match_count <= high {
				answer1 += 1
			}

			if xor(
				utf8.rune_string_at_pos(parts[2], low - 1) == to_match,
				utf8.rune_string_at_pos(parts[2], high - 1) == to_match,
			) {
				answer2 += 1
			}
		}

		fmt.printfln("Answer 1: %d", answer1)
		fmt.printfln("Answer 2: %d", answer2)
	}
}


xor :: proc(a, b: bool) -> bool {
	return (a && !b) || (!a && b)
}
