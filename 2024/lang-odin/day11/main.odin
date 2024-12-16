package day11

import "core:fmt"
import "core:log"
import "core:mem"
import "core:os"
import "core:slice"
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
		defer delete(bytes)
		input_data := string(bytes)

		answer1: int
		answer2: int

		stones: [dynamic]string
		defer delete(stones)
		stns := strings.split(input_data, " ")
		defer delete(stns)
		append(&stones, ..stns)

		log.debug("Initial stones", stones)

		for blinks := 0; blinks < MAX_BLINKS; blinks += 1 {
			log.debug("Blinks", blinks, stones)
			for idx := 0; idx < len(stones); idx += 1 {
				stone := stones[idx]
				log.debug("Next stone", stone)

				if stone == "0" {
					stones[idx] = "1"
					log.debug("0 -> 1", stones)
				} else {
					size := len(stone)

					if size % 2 == 0 {
						left_half := stone[:size / 2]
						left_val := strconv.atoi(left_half)
						right_half := stone[size / 2:]
						right_val := strconv.atoi(right_half)

						buf_left: [20]byte
						left := strconv.itoa(buf_left[:], left_val)
						defer delete(left)
						stones[idx] = strings.clone(left)

						buf_right: [20]byte
						right := strconv.itoa(buf_right[:], right_val)
						defer delete(right)
						idx += 1
						inject_at(&stones, idx, strings.clone(right))
						log.debug("Split right after", stones)
					} else {
						stone_val := strconv.atoi(stone)
						stone_val *= 2024
						buf: [20]byte
						multi := strconv.itoa(buf[:], stone_val)
						defer delete(multi)
						stones[idx] = strings.clone(multi)
						log.debug("Multiplied", stones)
					}
				}
			}

			log.debug("Blinked", stones)
		}

		for stone in stones {
			delete(stone)
		}

		answer1 = len(stones)

		fmt.printfln("Answer 1: %d", answer1)
		fmt.printfln("Answer 2: %d", answer2)
	}
}

MAX_BLINKS :: 2
