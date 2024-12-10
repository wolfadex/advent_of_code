package day07

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

		for equation in strings.split_lines_iterator(&input_data) {
			equation_parts := strings.split(equation, ": ")
			defer delete(equation_parts)

			goal_total := strconv.atoi(equation_parts[0])
			subtotals_1: [dynamic]int
			defer delete(subtotals_1)
			subtotals_2: [dynamic]int
			defer delete(subtotals_2)
			j: int
			answered_1: bool
			answered_2: bool

			iter_loop: for piece_str in strings.split_iterator(&equation_parts[1], " ") {
				if j == 0 {
					subtotals_1 = {strconv.atoi(piece_str)}
					subtotals_2 = {strconv.atoi(piece_str)}
				} else {
					next := strconv.atoi(piece_str)

					temp_subtotals_1 := subtotals_1[:]
					defer delete(temp_subtotals_1)
					subtotals_1 = {}

					for subtotal in temp_subtotals_1 {
						next_sum := subtotal + next
						if next_sum <= goal_total {
							append(&subtotals_1, next_sum)
						}

						next_product := subtotal * next
						if next_product <= goal_total {
							append(&subtotals_1, next_product)
						}
					}

					if len(subtotals_1) == 0 {
						answered_1 = true
					}

					temp_subtotals_2 := subtotals_2[:]
					defer delete(temp_subtotals_2)
					subtotals_2 = {}

					for subtotal in temp_subtotals_2 {
						next_sum := subtotal + next
						if next_sum <= goal_total {
							append(&subtotals_2, next_sum)
						}

						next_product := subtotal * next
						if next_product <= goal_total {
							append(&subtotals_2, next_product)
						}

						concatted_num := fmt.aprintf("%d%d", subtotal, next)
						defer delete(concatted_num)
						next_concat := strconv.atoi(concatted_num)
						if next_concat <= goal_total {
							append(&subtotals_2, next_concat)
						}
					}

					if len(subtotals_2) == 0 {
						answered_2 = true
					}

					if answered_1 && answered_2 {
						break iter_loop
					}
				}
				j += 1
			}

			if slice.any_of(subtotals_1[:], goal_total) {
				answer1 += goal_total
			}

			if slice.any_of(subtotals_2[:], goal_total) {
				answer2 += goal_total
			}
		}

		fmt.printfln("Answer 1: %d", answer1)
		fmt.printfln("Answer 2: %d", answer2)
	}
}
