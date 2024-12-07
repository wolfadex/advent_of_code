package day04

import "core:fmt"
import "core:log"
import "core:mem"
import "core:os"
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
		one_d, was_allocation := strings.replace_all(input_data, "\n", "")

		answer1: int
		answer2: int

		width := strings.index(input_data, "\n")


		grid_loop: for char, i in one_d {
			if char == 'X' {
				dir_loop: for dir in DIRECTIONS {
					letter_point := index_to_grid(i, width)

					for letter in MAS {

						letter_point += dir

						if letter_point.x < 0 || letter_point.x >= width || letter_point.y < 0 {
							continue dir_loop
						}

						index_to_search := grid_to_index(letter_point, width)

						if index_to_search < 0 ||
						   index_to_search >= len(one_d) ||
						   one_d[index_to_search] != letter {
							continue dir_loop
						}
					}
					answer1 += 1
				}
			} else if char == 'A' {
				letter_point := index_to_grid(i, width)

				if letter_point.x == 0 || letter_point.x == width - 1 {
					continue grid_loop
				}

				up_left := grid_to_index(letter_point + {-1, -1}, width)
				up_right := grid_to_index(letter_point + {1, -1}, width)
				down_left := grid_to_index(letter_point + {-1, 1}, width)
				down_right := grid_to_index(letter_point + {1, 1}, width)

				if in_bounds(up_left, one_d[:], width) &&
				   in_bounds(up_right, one_d[:], width) &&
				   in_bounds(down_left, one_d[:], width) &&
				   in_bounds(down_right, one_d[:], width) {

					forward: [3]u8 = {one_d[up_left], 'A', one_d[down_right]}
					backward: [3]u8 = {one_d[up_right], 'A', one_d[down_left]}

					if (forward == MAS || forward == SAM) && (backward == MAS || backward == SAM) {
						answer2 += 1
					}
				}
			}
		}


		if was_allocation {
			defer delete(one_d)
		}

		fmt.printfln("Answer 1: %d", answer1)
		fmt.printfln("Answer 2: %d", answer2)
	}
}

in_bounds :: proc(index_to_search: int, grid: string, width: int) -> bool {
	if index_to_search < 0 || index_to_search >= len(grid) {
		return false
	}

	return true
}

grid_to_index :: proc(point: [2]int, width: int) -> int {

	return (point.y * width) + point.x
}

index_to_grid :: proc(index, width: int) -> (point: [2]int) {

	point.x = index %% width
	point.y = index / width

	return point
}

DIRECTION :: enum {
	UP,
	DOWN,
	LEFT,
	RIGHT,
	UP_RIGHT,
	UP_LEFT,
	DOWN_RIGHT,
	DOWN_LEFT,
}

DIRECTIONS :: [DIRECTION][2]int {
	.UP         = {0, -1},
	.DOWN       = {0, 1},
	.RIGHT      = {1, 0},
	.LEFT       = {-1, 0},
	.UP_RIGHT   = {1, -1},
	.UP_LEFT    = {-1, -1},
	.DOWN_RIGHT = {1, 1},
	.DOWN_LEFT  = {-1, 1},
}

MAS: [3]u8 : {'M', 'A', 'S'}

SAM: [3]u8 : {'S', 'A', 'M'}
