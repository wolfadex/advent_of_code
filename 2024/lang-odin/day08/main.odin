package day08

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

		grid_str, was_allocation := strings.replace_all(input_data, "\n", "")
		width := strings.index(input_data, "\n")

		attennas: map[rune]([dynamic]Point)

		for char, i in grid_str {
			if char != '.' {
				attenna_points := attennas[char]
				append(&attenna_points, index_to_point(i, width))
				attennas[char] = attenna_points
			}
		}

		antinodes_1: map[Point]struct {}
		antinodes_2: map[Point]struct {}

		for _, points in attennas {
			for point, i in points {
				for p := i + 1; p < len(points); p += 1 {
					point2 := points[p]
					dist := point2 - point

					anti_1_1 := point - dist
					if in_bounds(anti_1_1, grid_str, width) {
						antinodes_1[anti_1_1] = {}
					}

					anti_1_2 := point2 + dist
					if in_bounds(anti_1_2, grid_str, width) {
						antinodes_1[anti_1_2] = {}
					}

					for anti_2_1 := point; in_bounds(anti_2_1, grid_str, width); anti_2_1 -= dist {
						antinodes_2[anti_2_1] = {}
					}

					for anti_2_2 := point2;
					    in_bounds(anti_2_2, grid_str, width);
					    anti_2_2 += dist {
						antinodes_2[anti_2_2] = {}
					}
				}
			}

			delete(points)
		}

		answer1 = len(antinodes_1)
		answer2 = len(antinodes_2)

		delete(antinodes_1)
		delete(antinodes_2)

		delete(attennas)

		if was_allocation {
			delete(grid_str)
		}

		fmt.printfln("Answer 1: %d", answer1)
		fmt.printfln("Answer 2: %d", answer2)
	}
}

Point :: distinct [2]int

point_to_index :: proc(point: Point, width: int) -> int {
	return (point.y * width) + point.x
}

index_to_point :: proc(index, width: int) -> (point: Point) {
	point.x = index %% width
	point.y = index / width
	return
}

in_bounds :: proc(pos: Point, grid: string, width: int) -> bool {
	i := point_to_index(pos, width)
	if i < 0 || i >= len(grid) || pos.x < 0 || pos.x >= width || pos.y < 0 {
		return false
	}

	return true
}
