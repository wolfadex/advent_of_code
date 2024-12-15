package day09

import "core:container/queue"
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

		grid, was_allocation := strings.replace_all(input_data, "\n", "")
		width := strings.index(input_data, "\n")
		height := strings.count(input_data, "\n") + 1

		for char, i in grid {
			if char == '0' {
				answer1 += find_9s(&grid, width, height, index_to_point(i, width))
				answer2 += find_ratings(&grid, width, height, index_to_point(i, width))
			}
		}

		if was_allocation {
			delete(grid)
		}

		fmt.printfln("Answer 1: %d", answer1)
		fmt.printfln("Answer 2: %d", answer2)
	}
}

find_9s :: proc(grid: ^string, width: int, height: int, start: Point) -> (total: int) {
	traversed: map[Point]struct {}
	defer delete(traversed)
	next_points: queue.Queue(Point)
	queue.init(&next_points)
	queue.append(&next_points, start)
	defer queue.destroy(&next_points)

	for point, ok := queue.pop_front_safe(&next_points);
	    ok;
	    point, ok = queue.pop_front_safe(&next_points) {

		point_idx := point_to_index(point, width)
		point_symbol := grid[point_idx]

		_, found := traversed[point]

		if found {
			continue
		}

		if point_symbol == '9' {
			total += 1
		} else {
			for dir in Direction_Vectors {
				neigh := dir + point

				if in_bounds(width, height, neigh) {
					symbol := grid[point_to_index(neigh, width)]
					next_sym := next_symbol(point_symbol)
					if symbol == next_sym {
						queue.append(&next_points, neigh)
					}
				}
			}
		}

		traversed[point] = {}
	}

	return
}

find_ratings :: proc(grid: ^string, width: int, height: int, start: Point) -> (total: int) {
	next_points: queue.Queue(Point)
	queue.init(&next_points)
	queue.append(&next_points, start)
	defer queue.destroy(&next_points)

	for point, ok := queue.pop_front_safe(&next_points);
	    ok;
	    point, ok = queue.pop_front_safe(&next_points) {

		point_idx := point_to_index(point, width)
		point_symbol := grid[point_idx]

		if point_symbol == '9' {
			total += 1
		} else {
			for dir in Direction_Vectors {
				neigh := dir + point

				if in_bounds(width, height, neigh) {
					symbol := grid[point_to_index(neigh, width)]
					next_sym := next_symbol(point_symbol)

					if symbol == next_sym {
						queue.append(&next_points, neigh)
					}
				}
			}
		}
	}

	return
}

next_symbol :: proc(symbol: u8) -> u8 {
	switch symbol {
	case '0':
		return '1'
	case '1':
		return '2'
	case '2':
		return '3'
	case '3':
		return '4'
	case '4':
		return '5'
	case '5':
		return '6'
	case '6':
		return '7'
	case '7':
		return '8'
	case '8':
		return '9'
	}

	return '0'
}

Point :: [2]int

Direction :: enum {
	UP,
	DOWN,
	LEFT,
	RIGHT,
}

Direction_Vectors :: [Direction][2]int {
	.UP    = {0, -1},
	.RIGHT = {+1, 0},
	.DOWN  = {0, +1},
	.LEFT  = {-1, 0},
}

in_bounds :: proc(width, height: int, point: Point) -> bool {
	return !(point.x < 0 || point.x >= width || point.y < 0 || point.y >= height)
}

point_to_index :: proc(point: Point, width: int) -> int {
	return (point.y * width) + point.x
}

index_to_point :: proc(index, width: int) -> (point: Point) {
	point.x = index %% width
	point.y = index / width

	return point
}
