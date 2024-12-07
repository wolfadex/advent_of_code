package day06

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

		answer1: int
		answer2: int

		one_d, was_allocation := strings.replace_all(input_data, "\n", "")

		width := strings.index(input_data, "\n")

		facing: FACING = .UP
		initial_index := strings.index(one_d, "^")
		pos := index_to_grid(initial_index, width)

		visited, _ := traverse_level(pos, one_d[:], width)
		answer1 = len(visited)
		delete(visited)

		for space, i in one_d {
			if space == '.' {
				left := one_d[:i]
				right := one_d[i + 1:]
				temporal_grid := strings.join({left, "O", right}, "")
				visit, loops := traverse_level(pos, temporal_grid[:], width)

				if loops {
					answer2 += 1
				}

				delete(visit)
				delete(temporal_grid)
			}
		}

		if was_allocation {
			delete(one_d)
		}

		fmt.printfln("Answer 1: %d", answer1)
		fmt.printfln("Answer 2: %d", answer2)
	}
}

traverse_level :: proc(
	initial_pos: [2]int,
	grid: string,
	width: int,
) -> (
	visited: map[int](bit_set[FACING]),
	loops: bool,
) {
	pos := initial_pos
	facing: FACING = .UP

	for {
		i := grid_to_index(pos, width)

		if !in_bounds(pos, grid[:], width) {
			break
		}

		prev_visited := visited[i]
		if facing in prev_visited {
			return visited, true
		}

		visited[i] |= {facing}

		next := pos

		switch facing {
		case .UP:
			next += {0, -1}
		case .DOWN:
			next += {0, 1}
		case .RIGHT:
			next += {1, 0}
		case .LEFT:
			next += {-1, 0}
		}

		next_i := grid_to_index(next, width)

		if !in_bounds(next, grid[:], width) {
			pos = next
			continue
		}

		if grid[next_i] == '#' || grid[next_i] == 'O' {
			switch facing {
			case .UP:
				facing = .RIGHT
			case .RIGHT:
				facing = .DOWN
			case .DOWN:
				facing = .LEFT
			case .LEFT:
				facing = .UP
			}
		} else {
			pos = next
		}
	}

	return visited, false
}


FACING :: enum {
	UP,
	DOWN,
	LEFT,
	RIGHT,
}


grid_to_index :: proc(point: [2]int, width: int) -> int {
	return (point.y * width) + point.x
}

index_to_grid :: proc(index, width: int) -> (point: [2]int) {
	point.x = index %% width
	point.y = index / width

	return point
}

in_bounds :: proc(pos: [2]int, grid: string, width: int) -> bool {
	i := grid_to_index(pos, width)
	if i < 0 || i >= len(grid) || pos.x < 0 || pos.x >= width || pos.y < 0 {
		return false
	}

	return true
}
