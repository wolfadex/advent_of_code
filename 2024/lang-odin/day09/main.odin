package day09

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

		answer1 = solve_part_1(input_data)
		answer2 = solve_part_2(input_data)

		fmt.printfln("Answer 1: %d", answer1)
		fmt.printfln("Answer 2: %d", answer2)
	}
}


solve_part_1 :: proc(input_data: string) -> (answer1: int) {
	start_gap_str := utf8.rune_at(input_data, 0)
	start_gap := utf8.runes_to_string({start_gap_str})
	file_position_start := strconv.atoi(start_gap)
	delete(start_gap)

	disk_start := 1
	disk_end := len(input_data) - 1

	start_file_id := 1
	end_file_id := disk_end / 2

	disk_end_count: int

	outer: for disk_start < disk_end {
		gap_rune := utf8.rune_at(input_data, disk_start)
		disk_start += 1
		gap_str := utf8.runes_to_string({gap_rune})
		gap := strconv.atoi(gap_str)
		delete(gap_str)

		if disk_end_count == 0 {
			end_block_rune := utf8.rune_at(input_data, disk_end)
			end_block_str := utf8.runes_to_string({end_block_rune})
			defer delete(end_block_str)
			disk_end_count = strconv.atoi(end_block_str)
		}

		for gap > 0 {
			answer1 += file_position_start * end_file_id
			file_position_start += 1
			disk_end_count -= 1
			gap -= 1

			if disk_end_count == 0 {
				end_file_id -= 1
				disk_end -= 2

				if disk_start >= disk_end {
					break outer
				}
				end_block_rune := utf8.rune_at(input_data, disk_end)
				end_block_str := utf8.runes_to_string({end_block_rune})
				defer delete(end_block_str)
				disk_end_count = strconv.atoi(end_block_str)
			}
		}

		if disk_start < disk_end {
			start_block_rune := utf8.rune_at(input_data, disk_start)
			disk_start += 1
			start_block_str := utf8.runes_to_string({start_block_rune})
			defer delete(start_block_str)
			start_block := strconv.atoi(start_block_str)
			start_count := start_block

			for start_count > 0 {
				answer1 += file_position_start * start_file_id
				file_position_start += 1
				start_count -= 1
			}

			start_file_id += 1
		} else if disk_start == disk_end {
			start_block_rune := utf8.rune_at(input_data, disk_start)
			disk_start += 1
			start_block_str := utf8.runes_to_string({start_block_rune})
			defer delete(start_block_str)
			start_block := strconv.atoi(start_block_str)

			answer1 += file_position_start * start_file_id
			file_position_start += 1
		}
	}

	return
}

SLICE :: union {
	EMPTY_SLICE,
	FILE_SLICE,
}

EMPTY_SLICE :: struct {
	size: int,
}
FILE_SLICE :: struct {
	size: int,
	id:   int,
}


solve_part_2 :: proc(input_data: string) -> (answer2: int) {
	slices: [dynamic]SLICE
	defer delete(slices)
	file_id := 0

	for r, i in input_data {
		size_str := utf8.runes_to_string({r})
		defer delete(size_str)
		size := strconv.atoi(size_str)

		if i % 2 == 0 {
			append(&slices, FILE_SLICE{size = size, id = file_id})
			file_id += 1
		} else {
			append(&slices, EMPTY_SLICE{size = size})
		}
	}

	id_to_move := file_id - 1

	for attempt_to_move(&slices, &id_to_move) {
		//
	}

	position: int

	for slc in slices {
		switch s in slc {
		case nil:
		case EMPTY_SLICE:
			position += s.size
		case FILE_SLICE:
			for i := 0; i < s.size; i += 1 {
				answer2 += s.id * (position + i)
			}
			position += s.size
		}
	}

	return
}


attempt_to_move :: proc(slices: ^[dynamic]SLICE, id_to_move: ^int) -> bool {
	file_to_move: SLICE
	max_move_index: int

	find_slice: for sl, i in slices {
		switch s in sl {
		case EMPTY_SLICE:
		case FILE_SLICE:
			if s.id == id_to_move^ {
				file_to_move = s
				max_move_index = i
				id_to_move^ -= 1
				break find_slice
			}
		}
	}


	switch f in file_to_move {
	case nil:
	case EMPTY_SLICE:
	case FILE_SLICE:
		move_check: for sl, i in slices {
			if i >= max_move_index {
				break move_check
			}
			switch s in sl {
			case FILE_SLICE:
			case EMPTY_SLICE:
				if s.size >= f.size {
					slices[i] = EMPTY_SLICE {
						size = s.size - f.size,
					}
					slices[max_move_index] = EMPTY_SLICE {
						size = f.size,
					}
					inject_at(slices, i, f)
					break move_check
				}
			}
		}
	}

	if id_to_move^ < 1 {
		return false
	}

	return true
}
