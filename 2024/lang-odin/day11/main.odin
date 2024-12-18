package day11


import "core:fmt"
import "core:log"
import "core:mem"
import "core:os"
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

	if len(os.args) < 2 {
		os.exit(1)
	}

	if len(os.args) < 3 {
		fmt.println("After the input file, please enter a number of blinks to make")
		os.exit(1)
	}

	filePath := os.args[1]
	times_to_blink := min(100, strconv.atoi(os.args[2]))

	if bytes, success := os.read_entire_file_from_filename(filePath); success {
		defer delete(bytes)
		input_data := string(bytes)

		answer1: int

		stones: map[int]int
		defer delete(stones)

		for stone in strings.split_iterator(&input_data, " ") {
			id := strconv.atoi(stone)
			stones[id] += 1
		}

		for blinks_rem := 0; blinks_rem < times_to_blink; blinks_rem += 1 {
			intermediate_counts: map[int]int

			for stone, qty in stones {
				if stone == 0 {
					intermediate_counts[1] += qty
				} else {
					size := num_digits(stone)
					if size % 2 == 0 {
						digits := fmt.tprintf("%d", stone)

						left_half := digits[:size / 2]
						right_half := digits[size / 2:]

						left := strconv.atoi(left_half)
						right := strconv.atoi(right_half)

						intermediate_counts[left] += qty
						intermediate_counts[right] += qty
					} else {
						intermediate_counts[stone * 2024] += qty
					}
				}
			}
			delete(stones)
			stones = intermediate_counts
		}

		for _, qty in stones {
			answer1 += qty
		}

		fmt.printfln("Answer: %d", answer1)
	}
}

num_digits :: proc(n: int) -> (count: int) {
	num := n
	for num != 0 {
		// Remove rightmost digit
		num = num / 10

		// Increment digit count by 1
		count += 1
	}

	return
}


// dive_blink :: proc(stone: string, blinks_rem: int, cache: ^map[string]int) -> (width: int) {
// 	if blinks_rem < 1 {
// 		return 1
// 	}


// 	key := fmt.aprintf("%s_%d", stone, (blinks_rem - 1))
// 	defer delete(key)
// 	depth_width := cache[key]

// 	if depth_width > 0 {
// 		return depth_width
// 	}

// 	if stone == "0" {
// 		width = dive_blink("1", blinks_rem - 1, cache)
// 	} else {
// 		size := len(stone)
// 		if size % 2 == 0 {
// 			left_half := stone[:size / 2]
// 			right_half := stone[size / 2:]

// 			left_val := strconv.atoi(left_half)
// 			right_val := strconv.atoi(right_half)

// 			buf_left: [20]byte
// 			left := strconv.itoa(buf_left[:], left_val)
// 			defer delete(left)

// 			buf_right: [20]byte
// 			right := strconv.itoa(buf_right[:], right_val)
// 			defer delete(right)

// 			left_count := dive_blink(left, blinks_rem - 1, cache)
// 			right_count := dive_blink(right, blinks_rem - 1, cache)

// 			width = left_count + right_count
// 		} else {
// 			stone_val := strconv.atoi(stone)
// 			stone_val *= 2024
// 			buf: [20]byte
// 			multi := strconv.itoa(buf[:], stone_val)
// 			defer delete(multi)

// 			width = dive_blink(multi, blinks_rem - 1, cache)
// 		}
// 	}

// 	cache[key] = width

// 	return
// }
