package day05

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
		input_data := string(bytes)

		answer1: int
		answer2: int

		parts, _ := strings.split(input_data, "\n\n")
		defer delete(parts)

		forward_rules: map[int](map[int]struct {})

		rule_reg, _ := regex.create("^(\\d+)\\|(\\d+)$")
		defer regex.destroy_regex(rule_reg)

		for rule in strings.split_lines_iterator(&parts[0]) {
			rule_parts, found := regex.match_and_allocate_capture(rule_reg, rule)
			defer regex.destroy_capture(rule_parts)

			if found {
				left := strconv.atoi(rule_parts.groups[1])
				right := strconv.atoi(rule_parts.groups[2])

				f_vals := forward_rules[left]
				f_vals[right] = {}
				forward_rules[left] = f_vals
			}
		}

		page_nums: [dynamic]int
		defer delete(page_nums)
		valid_order: bool

		update_loop: for update in strings.split_lines_iterator(&parts[1]) {
			up := update
			valid_order = true
			delete(page_nums)
			page_nums = {}

			for page_str in strings.split_iterator(&up, ",") {
				page := strconv.atoi(page_str)
				append(&page_nums, page)

				after_pages, has_after_pages := forward_rules[page]

				if !has_after_pages {
					continue
				}

				for prev_page in page_nums {
					_, prev_found := after_pages[prev_page]
					if prev_found {
						valid_order = false
						// continue update_loop
					}
				}
			}

			if valid_order {
				mid_point := len(page_nums) / 2
				answer1 += page_nums[mid_point]
			} else {
				sorted_pages := resort_pages(page_nums[:], &forward_rules)
				mid_point := len(sorted_pages) / 2
				answer2 += sorted_pages[mid_point]
			}
		}

		for key, val in forward_rules {
			delete(val)
		}
		delete(forward_rules)

		fmt.printfln("Answer 1: %d", answer1)
		fmt.printfln("Answer 2: %d", answer2)
	}
}


resort_pages :: proc(pages: []int, rules: ^map[int](map[int]struct {})) -> []int {
	i: int

	pages_loop: for {

		if i == len(pages) {
			return pages
		}

		after_pages, has_after_pages := rules[pages[i]]

		if !has_after_pages {
			i += 1
			continue
		}

		for prev_page, prev_i in pages {
			if prev_i == i {
				break
			}
			_, prev_found := after_pages[prev_page]

			if prev_found {
				slice.swap(pages, i, prev_i)
				i = 0
				continue pages_loop
			}
		}

		i += 1
	}

	return pages
}
