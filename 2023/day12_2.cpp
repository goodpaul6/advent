#include <vector>
#include <fstream>
#include <string>
#include <iostream>
#include <unordered_map>

#include "string_utils.hpp"
#include "span_utils.hpp"
#include "file_utils.hpp"

namespace {

struct Row {
    std::unordered_map<std::string, int> memo;

    std::string state;
    std::vector<int> dam_groups;
};

bool is_valid_arrangement(std::string_view state, std::span<int> dam_groups) {
    int group_size = 0;
    int orig_group_iter = 0;

    for(auto ch : state) {
        if(ch == '#') {
            group_size += 1;
        } else if(ch == '.') {
            if(group_size > 0) {
                if(orig_group_iter >= dam_groups.size() ||
                    group_size != dam_groups[orig_group_iter]) {
                    return false;
                }

                orig_group_iter += 1;
            }

            group_size = 0;
        } else {
            std::cout << state << '\n';
            // Should be fully arranged by now
            assert(false);
        }
    }

    // If this is a terminating group, make sure it matches
    return ((group_size == 0 && orig_group_iter == dam_groups.size()) ||
        (orig_group_iter == dam_groups.size() - 1 &&
        group_size == dam_groups[orig_group_iter])) ? 1 : 0;
}

int arrange_rest(Row& orig_row, std::string& temp_state, int pos) {
    if(pos >= temp_state.size()) {
        // Now that we've done the arrangement down this path,
        // count it as 1 if it's valid.
        auto valid = is_valid_arrangement(temp_state, 
            {orig_row.dam_groups.begin(), orig_row.dam_groups.end()});

        /*
        if(valid) {
            std::cout << orig_row.state << ',' << temp_state << '\n';
        }
        */

        return valid ? 1 : 0;
    }

    auto sub = temp_state.substr(pos);
    auto found = orig_row.memo.find(sub);

    if(found != orig_row.memo.end()) {
        return found->second;
    }
    
    if(temp_state[pos] != '?') {
        return arrange_rest(orig_row, temp_state, pos + 1);
    }
    
    int total = 0;

    std::string mem = temp_state;

    temp_state[pos] = '#';
    total += arrange_rest(orig_row, temp_state, pos + 1);

    temp_state = mem;

    temp_state[pos] = '.';
    total += arrange_rest(orig_row, temp_state, pos + 1);

    orig_row.memo[sub] = total;

    return total;
}

}

int main() {
    using namespace advent;

    std::ifstream input{"inputs/day12.txt"};

    std::vector<Row> rows;

    int test[] = {1, 1, 3};

    assert(is_valid_arrangement("#.#.###", test));
    assert(!is_valid_arrangement("#...###", test));

    int test2[] = {3, 2, 1};

    assert(!is_valid_arrangement(".###..##....", test2));

    for_each_line(input, [&](const auto& line) {
        Row row;

        for_each_split_part(line, " ", [&](auto part) {
            if(row.state.empty()) {
                row.state = std::string{part};
            } else {
                for_each_split_part(part, ",", [&](auto num_str) {
                    row.dam_groups.push_back(
                        string_to_number<int>(num_str)
                    );
                });
            }
        });

        rows.push_back(std::move(row));
    });

    int total = 0;

    for(auto& row : rows) {
        std::string state{row.state};

        total += arrange_rest(row, state, 0);
    }

    std::cout << total << '\n';

    return 0;
}