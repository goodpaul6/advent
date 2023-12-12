#include <vector>
#include <fstream>
#include <string>
#include <iostream>
#include <unordered_map>

#include "string_utils.hpp"
#include "span_utils.hpp"
#include "file_utils.hpp"

using namespace advent;

namespace {

struct Row {
    std::unordered_map<std::string, int> memo;

    std::string state;
    std::vector<int> dam_groups;
};

bool is_valid_arrangement(std::string_view state, std::span<int> dam_groups) {
    int group_iter = 0;
    bool valid = true;

    for_each_split_part(state, ".", [&](auto part) {
        if(part.empty()) {
            return true;
        }

        int group_size = part.size();

        if(group_size != dam_groups[group_iter]) {
            valid = false;
            return false;
        }

        group_iter += 1;
        return true;
    });

    return valid && group_iter == dam_groups.size();
}

int arrange_rest(Row& orig_row, char* temp_state, int pos) {
    if(pos >= orig_row.state.size()) {
        // Now that we've done the arrangement down this path,
        // count it as 1 if it's valid.
        auto valid = is_valid_arrangement({temp_state, orig_row.state.size()}, 
            {orig_row.dam_groups.begin(), orig_row.dam_groups.end()});

        /*
        if(valid) {
            std::cout << orig_row.state << ',' << temp_state << '\n';
        }
        */

        return valid ? 1 : 0;
    }
    
    if(temp_state[pos] != '?') {
        return arrange_rest(orig_row, temp_state, pos + 1);
    }
    
    int total = 0;

    char mem[256];

    assert(orig_row.state.size() <= sizeof(mem));

    std::memcpy(mem, temp_state, orig_row.state.size());

    temp_state[pos] = '#';
    total += arrange_rest(orig_row, temp_state, pos + 1);

    std::memcpy(temp_state, mem, orig_row.state.size());

    temp_state[pos] = '.';
    total += arrange_rest(orig_row, temp_state, pos + 1);

    std::memcpy(temp_state, mem, orig_row.state.size());

    return total;
}

}

int main() {
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

                    return true;
                });
            }

            return true;
        });

#if 0
        // Repeat 5 times
        auto temp = row.state;
        auto temp_dam_groups = row.dam_groups;

        for(int i = 0; i < 5; ++i) {
            row.state += '?';
            row.state += temp;

            row.dam_groups.insert(row.dam_groups.end(), temp_dam_groups.begin(), temp_dam_groups.end());
        }
#endif

        rows.push_back(std::move(row));
    });

    int total = 0;

    for(auto& row : rows) {
        char state[256];
        std::memcpy(state, row.state.data(), row.state.size());

        total += arrange_rest(row, state, 0);
    }

    std::cout << total << '\n';

    return 0;
}