#include <vector>
#include <fstream>
#include <string>
#include <iostream>
#include <unordered_map>
#include <cstdlib>

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

int arrange_rest(Row& orig_row, char* temp_state, int pos, int group_count) {
    /*
    if(group_count > orig_row.dam_groups.size()) {
        return 0;
    }
    */

    if(pos >= orig_row.state.size()) {
        /*
        if(group_count != orig_row.dam_groups.size()) {
            return 0;
        }
        */

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

    /*
    auto sub = temp_state.substr(0, pos);
    auto found = orig_row.memo.find(sub);

    if(found != orig_row.memo.end()) {
        return found->second;
    }
    */
    
    if(temp_state[pos] != '?') {
        if(temp_state[pos] == '#') {
            if(pos == 0 ||
               temp_state[pos - 1] != '#') {
                group_count += 1;
            }
        }

        return arrange_rest(orig_row, temp_state, pos + 1, group_count);
    }
 
    int total = 0;

    char mem[256];

    assert(orig_row.state.size() < sizeof(mem));

    std::memcpy(mem, temp_state, orig_row.state.size());

    temp_state[pos] = '#';

    int gc = group_count;
    if(pos == 0 || temp_state[pos - 1] != '#') {
        // We created a new group, so increment group count
        gc += 1;
    }
    
    total += arrange_rest(orig_row, temp_state, pos + 1, gc);

    std::memcpy(temp_state, mem, orig_row.state.size());
    gc = group_count;

    temp_state[pos] = '.';
    total += arrange_rest(orig_row, temp_state, pos + 1, gc);

    std::memcpy(temp_state, mem, orig_row.state.size());

    //orig_row.memo[sub] = total;

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
        char temp_state[256];

        assert(row.state.size() <= sizeof(temp_state));
        std::memcpy(temp_state, &row.state[0], row.state.size());

        total += arrange_rest(row, temp_state, 0, 0);
    }

    std::cout << total << '\n';

    std::quick_exit(0);
}