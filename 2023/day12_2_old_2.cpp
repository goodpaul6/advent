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

std::int64_t arrange_rest(Row& orig_row, char* temp_state, int pos, int gsize, int giter) {

    if(giter > orig_row.dam_groups.size()) {
        return 0;
    }

    if(gsize > 0 && gsize > orig_row.dam_groups[giter]) {
        return 0;
    }

    if(pos >= orig_row.state.size()) {
        auto valid = (giter == orig_row.dam_groups.size() && gsize == 0) ||
           (giter == orig_row.dam_groups.size() - 1 && gsize == orig_row.dam_groups.back());

        /*
        // Now that we've done the arrangement down this path,
        // count it as 1 if it's valid.
        auto valid = is_valid_arrangement({temp_state, orig_row.state.size()}, 
            {orig_row.dam_groups.begin(), orig_row.dam_groups.end()});
        */

#if 1
        if(valid) {
            static int count = 0;

            count += 1;
            if(count % 1'000'000 == 0) {
                std::cout << count << " arrangements found.\n";
            }
        }
#endif

        return valid ? 1 : 0;
    }
    
    if(temp_state[pos] != '?') {
        if(temp_state[pos] == '#') {
            gsize += 1;
        } else if(temp_state[pos] == '.') {
            if(gsize > 0) {
                if(gsize != orig_row.dam_groups[giter]) {
                    return 0;
                }

                giter += 1;
            }

            gsize = 0;
        }

        return arrange_rest(orig_row, temp_state, pos + 1, gsize, giter);
    }
    
    std::int64_t total = 0;

    char mem[256];

    assert(orig_row.state.size() <= sizeof(mem));

    auto osize = orig_row.state.size();

    std::memcpy(mem, temp_state, osize);

    int prev_gsize = gsize;
    int prev_giter = giter;

    temp_state[pos] = '#';
    gsize += 1;

    total += arrange_rest(orig_row, temp_state, pos + 1, gsize, giter);

    std::memcpy(temp_state, mem, osize);

    temp_state[pos] = '.';

    giter = prev_giter;

    if(prev_gsize > 0) {
        if(prev_gsize != orig_row.dam_groups[prev_giter]) {
            return total;
        }

        giter += 1;
    }

    gsize = 0;

    total += arrange_rest(orig_row, temp_state, pos + 1, gsize, giter);

    std::memcpy(temp_state, mem, osize);

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

#if 1
        auto temp = row.state;
        auto temp_dam_groups = row.dam_groups;

        for(int i = 0; i < 4; ++i) {
            row.state += '?';
            row.state += temp;

            row.dam_groups.insert(row.dam_groups.end(), temp_dam_groups.begin(), temp_dam_groups.end());
        }
#endif

        rows.push_back(std::move(row));
    });

    int64_t total = 0;

    for(auto& row : rows) {
        char state[256];
        std::memcpy(state, row.state.data(), row.state.size());

        total += arrange_rest(row, state, 0, 0, 0);
    }

    std::cout << total << '\n';

    return 0;
}