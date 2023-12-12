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

template <typename Container>
void print_values(const Container& values) {
    std::cout << '{';
    for(bool first = true; const auto& value : values) {
        if(!first) {
            std::cout << ", ";
        }

        std::cout << value;
        first = false;
    }
    std::cout << "}\n";
}

std::int64_t arrange(std::string_view state, std::span<int> dam_groups) {
    // Note that the strings store a group length per byte, so they're just
    // std::vector<byte> really but I don't want to write a hash + equality check
    // for that so I'm leveraging this.
    //
    // Also note that if it ends with a zero that's indicating the previous group
    // ended.
    std::unordered_map<std::string, std::int64_t> valid_subgroup_counts;

    valid_subgroup_counts.try_emplace("", 1);

    for(auto state_ch : state) {
        std::unordered_map<std::string, std::int64_t> new_subgroups;

        std::string_view possible_chars;

        if(state_ch == '?') {
            possible_chars = "#.";
        } else if(state_ch == '#') {
            possible_chars = "#";
        } else if(state_ch == '.') {
            possible_chars = ".";
        }

        for(const auto& [groups, count] : valid_subgroup_counts) {
            for(auto ch : possible_chars) {
                auto new_groups = groups;

                if(ch == '.') {
                    if(groups.empty() || groups.back() != 0) {
                        new_groups.push_back(0);
                    }
                } else if (ch == '#') {
                    if(groups.empty()) {
                        new_groups.push_back(1);
                    } else {
                        new_groups.back() += 1;
                    }
                }

                new_subgroups[new_groups] += count;
            }
        }

        // Filter the new subgroups and only keep the valid ones
        std::erase_if(new_subgroups, [&](const auto& item) {
            auto& groups = item.first;

            auto size = groups.size();
            
            // This is just a sentinel value
            if(groups.back() == 0) {
                size -= 1;
            }

            if(size > dam_groups.size()) {
                return true;
            }

            if(size == 0) {
                return false;
            }

            if(groups[size - 1] > dam_groups[size - 1]) {
                return true;
            }

            for(int i = 0; i < size - 1; ++i) {
                if(groups[i] != dam_groups[i]) {
                    return true;
                }
            }

            return false;
        });

        valid_subgroup_counts = std::move(new_subgroups);
    }

    std::int64_t total = 0;

#ifdef DEBUG_PRINT
    std::cout << "dam_groups=";
    print_values(dam_groups);
#endif

    for(auto& [groups, count] : valid_subgroup_counts) {
        auto size = groups.size();

        if(!groups.empty() && groups.back() == 0) {
            size -= 1;
        }

        if(size != dam_groups.size()) {
            continue;
        }

        bool valid = true;

        for(int i = 0; i < size; ++i) {
            if(groups[i] != dam_groups[i]) {
                valid = false;
                break;
            }
        }

        if(valid) {
#ifdef DEBUG_PRINT
            std::cout << '{';
            for(bool first = true; auto ch : groups) {
                if(!first) {
                    std::cout << ", ";
                }
                std::cout << static_cast<int>(ch);
                first = false;
            }
            std::cout << "}\n";
#endif

            total += count;
        }
    }

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
        total += arrange(row.state, row.dam_groups);
    }

    std::cout << total << '\n';

    return 0;
}