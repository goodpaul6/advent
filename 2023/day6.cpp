#include <iostream>
#include <vector>
#include <cassert>

#include "string_utils.hpp"
#include "file_utils.hpp"

using namespace advent;

int main() {
    std::vector<int> times;
    std::vector<int> dists;

    for_each_line(std::cin, [&](auto str) {
        std::vector<int>* dest = nullptr;

        for_each_split_part(str, DEFAULT_WHITESPACE_CHARS, [&](auto str) {
            std::cout << str << '\n';
            if(str == "Time:") {
                dest = &times;
            } else if(str == "Distance:") {
                dest = &dists;
            } else {
                assert(dest);
                dest->push_back(string_to_number<int>(str));
            }
            return true;
        });

        return true;
    });

    int ways_to_win_all = 1;

    for(int i = 0; i < times.size(); ++i) {
        int time = times[i];
        int dist = dists[i];

        int win_times = 0;

        for(int p_time = 1; p_time <= time; ++p_time) {
            if(static_cast<float>(dist + 1) / static_cast<float>(time - p_time) > dist) {
                win_times += 1;
            }
        }

        ways_to_win_all *= win_times;
    }

    std::cout << ways_to_win_all << '\n';

    return 0;
}