#pragma once

#include <string>
#include <istream>
#include <string_view>

namespace advent {

template <typename Fn>
void for_each_line(std::istream& stream, Fn&& fn) {
    std::string line;
    while(std::getline(stream, line)) {
        fn(line);
    }
}

inline void read_lines(std::istream& stream, std::vector<std::string>& lines) {
    for_each_line(stream, [](std::string_view line) {
        lines.emplace_back(line);
    });
}

}