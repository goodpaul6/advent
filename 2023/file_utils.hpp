#pragma once

namespace advent {

template <typename Fn>
void for_each_line(std::istream& stream, Fn&& fn) {
    std::string line;
    while(std::getline(stream, line)) {
        fn(line);
    }
}

}