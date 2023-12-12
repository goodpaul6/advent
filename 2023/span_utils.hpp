#pragma once

#include <span>

namespace advent {

template <typename T>
inline void slice(std::span<T> items, std::ptrdiff_t start, std::ptrdiff_t stop) {
    if(start < 0) {
        start = items.size() + start;
    }

    if(stop < 0) {
        stop = items.size() + stop;
    }

    if(stop > items.size()) {
        stop = items.size();
    }

    auto size = stop - start;

    return items.subspan(static_cast<std::size_t>(start), static_cast<std::size_t>(size));
}

}