#pragma once

#include <charconv>
#include <cstring>
#include <cassert>
#include <vector>
#include <string_view>

namespace advent {

inline constexpr std::string_view DEFAULT_WHITESPACE_CHARS = " \t\n\r";

template <typename Fn>
void for_each_split_part(std::string_view str, std::string_view delim, Fn&& fn) {
    for(;;) {  
        auto next_pos = str.find(delim);
        auto sub_str = str.substr(0, next_pos);

        fn(sub_str);

        if(next_pos == std::string_view::npos) {
            return;
        }

        str.remove_prefix(next_pos + 1);
    }
}

inline std::string_view strip_left(std::string_view str, std::string_view whitespace = DEFAULT_WHITESPACE_CHARS) {
    auto start_pos = str.find_first_not_of(whitespace);
    str.remove_prefix(start_pos);

    return str;
}

inline std::string_view strip_right(std::string_view str, std::string_view whitespace = DEFAULT_WHITESPACE_CHARS) {
    auto end_pos = str.find_last_not_of(whitespace);
    str.remove_suffix(str.size() - end_pos - 1);

    return str;
}

inline std::string_view strip(std::string_view str, std::string_view whitespace = DEFAULT_WHITESPACE_CHARS) {
    str = strip_left(str, whitespace);
    str = strip_right(str, whitespace);

    return str;
}

inline void test_split_string() {
    std::string_view str = "a,b,c";

    std::vector<std::string_view> strs;

    for_each_split_part(str, ",", [&](auto part) {
        strs.push_back(part);
    });

    assert(strs.size() == 3);

    assert(strs[0] == "a");
    assert(strs[1] == "b");
    assert(strs[2] == "c");
}

template <typename I>
I string_to_number(std::string_view str) {
    I result;

    [[maybe_unused]] auto [ptr, ec] = std::from_chars(str.data(), str.data() + str.size(), result);

    assert(ec == std::errc{});

    return result;
}

inline void test_strip() {
    std::string_view str = " \t\nabc \r";

    assert(strip_left(str) == "abc \r");
    assert(strip_right(str) == " \t\nabc");
    assert(strip(str) == "abc");
}

inline void test_string_utils() {
    test_split_string();
    test_strip();
}

}