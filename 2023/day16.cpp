#include <iostream>
#include <fstream>
#include <vector>
#include <algorithm>
#include <future>

#include "string_utils.hpp"
#include "span_utils.hpp"
#include "file_utils.hpp"

using namespace advent;

int main(int argc, char** argv) {
    if(argc < 2) {
        std::cerr << "FILE! GIVE FILE!\n";
        return 1;
    }

    std::ifstream input{argv[1]};

    if(!input.good()) {
        std::cerr << "NO FILE WORKY\n";
        return 1;
    }

    struct Beam {
        int x = 0;
        int y = 0;

        int dx = 0;
        int dy = 0;    

        bool dead = false;
    };

    auto rot90_ccw = [&](Beam& b) {
        if(b.dx == 1) {
            b.dx = 0;
            b.dy = -1;
        } else if(b.dy == -1) {
            b.dy = 0;
            b.dx = -1;
        } else if(b.dx == -1) {
            b.dx = 0;
            b.dy = 1;
        } else if(b.dy == 1) {
            b.dx = 1;
            b.dy = 0;
        }
    };

    auto rot90_cw = [&](Beam& b) {
        if(b.dx == 1) {
            b.dx = 0;
            b.dy = 1;
        } else if(b.dy == 1) {
            b.dx = -1;
            b.dy = 0;
        } else if(b.dx == -1) {
            b.dx = 0;
            b.dy = -1;
        } else if(b.dy == -1) {
            b.dx = 1;
            b.dy = 0;
        }
    };

    std::vector<std::vector<char>> chars;

    for_each_line(input, [&](const auto& line) {
        chars.emplace_back();
        chars.back().reserve(line.size());

        for(auto ch : line) {
            chars.back().push_back(ch);
        }
    });

    int w = chars.at(0).size();
    int h = chars.size();

    const auto total_init_sync = [&](int ix, int iy, int idx, int idy) {
        std::vector<Beam> beams;

        beams.emplace_back();
        beams.back().x = ix;
        beams.back().y = iy;
        beams.back().dx = idx;
        beams.back().dy = idy;

        // 0 == false
        std::vector<std::vector<int>> eng;

        eng.resize(h);

        for(auto& v : eng) {
            v.resize(w, 0);
        }

        int prev_total = 0;
        int total_seen_times = 0;

        while(beams.size()) {
            // Int iteration so we can push back while iterating
            for(int i = 0; i < beams.size(); ++i) {
                auto& b = beams[i];

                eng[b.y][b.x] = 1;
                auto ch = chars[b.y][b.x];

                //std::cout << i << ',' << b.x << ',' << b.y << ',' << b.dx << ',' << b.dy << ',' << ch << '\n';

                if(ch == '/') {
                    if(b.dy != 0) {
                        rot90_cw(b);
                    } else {
                        rot90_ccw(b);
                    }
                } else if(ch == '\\') {
                    if(b.dx != 0) {
                        rot90_cw(b);
                    } else {
                        rot90_ccw(b);
                    }
                } else if(ch == '|') {
                    if(b.dx != 0) {
                        Beam new_b = b;
                        new_b.dx = 0;
                        new_b.dy = -1;

                        beams.emplace_back(std::move(new_b));                    

                        beams[i].dx = 0;
                        beams[i].dy = 1;
                    }
                } else if(ch == '-') {
                    if(b.dy != 0) {
                        Beam new_b = b;
                        new_b.dx = -1;
                        new_b.dy = 0;

                        beams.emplace_back(std::move(new_b));

                        beams[i].dx = 1;
                        beams[i].dy = 0;
                    }
                }

                {
                    auto& b = beams[i];

                    int nx = beams[i].x + beams[i].dx;
                    int ny = beams[i].y + beams[i].dy;

                    if(nx < 0 || nx >= w || ny < 0 || ny >= h) {
                        b.dead = true;
                        continue;
                    }

                    b.x = nx;
                    b.y = ny;
                }
            }

            auto removed = std::remove_if(beams.begin(), beams.end(),
                [](auto& b) { return b.dead; });
            beams.erase(removed, beams.end());


            int total = 0;

            for(auto& e : eng) {
                for(auto& v : e) {
                    //std::cout << (v ? '#' : '.');
                    total += v;
                }
                //std::cout << '\n';
            }

            if(total == prev_total) {
                total_seen_times += 1;

                if(total_seen_times % 5 == 0) {
                    std::cout << "Seen " << total << " " << total_seen_times << " times\n";
                }
            } else {
                total_seen_times = 0; 
            }

            if(total_seen_times >= 5 /* (w * h) */) {
                return total;
            }

            prev_total = total;
        }

        return 0;
    };

    const auto total_init = [&](auto... params) {
        return std::async(std::launch::async, total_init_sync, params...);
    };

    int max_total = 0;

    for(int x = 0; x < w; ++x) {
        std::future<int> res[] = {
            total_init(x, 0, 1, 0),
            total_init(x, 0, -1, 0),
            total_init(x, h-1, 1, 0),
            total_init(x, h-1, -1, 0),
            total_init(x, 0, 0, 1),
            total_init(x, h-1, 0, -1),
        };

        for(auto& r : res){
            max_total = std::max(max_total, r.get());
        }    
    }

    for(int y = 0; y < w; ++y) {
        std::future<int> res[] = {
            total_init(0, y, 1, 0),
            total_init(0, y, -1, 0),
            total_init(w-1, y, 1, 0),
            total_init(w-1, y, -1, 0),
            total_init(0, y, 0, 1),
            total_init(w-1, y, 0, -1),
        };

        for(auto& r : res){
            max_total = std::max(max_total, r.get());
        }
    }

    std::cout << "Max total: " << max_total << '\n';

    return 0;
}