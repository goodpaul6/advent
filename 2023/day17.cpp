#include <iostream>
#include <vector>
#include <unordered_map>
#include <unordered_set>
#include <cstring>
#include <climits>
#include <algorithm>
#include <queue>

#include "file_utils.hpp"
#include "string_utils.hpp"

using namespace advent;

namespace {

struct PosOff {
    uint8_t x = 0;
    uint8_t y = 0;
    
    // Distance moved in each dir from x and y
    int8_t dx = 0;
    int8_t dy = 0;

    int hl_so_far = 0;
}; 

}

int main() { 
    std::vector<std::vector<int>> blocks;

    for_each_line(std::cin, [&](auto line) {
        blocks.emplace_back();

        for(auto ch : line) {
            blocks.back().push_back(ch - '0');
        }
    });

    int w = blocks[0].size();
    int h = blocks.size();

    struct HeatLossDir {
        int hl_n = INT_MAX;
        int hl_s = INT_MAX;
        int hl_e = INT_MAX;
        int hl_w = INT_MAX;
    };

    std::unordered_map<int, HeatLossDir> min_at_pos_dir;

    auto try_put_min = [&](int x, int y, int dx, int dy, int hl) {
        auto& prev = min_at_pos_dir[x + y * w];
        
        if(dx < 0 && hl < prev.hl_w) {
            prev.hl_w = hl;
            return true;
        }

        if(dx > 0 && hl < prev.hl_e) {
            prev.hl_e = hl;
            return true;
        }

        if(dy < 0 && hl < prev.hl_n) {
            prev.hl_n = hl;
            return true;
        }

        if(dy > 0 && hl < prev.hl_s) {
            prev.hl_s = hl;
            return true;
        }

        return false;
    };

    auto get_min = [&](int x, int y) {
        auto& prev = min_at_pos_dir[x + y * w];

        return std::min(
            prev.hl_n,
            std::min(
                prev.hl_s,
                std::min(
                    prev.hl_e,
                    prev.hl_w
                )
            )
        );
    };

    std::queue<PosOff> pos_offs;

    PosOff p_right;
    p_right.dx = 1;

    PosOff p_down;
    p_down.dy = 1;

    pos_offs.emplace(p_right);
    pos_offs.emplace(p_down);

    // Submissions:
    // 984 too high
    // Just a guess 735 too low
    // 860 is not right answer but in the ballpark I guess?
    // 872 is also not right answer

    while(!pos_offs.empty()) {
        auto pos = pos_offs.front();   
        pos_offs.pop();

        int xx = pos.x + pos.dx;
        int yy = pos.y + pos.dy;

        if((pos.dx != 0 && pos.dy != 0) || (pos.dx == 0 && pos.dy == 0) || std::abs(pos.dx) > 3 || std::abs(pos.dy) > 3 ||
            xx < 0 || xx >= w || yy < 0 || yy >= h) {
            continue;
        }

        // auto pos_s = pos.to_hashable_string();
  
        int c = blocks[yy][xx];
        
        pos.hl_so_far += c;

        // std::cout << xx << ',' << yy << ',' << static_cast<int>(pos.dx) << ',' << static_cast<int>(pos.dy) << ',' << static_cast<int>(pos.hl_so_far) << '\n';

        if(!try_put_min(xx, yy, pos.dx, pos.dy, pos.hl_so_far)) {
            // There is a cheaper way to get here (GOING IN THE SAME DIRECTION), don't bother going down this path further
            continue;
        }

#if 0
        auto& prev_min = min_at_pos_dir[xx + yy * w];
        
        if(prev_min == 0 || pos.hl_so_far < prev_min) {
            prev_min = pos.hl_so_far;
        } else {

            // FIXME(Apaar): Actually, the above is not sufficient. Sometimes, we have a path that
            // costs more locally but allows us to curve around something that would cost more globally.
            continue;
        }
#endif


        // Bottom right corner
        if(xx == w - 1 && yy == h - 1) {
            continue;
        }

        if(pos.dx > 0 && pos.dx < 3) {
            auto p = pos;
            p.dx += 1;

            pos_offs.push(p);     
        }

        if(pos.dx < 0 && pos.dx > -3) {
            auto p = pos;
            p.dx -= 1;
            
            pos_offs.push(p);
        }

        if(pos.dy > 0 && pos.dy < 3) {
            auto p = pos;
            p.dy += 1;

            pos_offs.push(p);     
        }

        if(pos.dy < 0 && pos.dy > -3) {
            auto p = pos;
            p.dy -= 1;
            
            pos_offs.push(p);
        }

        if(pos.dx != 0 || pos.dy != 0) {
            auto p = pos;

            // Changing directions
            p.x = xx;
            p.y = yy;
            p.dx = 0;
            p.dy = 0;

            if(pos.dy != 0) {
                p.dx = -1;
                pos_offs.push(p);

                p.dx = 1;
                pos_offs.push(p);
            } else if(pos.dx != 0) {
                p.dy = -1;
                pos_offs.push(p);

                p.dy = 1;
                pos_offs.push(p);
            }
        }
    }

#if 0
    int cur_x = (w - 1);
    int cur_y = (h - 1);

    while(cur_x != 0 || cur_y != 0) {
        int max_diff = 0;
        int max_diff_x = 0;
        int max_diff_y = 0;

        for(int dy = -1; dy <= 1; ++dy) {
            for(int dx = -1; dx <= 1; ++dx) {
                if((dx == 0 && dy == 0) ||
                   (dx != 0 && dy != 0)) {
                    continue;
                }

                int nx = cur_x + dx;
                int ny = cur_y + dy;

                if(nx < 0 || nx >= w || ny < 0 || ny >= h) {
                    continue;
                }

                auto n_value = min_at_pos[nx + ny * w];
                auto cur_value = min_at_pos[cur_x + cur_y * w];

                int diff = cur_value - n_value;

                if(diff > max_diff) {
                    max_diff_x = nx;
                    max_diff_y = ny;
                    max_diff = diff;
                }
            }
        }

        std::cout << cur_x << ',' << cur_y << "->" << max_diff_x << ',' << max_diff_y << '\n';
        cur_x = max_diff_x;
        cur_y = max_diff_y;
    }
#endif

    for(int y = 0; y < h; ++y) {
        for(int x = 0; x < w; ++x) {
            auto& raw_min = min_at_pos_dir[x + y * w];

            auto nice = [](int n) {
                return n == INT_MAX ? 0 : n;
            };

            std::cout << nice(raw_min.hl_e) << 'e' << 
                         nice(raw_min.hl_w) << 'w' << 
                         nice(raw_min.hl_n) << 'n' << 
                         nice(raw_min.hl_s) << 's' << ',';
        }
        std::cout << '\n';
    }

    std::cout << get_min((w - 1), (h - 1)) << '\n';

    return 0;
}