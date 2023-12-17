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

    std::unordered_map<int, int> min_at_pos;
    std::unordered_map<std::string, int> seen;

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

        {
            std::string pos_off_key;

            int values[] = { pos.x, pos.y, pos.dx, pos.dy };

            pos_off_key.resize(sizeof(values));
            std::memcpy(pos_off_key.data(), values, sizeof(values));

            auto& prev = seen[pos_off_key];

            // OMG IT WAS LITERALLY JUST LTE INSTEAD OF LT AHHHHHHHHHHHHHHHH
            if(prev != 0 && prev <= pos.hl_so_far) {
                // We've been in this exact pos and dx/dy before with a lower hl_so_far
                continue;
            }

            prev = pos.hl_so_far;
        }

        // auto pos_s = pos.to_hashable_string();
  
        int c = blocks[yy][xx];
        
        pos.hl_so_far += c;

        #if 0
        if(xx == 2 && yy == 0) {
            std::cout << xx << ',' << yy << ',' << static_cast<int>(pos.dx) << ',' << static_cast<int>(pos.dy) << ',' << static_cast<int>(pos.hl_so_far) << '\n';
        }
        #endif

        auto& prev_min = min_at_pos[xx + yy * w];
        
        if(prev_min == 0 || pos.hl_so_far < prev_min) {
            prev_min = pos.hl_so_far;
        }

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
            std::cout << min_at_pos[x + y * w] << ',';
        }
        std::cout << '\n';
    }

    std::cout << min_at_pos[(w - 1) + (h - 1) * w] << '\n';

    return 0;
}