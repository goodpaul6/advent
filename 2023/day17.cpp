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

    uint16_t hl_so_far = 0;
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

    std::queue<PosOff> pos_offs;

    pos_offs.emplace();

    // Submissions:
    // 984 too high
    // 

    while(!pos_offs.empty()) {
        auto pos = pos_offs.front();   
        pos_offs.pop();

        int xx = pos.x + pos.dx;
        int yy = pos.y + pos.dy;

        if((pos.dx != 0 && pos.dy != 0) || std::abs(pos.dx) > 3 || std::abs(pos.dy) > 3 ||
            xx < 0 || xx >= blocks[0].size() || yy < 0 || yy >= blocks.size()) {
            continue;
        }

        // auto pos_s = pos.to_hashable_string();
  
        int c = blocks[yy][xx];
        
        if(xx != 0 || yy != 0) {
            pos.hl_so_far += c;
        }

        auto& prev_min = min_at_pos[xx + yy * w];
        
        if(prev_min == 0) {
            prev_min = pos.hl_so_far;
        } else if (pos.hl_so_far < prev_min) {
            prev_min = pos.hl_so_far;
        } else {
            // There is a cheaper way to get here, don't bother going down this path further
            continue;
        }

        // std::cout << xx << ',' << yy << ',' << static_cast<int>(pos.dx) << ',' << static_cast<int>(pos.dy) << ',' << static_cast<int>(pos.hl_so_far) << '\n';

        // Bottom right corner
        if(xx == blocks[0].size() - 1 && yy == blocks.size() - 1) {
            continue;
        }

        if(pos.dx >= 0 && pos.dx < 3) {
            auto p = pos;
            p.dx += 1;

            pos_offs.push(p);     
        }

        if(pos.dx <= 0 && pos.dx > -3) {
            auto p = pos;
            p.dx -= 1;
            
            pos_offs.push(p);
        }

        if(pos.dy >= 0 && pos.dy < 3) {
            auto p = pos;
            p.dy += 1;

            pos_offs.push(p);     
        }

        if(pos.dy <= 0 && pos.dy > -3) {
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

    for(int y = 0; y < h; ++y) {
        for(int x = 0; x < w; ++x) {
            std::cout << min_at_pos[x + y * w] << ',';
        }
        std::cout << '\n';
    }

    std::cout << min_at_pos[(w - 1) + (h - 1) * w] << '\n';

    return 0;
}