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
    int x = 0;
    int y = 0;
    
    // Distance moved in each dir from x and y
    int dx = 0;
    int dy = 0;

    std::string to_hashable_string() const {
        std::string s;

        int values[] = {x, y, dx, dy};

        s.resize(sizeof(values));
        std::memcpy(s.data(), values, sizeof(values));

        return s;
    }
}; 

int lowest_heat_loss(
    const std::vector<std::vector<int>>& blocks, 
    std::unordered_map<std::string, int>& seen,
    PosOff pos) {
    int xx = pos.x + pos.dx;
    int yy = pos.y + pos.dy;

    if((pos.dx != 0 && pos.dy != 0) || std::abs(pos.dx) > 3 || std::abs(pos.dy) > 3 ||
       xx < 0 || xx >= blocks[0].size() || yy < 0 || yy >= blocks.size()) {
        return INT_MAX;
    }

    std::cout << pos.x << ',' << pos.y << ',' << pos.dx << ',' << pos.dy << '\n';

    auto pos_s = pos.to_hashable_string();
    auto found = seen.find(pos_s);

    if(found != seen.end()) {
        return found->second;
    }

    int c = blocks[yy][xx];

    // Bottom right corner
    if(xx == blocks[0].size() - 1 && yy == blocks.size() - 1) {
        return c;
    }
    
    int m = INT_MAX;

    for(int i = 1; i <= 3; ++i) {
        if(pos.dx >= 0) {
            PosOff p = pos;
            p.dx = pos.dx + i;
            
            m = std::min(m, lowest_heat_loss(blocks, seen, p));
        }

        if(pos.dx <= 0) {
            PosOff p = pos;
            p.dx = pos.dx - i;
            
            m = std::min(m, lowest_heat_loss(blocks, seen, p));
        }

        if(pos.dx != 0) {
            PosOff p;

            // Changing directions
            p.x = xx;
            p.y = yy;
            p.dx = 0;
            p.dy = i;

            m = std::min(m, lowest_heat_loss(blocks, seen, p));

            p.dy = -i;

            m = std::min(m, lowest_heat_loss(blocks, seen, p));
        } else {
            if(pos.dy >= 0) {
                PosOff p = pos;
                p.dy = pos.dy + i;
                
                m = std::min(m, lowest_heat_loss(blocks, seen, p));
            }

            if(pos.dy <= 0) {
                PosOff p = pos;
                p.dy = pos.dy - i;
                
                m = std::min(m, lowest_heat_loss(blocks, seen, p));
            }

            if(pos.dy != 0) {
                PosOff p = pos;

                // Changing directions
                p.x = xx;
                p.y = yy;
                p.dx = i;
                p.dy = 0;

                m = std::min(m, lowest_heat_loss(blocks, seen, p));

                p.dx = -i;

                m = std::min(m, lowest_heat_loss(blocks, seen, p));
            }
        }
    }

    int res = m + c;
    seen[pos_s] = res;
    
    return res;
}

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

    struct PosOffHL : PosOff {
        int hl_so_far = 0;
    };

    std::queue<PosOffHL> pos_offs;

    pos_offs.emplace();

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

        // std::cout << xx << ',' << yy << ',' << pos.dx << ',' << pos.dy << ',' << pos.hl_so_far << '\n';

        // Bottom right corner
        if(xx == blocks[0].size() - 1 && yy == blocks.size() - 1) {
            continue;
        }

        for(int i = 1; i <= 1; ++i) {
            if(pos.dx >= 0) {
                auto p = pos;
                p.dx = pos.dx + i;

                pos_offs.push(p);     
            }

            if(pos.dx <= 0) {
                auto p = pos;
                p.dx = pos.dx - i;
                
                pos_offs.push(p);
            }

            if(pos.dx != 0) {
                auto p = pos;

                // Changing directions
                p.x = xx;
                p.y = yy;
                p.dx = 0;
                p.dy = i;

                pos_offs.push(p);

                p.dy = -i;

                pos_offs.push(p);
            } else {
                if(pos.dy >= 0) {
                    auto p = pos;
                    p.dy = pos.dy + i;
                    
                    pos_offs.push(p);
                }

                if(pos.dy <= 0) {
                    auto p = pos;
                    p.dy = pos.dy - i;
                    
                    pos_offs.push(p);
                }

                if(pos.dy != 0) {
                    auto p = pos;

                    // Changing directions
                    p.x = xx;
                    p.y = yy;
                    p.dx = i;
                    p.dy = 0;

                    pos_offs.push(p);

                    p.dx = -i;

                    pos_offs.push(p);
                }
            }
        }
    }

    std::cout << min_at_pos[(w - 1) + (h - 1) * w] << '\n';

    return 0;
}