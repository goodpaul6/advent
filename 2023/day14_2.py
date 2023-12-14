lines = open('inputs/day14.txt').readlines()
chars = [[ch for ch in line if ch != '\n'] for line in lines]

cycle_dirs = ('n', 'w', 's', 'e')

w = len(chars[0])
h = len(chars)

for i in range(3):
    for dir_ in cycle_dirs:
        if dir_ == 'n':
            for col in range(w):
                last_empty_row = 0
                for row in range(h):
                    ch = chars[row][col]

                    if ch == '#':
                        last_empty_row = row + 1
                        continue

                    if ch == '.':
                        continue

                    # Move it to the last empty row, and increment the lowest row
                    chars[row][col] = '.'
                    chars[last_empty_row][col] = ch
                    last_empty_row += 1
        elif dir_ == 's':
            for col in range(w):
                last_empty_row = h - 1
                for row in reversed(range(h)):
                    ch = chars[row][col]

                    if ch == '#':
                        last_empty_row = row - 1
                        continue

                    if ch == '.':
                        continue

                    chars[row][col] = '.'
                    chars[last_empty_row][col] = ch
                    last_empty_row -= 1

        elif dir_ == 'w':
            for row in range(h):
                last_empty_col = 0
                for col in range(w):
                    ch = chars[row][col]

                    if ch == '#':
                        last_empty_col = col + 1
                        continue
                    
                    if ch == '.':
                        continue
                    
                    chars[row][col] = '.'
                    chars[row][last_empty_col] = ch
                    last_empty_col += 1

        elif dir_ == 'e':
            for row in range(h):
                last_empty_col = w - 1
                for col in reversed(range(w)):
                    ch = chars[row][col]

                    if ch == '#':
                        last_empty_col = col - 1
                        continue
                        
                    if ch == '.':
                        continue

                    chars[row][col] = '.'
                    chars[row][last_empty_col] = ch
                    last_empty_col -= 1

        #print(dir_)
        #print("\n".join(["".join(line) for line in chars]))
        #print()

total_load = 0

for row, line in enumerate(chars):
    for ch in line:
        if ch == 'O':
            total_load += (len(chars) - row)

print("\n".join(["".join(line) for line in chars]))
print(total_load)

