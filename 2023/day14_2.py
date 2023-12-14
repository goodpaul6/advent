lines = open('inputs/day14.txt').readlines()
chars = [[ch for ch in line if ch != '\n'] for line in lines]

cycle_dirs = ('n',) # , 'w', 's', 'e')

w = len(chars[0])
h = len(chars)

for i in range(1):
    for dir_ in cycle_dirs:
        if dir_ == 'n':
            # Keep track of the lowest row available for a given row/column
            lowest_row = [[1000 for _ in range(w)] for _ in range(h)]

            for col in range(w):
                last_empty_row = 0
                for row in range(h):
                    if chars[row][col] == '#':
                        last_empty_row = row

                    #print(f'{last_empty_row=} {row=} {col=}')
                    lowest_row[row][col] = last_empty_row

            for col in range(w):
                last_empty_row = 0
                for row in range(h):
                    ch = chars[row][col]

                    if ch == '#':
                        last_empty_row = row

                    if ch == '.':
                        continue

                    # Move it to the last empty row, and increment the lowest row
                    chars[row][col] = '.'
                    chars[last_empty_row][col] = ch
                    last_empty_row += 1

                    #lrow = lowest_row[row][col]
                    #print(f'{row=} {col=} {lrow=}')

                    #chars[lrow][col] = ch

                    #if row < h - 1:
                    #    lowest_row[row + 1][col] = max(lrow + 1, lowest_row[row + 1][col])

                    #cur_row = row
                    #while chars[cur_row][col] == 'O' and cur_row > 0 and chars[cur_row-1][col] == '.':
                    #    chars[cur_row-1][col] = 'O'
                    #    chars[cur_row][col] = '.'
                    #    cur_row -= 1

        elif dir_ == 's':
            for row in reversed(range(len(chars))):
                for col, ch in enumerate(line):
                    cur_row = row
                    while chars[cur_row][col] == 'O' and cur_row < len(chars) - 1 and chars[cur_row+1][col] == '.':
                        chars[cur_row+1][col] = 'O'
                        chars[cur_row][col] = '.'
                        cur_row += 1
        elif dir_ == 'w':
            for row, line in enumerate(chars):
                for col, ch in enumerate(line):
                    cur_col = col
                    # TODO(Apaar): Optimize by recording what the furthest spot to the left is for any given column
                    # Just two 2d arrays that contain the furthest spot to the left for a given column, precomputed in
                    # O(n^2).
                    while chars[row][cur_col] == 'O' and cur_col > 0 and chars[row][cur_col - 1] == '.':
                        chars[row][cur_col-1] = 'O'
                        chars[row][cur_col] = '.'
                        cur_col -= 1
        elif dir_ == 'e':
            for row, line in enumerate(chars):
                for col in reversed(range(len(line))):
                    cur_col = col
                    # TODO(Apaar): Optimize by recording what the furthest spot to the left is for any given column
                    # Just two 2d arrays that contain the furthest spot to the left for a given column, precomputed in
                    # O(n^2).
                    while chars[row][cur_col] == 'O' and cur_col < len(line) - 1 and chars[row][cur_col + 1] == '.':
                        chars[row][cur_col+1] = 'O'
                        chars[row][cur_col] = '.'
                        cur_col += 1

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

