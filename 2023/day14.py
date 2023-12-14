lines = open('inputs/day14.txt').readlines()
chars = [[ch for ch in line if ch != '\n'] for line in lines]

cycle = ('n', 'w', 's', 'e')


for row, line in enumerate(chars):
    for col, ch in enumerate(line):
        cur_row = row
        while chars[cur_row][col] == 'O' and cur_row > 0 and chars[cur_row-1][col] == '.':
            chars[cur_row-1][col] = 'O'
            chars[cur_row][col] = '.'
            cur_row -= 1

total_load = 0

for row, line in enumerate(chars):
    for col, ch in enumerate(line):
        if ch == 'O':
            total_load += (len(chars) - row)

print("\n".join(["".join(line) for line in chars]))
print(total_load)

