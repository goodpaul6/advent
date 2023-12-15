lines = open('inputs/day15.txt').readlines()
line = ",".join(lines)

def my_hash(s):
    cur_value = 0

    for ch in s:
        cur_value += ord(ch)
        cur_value *= 17
        cur_value %= 256
    
    return cur_value

sum = 0
for s in line.split(','):
    sum += my_hash(s)

print(sum)