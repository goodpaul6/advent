lines = open('inputs/day15.txt').readlines()
line = ",".join(lines)

def my_hash(s):
    cur_value = 0

    for ch in s:
        cur_value += ord(ch)
        cur_value *= 17
        cur_value %= 256
    
    return cur_value

steps = []

for s in line.split(','):
    if '=' in s:
        label, dest = s.split('=')
        steps.append(('=', label, dest))
    elif '-' in s:
        steps.append(('-', s[:-1]))    

boxes = [[] for _ in range(256)]

for step in steps:
    op, label = step[0], step[1]
    box_idx = my_hash(label)
    box = boxes[box_idx]

    if op == '-':
        # We will reverse it back so it
        # looks like everything shifted
        # forward
        # box.reverse()

        lens_idx = next(
            (i for i in range(len(box))
            if box[i][0] == label),
            None
        )

        if lens_idx == None:
            #box.reverse()
            continue 

        del box[lens_idx]

        #box.reverse()
    elif op == '=':
        focal_len = int(step[2])

        lens_idx = next(
            (i for i in range(len(box))
            if box[i][0] == label),
            None
        )

        if lens_idx is not None:
            box[lens_idx][1] = focal_len
            continue

        box.append([label, focal_len])

print([box for box in boxes if box])

def focus_power(box_idx, lens_idx):
    lens = boxes[box_idx][lens_idx]

    return ((box_idx + 1) *
            (lens_idx + 1) *
            lens[1])

total = 0
for box_idx, box in enumerate(boxes):
    for lens_idx in range(len(box)):
        total += focus_power(box_idx, lens_idx)
    
print(total)
