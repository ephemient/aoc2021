import heapq

SAMPLE_INPUT = [
    "#############",
    "#...........#",
    "###B#C#B#D###",
    "  #A#D#C#A#",
    "  #########",
]


def solve(lines):
    state = [[0] for _ in range(11)]
    for line in lines[2 : len(lines) - 1]:
        for i in range(2, 9, 2):
            state[i].append(ord(line[i + 1]) - ord("A") + 1)
    state = tuple(tuple(stack) for stack in state)
    visited, queue = {state: 0}, [(0, state)]
    while queue:
        _, state = heapq.heappop(queue)
        cost = visited[state]
        if all(all(a == b for b in state[2 * a][1:]) for a in range(1, 5)):
            return cost
        priority_moves, other_moves = [], []
        for i, src in enumerate(state):
            for x, a in enumerate(src):
                if a:
                    break
            else:
                continue
            if i == 2 * a and not any(b and b != a for b in src):
                continue
            for j, dst in enumerate(state):
                if i == j or any(
                    state[k][0] for k in (range(i + 1, j) if i < j else range(j + 1, i))
                ):
                    continue
                for y in range(len(dst) - 1, -1, -1):
                    if not dst[y]:
                        break
                else:
                    continue
                if j == 2 * a:
                    if all(not b or b == a for b in dst):
                        priority_moves.append((a, i, x, j, y))
                elif j not in range(2, 9, 2):
                    if i in range(2, 9, 2) and not dst[0]:
                        other_moves.append((a, i, x, j, y))
        for a, i, x, j, y in [priority_moves[0]] if priority_moves else other_moves:
            cost2 = cost + 10 ** (a - 1) * (abs(i - j) + x + y)
            state2 = [list(stack) for stack in state]
            state2[i][x] = 0
            state2[j][y] = a
            state2 = tuple(tuple(stack) for stack in state2)
            if state2 not in visited or visited[state2] > cost2:
                visited[state2] = cost2
                heapq.heappush(queue, (cost2, state2))


def part1(lines):
    """
    >>> part1(SAMPLE_INPUT)
    12521
    """
    return solve(lines)


def part2(lines):
    """
    >>> part2(SAMPLE_INPUT)
    44169
    """
    return solve(lines[0:3] + ["  #D#C#B#A#", "  #D#B#A#C#"] + lines[3:])


parts = (part1, part2)
