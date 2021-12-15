import heapq
import math

SAMPLE_INPUT = [
    "1163751742",
    "1381373672",
    "2136511328",
    "3694931569",
    "7463417111",
    "1319128137",
    "1359912421",
    "3125421639",
    "1293138521",
    "2311944581",
]


def solve(risks):
    bests = [[math.inf] * len(row) for row in risks]
    bests[0][0] = 0
    queue = []
    heapq.heappush(queue, (0, 0, 0))
    while True:
        _, x0, y0 = heapq.heappop(queue)
        c = bests[y0][x0]
        if y0 == len(risks) - 1 and x0 == len(risks[y0]) - 1:
            return c
        for x1, y0 in ((x0 - 1, y0), (x0, y0 - 1), (x0, y0 + 1), (x0 + 1, y0)):
            if y0 not in range(len(risks)) or x1 not in range(len(risks[y0])):
                continue
            d = c + risks[y0][x1]
            if d < bests[y0][x1]:
                bests[y0][x1] = d
                heapq.heappush(queue, (d, x1, y0))


def part1(lines):
    """
    >>> part1(SAMPLE_INPUT)
    40
    """
    risks = [list(map(int, line.strip())) for line in lines]
    return solve(risks)


def part2(lines):
    """
    >>> part2(SAMPLE_INPUT)
    315
    """
    risks = [
        [(int(char) - 1 + dx + dy) % 9 + 1 for dx in range(5) for char in line.strip()]
        for dy in range(5)
        for line in lines
    ]
    return solve(risks)


parts = (part1, part2)
