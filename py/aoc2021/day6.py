def solve(times, lines):
    fishes = [0] * 9
    for line in lines:
        for fish in line.strip().split(","):
            fishes[int(fish)] += 1
    for _ in range(times):
        zero = fishes[0]
        fishes[0:8] = fishes[1:]
        fishes[6] += zero
        fishes[8] = zero
    return sum(fishes)


def part1(lines):
    """
    >>> part1(["3,4,3,1,2"])
    5934
    """
    return solve(80, lines)


def part2(lines):
    """
    >>> part2(["3,4,3,1,2"])
    26984457539
    """
    return solve(256, lines)


parts = (part1, part2)
