from collections import Counter

SAMPLE_INPUT = [
    "NNCB",
    "",
    "CH -> B",
    "HH -> N",
    "CB -> H",
    "NH -> C",
    "HB -> C",
    "HC -> B",
    "HN -> C",
    "NN -> C",
    "BH -> H",
    "NC -> B",
    "NB -> B",
    "BN -> B",
    "BB -> N",
    "BC -> B",
    "CC -> N",
    "CN -> C",
]


def solve(lines, times):
    rules = {
        line[:2]: (line[0] + line[-1], line[-1] + line[1])
        for line in map(str.strip, lines[2:])
    }
    state = Counter(a + b for a, b in zip(lines[0], lines[0][1:].rstrip()))
    for _ in range(times):
        newstate = Counter()
        for src, n in state.items():
            for dst in rules[src]:
                newstate[dst] += n
        state = newstate
    counts = Counter(lines[0][0] + lines[0].rstrip()[-1])
    for pair, n in state.items():
        for c in pair:
            counts[c] += n
    return (max(counts.values()) - min(counts.values())) // 2


def part1(lines):
    """
    >>> part1(SAMPLE_INPUT)
    1588
    """
    return solve(lines, 10)


def part2(lines):
    """
    >>> part2(SAMPLE_INPUT)
    2188189693529
    """
    return solve(lines, 40)


parts = (part1, part2)
