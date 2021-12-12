from collections import defaultdict

SAMPLE_INPUT_1 = [
    "start-A",
    "start-b",
    "A-c",
    "A-b",
    "b-d",
    "A-end",
    "b-end",
]
SAMPLE_INPUT_2 = [
    "dc-end",
    "HN-start",
    "start-kj",
    "dc-start",
    "dc-HN",
    "LN-dc",
    "HN-end",
    "kj-sa",
    "kj-HN",
    "kj-dc",
]
SAMPLE_INPUT_3 = [
    "fs-end",
    "he-DX",
    "fs-he",
    "start-DX",
    "pj-DX",
    "end-zg",
    "zg-sl",
    "zg-pj",
    "pj-he",
    "RW-he",
    "fs-DX",
    "pj-RW",
    "zg-RW",
    "start-pj",
    "he-WI",
    "zg-he",
    "pj-fs",
    "start-RW",
]


def solve(lines, bonus):
    edges = defaultdict(set)
    for line in lines:
        lhs, rhs = line.strip().split("-", maxsplit=2)
        edges[lhs].add(rhs)
        edges[rhs].add(lhs)

    def walk(current, path, bonus):
        if current == "end":
            yield path
        else:
            for next in edges[current]:
                if next == "start":
                    pass
                elif next.isupper() or next not in path:
                    yield from walk(next, path + [next], bonus)
                elif bonus and next.islower():
                    yield from walk(next, path + [next], False)

    return sum(1 for _ in walk("start", ["start"], bonus))


def part1(lines):
    """
    >>> part1(SAMPLE_INPUT_1)
    10
    >>> part1(SAMPLE_INPUT_2)
    19
    >>> part1(SAMPLE_INPUT_3)
    226
    """
    return solve(lines, False)


def part2(lines):
    """
    >>> part2(SAMPLE_INPUT_1)
    36
    >>> part2(SAMPLE_INPUT_2)
    103
    >>> part2(SAMPLE_INPUT_3)
    3509
    """
    return solve(lines, True)


parts = (part1, part2)
