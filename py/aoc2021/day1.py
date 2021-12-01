import collections
import itertools


def part1(lines):
    """
    >>> part1(["199", "200", "208", "210", "200", "207", "240", "269", "260", "263"])
    7
    """
    return sum(x < y for x, y in itertools.pairwise(map(int, lines)))


def sliding_window(iterable, n):
    # sliding_window('ABCDEFG', 4) -> ABCD BCDE CDEF DEFG
    it = iter(iterable)
    window = collections.deque(itertools.islice(it, n), maxlen=n)
    if len(window) == n:
        yield tuple(window)
    for x in it:
        window.append(x)
        yield tuple(window)


def part2(lines):
    """
    >>> part2(["199", "200", "208", "210", "200", "207", "240", "269", "260", "263"])
    5
    """
    return sum(
        x < y
        for x, y in itertools.pairwise(map(sum, sliding_window(map(int, lines), 3)))
    )


parts = (part1, part2)
