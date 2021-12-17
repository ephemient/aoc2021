import itertools
import math
import operator
import re
from collections import defaultdict

RE = re.compile(r"target area: x=(\d+)..(\d+), y=(-\d+)..(-\d+)")


def solve(lines):
    """
    >>> solve(["target area: x=20..30, y=-10..-5"])
    (45, 112)
    """
    match = RE.match(lines[0])
    x0 = int(match.group(1))
    x1 = int(match.group(2))
    y0 = int(match.group(3))
    y1 = int(match.group(4))
    max_t, dy_hits = 0, defaultdict(set)
    for dy in range(y0, -y0 + 1):
        for t, y in enumerate(
            itertools.accumulate(itertools.count(dy, -1), operator.add, initial=0)
        ):
            if y < y0:
                break
            if y0 <= y <= y1:
                max_t = max(max_t, t)
                dy_hits[t].add(dy)
    max_dy, count = 0, 0
    for dx in range(math.ceil(math.sqrt(2.0 * x0 + 0.25) - 0.5), x1 + 1):
        dys = set()
        for t, x in enumerate(
            itertools.accumulate(
                itertools.chain(range(dx, 0, -1), itertools.repeat(0)),
                operator.add,
                initial=0,
            )
        ):
            if t > max_t or x > x1:
                break
            if x0 <= x <= x1:
                dys.update(dy_hits[t])
        max_dy = max(max_dy, max(dys, default=0))
        count += len(dys)
    return max_dy * (max_dy + 1) // 2, count


parts = (solve,)
