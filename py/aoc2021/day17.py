import re

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
    max_dy, count = 0, 0
    for dx in range(x1 + 1):
        for dy in range(min(y0, 0), max(abs(y0), abs(y1)) + 1):
            dx_, dy_ = dx, dy
            x, y = 0, 0
            while x <= x1 and (dy_ > 0 or y >= y0):
                if x in range(x0, x1 + 1) and y in range(y0, y1 + 1):
                    max_dy = max(max_dy, dy)
                    count += 1
                    break
                x, y = x + dx_, y + dy_
                if dx_:
                    dx_ -= 1
                dy_ -= 1
    return max_dy * (max_dy + 1) // 2, count


parts = (solve,)
