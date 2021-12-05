def part1(lines):
    """
    >>> part1(["0,9 -> 5,9", "8,0 -> 0,8", "9,4 -> 3,4", "2,2 -> 2,1", "7,0 -> 7,4", "6,4 -> 2,0", "0,9 -> 2,9", "3,4 -> 1,4", "0,0 -> 8,8", "5,5 -> 8,2"])
    5
    """
    points = {}
    for line in lines:
        start, end = line.split(" -> ", maxsplit=2)
        x0, y0 = map(int, start.split(",", maxsplit=2))
        x1, y1 = map(int, end.split(",", maxsplit=2))
        if x0 == x1:
            for y in range(min(y0, y1), max(y0, y1) + 1):
                points[(x0, y)] = points.get((x0, y), 0) + 1
        elif y0 == y1:
            for x in range(min(x0, x1), max(x0, x1) + 1):
                points[(x, y0)] = points.get((x, y0), 0) + 1
    return sum(count > 1 for count in points.values())


def part2(lines):
    """
    >>> part2(["0,9 -> 5,9", "8,0 -> 0,8", "9,4 -> 3,4", "2,2 -> 2,1", "7,0 -> 7,4", "6,4 -> 2,0", "0,9 -> 2,9", "3,4 -> 1,4", "0,0 -> 8,8", "5,5 -> 8,2"])
    12
    """
    points = {}
    for line in lines:
        start, end = line.split(" -> ", maxsplit=2)
        x0, y0 = map(int, start.split(",", maxsplit=2))
        x1, y1 = map(int, end.split(",", maxsplit=2))
        if x0 == x1:
            for y in range(min(y0, y1), max(y0, y1) + 1):
                points[(x0, y)] = points.get((x0, y), 0) + 1
        elif y0 == y1:
            for x in range(min(x0, x1), max(x0, x1) + 1):
                points[(x, y0)] = points.get((x, y0), 0) + 1
        elif abs(x1 - x0) == abs(y1 - y0):
            dx = 1 if x1 > x0 else -1
            dy = 1 if y1 > y0 else -1
            for i in range(abs(x1 - x0) + 1):
                xy = x0 + i * dx, y0 + i * dy
                points[xy] = points.get(xy, 0) + 1
    return sum(count > 1 for count in points.values())


parts = (part1, part2)
