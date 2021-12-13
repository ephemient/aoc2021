import functools

SAMPLE_INPUT = [
    "6,10",
    "0,14",
    "9,10",
    "0,3",
    "10,4",
    "4,11",
    "6,0",
    "6,12",
    "4,1",
    "0,13",
    "10,12",
    "3,4",
    "3,0",
    "8,4",
    "1,10",
    "2,14",
    "8,10",
    "9,0",
    "",
    "fold along y=7",
    "fold along x=5",
]


def foldX(foldX, x, y):
    return (foldX - abs(x - foldX), y)


def foldY(foldY, x, y):
    return (x, foldY - abs(y - foldY))


def parse(lines):
    it = iter(lines)
    points = set()
    for line in it:
        if not line or line.isspace():
            break
        x, y = line.split(",", maxsplit=2)
        points.add((int(x), int(y)))
    folds = []
    for line in it:
        if line.startswith("fold along x="):
            folds.append(functools.partial(foldX, int(line[13:].rstrip())))
        elif line.startswith("fold along y="):
            folds.append(functools.partial(foldY, int(line[13:].rstrip())))
        else:
            raise ValueError(f"bad input: {line}")
    return points, folds


def part1(lines):
    """
    >>> part1(SAMPLE_INPUT)
    17
    """
    points, folds = parse(lines)
    fold, *_ = folds
    points = set(fold(x, y) for x, y in points)
    return len(points)


def part2(lines):
    """
    >>> part2(SAMPLE_INPUT)
    '▓▓▓▓▓\\n▓░░░▓\\n▓░░░▓\\n▓░░░▓\\n▓▓▓▓▓'
    """
    points, folds = parse(lines)

    def fold(point, fold):
        x, y = point
        return fold(x, y)

    points = set(functools.reduce(fold, folds, point) for point in points)
    x0 = min(x for x, _ in points)
    y0 = min(y for _, y in points)
    x1 = max(x for x, _ in points)
    y1 = max(y for _, y in points)
    return "\n".join(
        "".join("\u2593" if (x, y) in points else "\u2591" for x in range(x0, x1 + 1))
        for y in range(y0, y1 + 1)
    )


parts = (part1, part2)
