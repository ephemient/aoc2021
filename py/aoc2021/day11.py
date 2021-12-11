def solve(lines):
    """
    >>> solve(["5483143223", "2745854711", "5264556173", "6141336146", "6357385478", "4167524645", "2176841721", "6882881134", "4846848554", "5283751526"])
    (1656, 195)
    """
    levels = [list(map(int, line.strip())) for line in lines]
    flashes = []
    while any(n for row in levels for n in row):
        for row in levels:
            for i in range(len(row)):
                row[i] += 1
        count = 0
        while any(n > 9 for row in levels for n in row):
            for y0, row in enumerate(levels):
                for x0, level in enumerate(row):
                    if level <= 9:
                        continue
                    count += 1
                    for y1 in range(y0 - 1, y0 + 2):
                        if not 0 <= y1 < len(levels):
                            continue
                        for x1 in range(x0 - 1, x0 + 2):
                            if not 0 <= x1 < len(levels[y1]):
                                continue
                            if x0 == x1 and y0 == y1:
                                levels[y1][x1] = -1
                            elif levels[y1][x1] >= 0:
                                levels[y1][x1] += 1
        flashes.append(count)
        for row in levels:
            for i in range(len(row)):
                if row[i] < 0:
                    row[i] = 0
    return (sum(flashes[:100]), len(flashes))


parts = (solve,)
