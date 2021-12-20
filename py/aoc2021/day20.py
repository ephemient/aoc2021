import functools
import itertools

SAMPLE_INPUT = [
    "..#.#..#####.#.#.#.###.##.....###.##.#..###.####..#####..#....#..#..##..###..######.###...####..#..#####..##..#.#####...##.#.#..#.##..#.#......#.###.######.###.####...#.##.##..#..#..#####.....#.#....###..#.##......#.....#..#..#..##..#...##.######.####.####.#.#...#.......#..#.#.#...####.##.#......#..#...##.#.##..#...##.#.##..###.#......#.#.......#.#.#.####.###.##...#.....####.#..#..#.##.#....##..#.####....##...##..#...#......#.#.......#.......##..####..#...#.#.#...##..#.#..###..#####........#..####......#..#",
    "",
    "#..#.",
    "#....",
    "##..#",
    "..#..",
    "..###",
]


def images(lines):
    alg = lines[0].strip()
    image = [line.strip() for line in lines[2:]]
    fill = "."
    while True:
        yield image if fill == "." else None
        width = len(image[0])
        image = [
            "".join(
                alg[
                    functools.reduce(
                        lambda x, y: 2 * x + (ord(y) & 1),
                        (
                            line1[x - 1] if line1 and 0 <= x - 1 < width else fill,
                            line1[x] if line1 and 0 <= x < width else fill,
                            line1[x + 1] if line1 and 0 <= x + 1 < width else fill,
                            line2[x - 1] if line2 and 0 <= x - 1 < width else fill,
                            line2[x] if line2 and 0 <= x < width else fill,
                            line2[x + 1] if line2 and 0 <= x + 1 < width else fill,
                            line3[x - 1] if line3 and 0 <= x - 1 < width else fill,
                            line3[x] if line3 and 0 <= x < width else fill,
                            line3[x + 1] if line3 and 0 <= x + 1 < width else fill,
                        ),
                        0,
                    )
                ]
                for x in range(-1, width + 1)
            )
            for y in range(-1, len(image) + 1)
            for line1 in (image[y - 1] if 0 <= y - 1 < len(image) else None,)
            for line2 in (image[y] if 0 <= y < len(image) else None,)
            for line3 in (image[y + 1] if 0 <= y + 1 < len(image) else None,)
        ]
        fill = alg[-1] if ord(fill) & 1 else alg[0]


def part1(lines):
    """
    >>> part1(SAMPLE_INPUT)
    35
    """
    return sum(
        c == "#"
        for line in next(itertools.islice(images(lines), 2, None))
        for c in line
    )


def part2(lines):
    """
    >>> part2(SAMPLE_INPUT)
    3351
    """
    return sum(
        c == "#"
        for line in next(itertools.islice(images(lines), 50, None))
        for c in line
    )


parts = (part1, part2)
