import itertools
import math
import operator


def solve(lines):
    """
    >>> solve(["7,4,9,5,11,17,23,2,0,14,21,24,10,16,13,6,15,25,12,22,18,20,8,19,3,26,1", "", "22 13 17 11  0", " 8  2 23  4 24", "21  9 14 16  7", " 6 10  3 18  5", " 1 12 20 15 19", "", " 3 15  0  2 22", " 9 18 13 17  5", "19  8  7 25 23", "20 11 10 24  4", "14 21 16 12  6", "", "14 21 17 24  4", "10 16 15  9 19", "18  8 23 26 20", "22 11 13  6  5", " 2  0 12  3  7"])
    (4512, 1924)
    """
    it = iter(lines)
    draws = [int(draw) for draw in next(it).split(",")]
    draw_turns = {}
    for turn, draw in enumerate(draws):
        draw_turns.setdefault(draw, turn)
    boards = []
    for key, group in itertools.groupby(it, key=lambda line: bool(line.strip())):
        if not key:
            continue
        width = -1
        values = []
        for y, line in enumerate(group):
            row = [int(item) for item in line.split()]
            if width < 0:
                width = len(row)
            else:
                assert width == len(row)
            values.extend(row)
        assert values
        turns = [draw_turns.get(value, math.inf) for value in values]
        turn = min(
            min(
                max(turns[j] for j in range(i, i + width))
                for i in range(0, len(turns), width)
            ),
            min(
                max(turns[j] for j in range(i, len(turns), width))
                for i in range(0, width)
            ),
        )
        if turn < math.inf:
            boards.append(
                (
                    turn,
                    draws[turn]
                    * sum(
                        value
                        for value in values
                        if draw_turns.get(value, math.inf) > turn
                    ),
                )
            )
    return (
        min(boards, key=operator.itemgetter(0))[1],
        max(boards, key=operator.itemgetter(0))[1],
    )


parts = (solve,)
