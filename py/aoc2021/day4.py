import itertools

EXAMPLE = (
    "7,4,9,5,11,17,23,2,0,14,21,24,10,16,13,6,15,25,12,22,18,20,8,19,3,26,1",
    "",
    "22 13 17 11  0",
    " 8  2 23  4 24",
    "21  9 14 16  7",
    " 6 10  3 18  5",
    " 1 12 20 15 19",
    "",
    " 3 15  0  2 22",
    " 9 18 13 17  5",
    "19  8  7 25 23",
    "20 11 10 24  4",
    "14 21 16 12  6",
    "",
    "14 21 17 24  4",
    "10 16 15  9 19",
    "18  8 23 26 20",
    "22 11 13  6  5",
    " 2  0 12  3  7",
)


class BingoBoard:
    def __init__(self, width, height, unmarked, marked):
        self.width = width
        self.height = height
        self.unmarked = unmarked
        self.marked = marked

    def play(self, draw):
        try:
            marks = self.unmarked.pop(draw)
        except KeyError:
            return False
        self.marked |= marks
        for y in range(self.height):
            mask = (1 << self.width) - 1 << y * self.width
            if self.marked & mask == mask:
                return True
        for x in range(self.width):
            mask = ((1 << self.width * self.height) - 1) // ((1 << self.width) - 1) << x
            if self.marked & mask == mask:
                return True
        return False

    def sum(self):
        return sum(key * value.bit_count() for key, value in self.unmarked.items())


def parse(lines):
    it = iter(lines)
    draws = [int(draw) for draw in next(it).split(",")]
    boards = []
    for key, group in itertools.groupby(it, key=lambda line: bool(line.strip())):
        if not key:
            continue
        width, height, unmarked = -1, 0, {}
        for y, line in enumerate(group):
            row = [int(item) for item in line.split()]
            assert row
            if height:
                assert width == len(row)
            else:
                width = len(row)
            height += 1
            for x, item in enumerate(row):
                unmarked[item] = unmarked.get(item, 0) | 1 << x + y * width
        boards.append(BingoBoard(width, height, unmarked, 0))
    return draws, boards


def part1(lines):
    """
    >>> part1(EXAMPLE)
    4512
    """
    draws, boards = parse(lines)
    for draw in draws:
        for board in boards:
            if board.play(draw):
                return draw * board.sum()


def part2(lines):
    """
    >>> part2(EXAMPLE)
    1924
    """
    draws, boards = parse(lines)
    for draw in draws:
        new_boards = []
        for i, board in enumerate(boards):
            if board.play(draw):
                score = draw * board.sum()
            else:
                new_boards.append(board)
        boards = new_boards
    return score


parts = (part1, part2)
