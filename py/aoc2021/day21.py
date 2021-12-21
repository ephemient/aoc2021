import functools


def part1(lines):
    """
    >>> part1(["Player 1 starting position: 4", "Player 2 starting position: 8"])
    739785
    """
    player1, player2 = int(lines[0][28:].strip()), int(lines[1][28:].strip())
    score1, score2, n = 0, 0, 0
    while score2 < 1000:
        player1, player2 = (
            player2,
            (player1 + n % 100 + (n + 1) % 100 + (n + 2) % 100 + 2) % 10 + 1,
        )
        score1, score2 = score2, score1 + player2
        n += 3
    return n * score1


@functools.lru_cache(maxsize=None)
def score(player1, player2, score1, score2):
    x, y = 0, 0
    for d, n in ((3, 1), (4, 3), (5, 6), (6, 7), (7, 6), (8, 3), (9, 1)):
        play = (player1 + d - 1) % 10 + 1
        if score1 + play < 21:
            x2, y2 = score(player2, play, score2, score1 + play)
            x += n * y2
            y += n * x2
        else:
            x += n
    return x, y


def part2(lines):
    """
    >>> part2(["Player 1 starting position: 4", "Player 2 starting position: 8"])
    444356092776315
    """
    x, y = score(int(lines[0][28:].strip()), int(lines[1][28:].strip()), 0, 0)
    return max(x, y)


parts = (part1, part2)
