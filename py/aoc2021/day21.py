from collections import Counter


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


class Score:
    def __init__(self):
        self.data = [None] * 44100
        self.dice = Counter(
            i + j + k for i in range(1, 4) for j in range(1, 4) for k in range(1, 4)
        )

    def __call__(self, player1, player2, score1, score2):
        i = player1 + 10 * player2 + 100 * score1 + 2100 * score2 - 11
        if self.data[i]:
            x, y = self.data[i]
        else:
            x, y = 0, 0
            for d, n in self.dice.items():
                play = (player1 + d - 1) % 10 + 1
                if score1 + play < 21:
                    x2, y2 = self(player2, play, score2, score1 + play)
                    x += n * y2
                    y += n * x2
                else:
                    x += n
            self.data[i] = x, y
        return x, y


Score = Score()


def part2(lines):
    """
    >>> part2(["Player 1 starting position: 4", "Player 2 starting position: 8"])
    444356092776315
    """
    x, y = Score(int(lines[0][28:].strip()), int(lines[1][28:].strip()), 0, 0)
    return max(x, y)


parts = (part1, part2)
