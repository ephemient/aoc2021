import math


def part1(lines):
    """
    >>> part1(["16,1,2,0,4,2,7,1,2,14"])
    37
    """
    nums = sorted(int(word) for line in lines for word in line.strip().split(","))
    median = nums[len(nums) // 2]
    return sum(abs(num - median) for num in nums)


def t(n):
    return n * (n + 1) // 2


def part2(lines):
    """
    >>> part2(["16,1,2,0,4,2,7,1,2,14"])
    168
    """
    nums = list(int(word) for line in lines for word in line.strip().split(","))
    mean = sum(nums) / len(nums)
    mean0 = math.floor(mean)
    mean1 = math.ceil(mean)
    return min(
        sum(t(abs(num - mean0)) for num in nums),
        sum(t(abs(num - mean1)) for num in nums),
    )


parts = (part1, part2)
