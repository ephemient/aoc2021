# [Advent of Code 2021](https://adventofcode.com/2021)
### my answers in [Python](https://www.python.org/) ![Python CI](https://github.com/ephemient/aoc2021/workflows/Python%20CI/badge.svg)

This project builds with [Poetry](https://python-poetry.org/).

Setup:

```sh
curl -sSL https://install.python-poetry.org | python3 -
```

Run the test suite:

```sh
poetry run pytest
```

Run the benchmarks:

```sh
poetry run pytest --benchmark-enable
```

Print solutions for the inputs provided in local data files:

```sh
poetry run aoc2021
```

Lint and code with [Black](https://black.readthedocs.io/), [flake8](https://flake8.pycqa.org/), and [isort](https://pycqa.github.io/isort/):

```sh
poetry run black .
poetry run flake8 .
poetry run isort .
```
