# [Advent of Code 2021](https://adventofcode.com/2021)
### my answers in [Kotlin](https://www.kotlinlang.org/) ![Kotlin CI](https://github.com/ephemient/aoc2021/workflows/Kotlin%20CI/badge.svg)

This project builds with [Gradle](https://gradle.org/).

Run the [JUnit 5](https://junit.org/junit5/) test suite:

```sh
./gradlew jvmTest
```

Run [kotlinx.benchmark](https://github.com/Kotlin/kotlinx-benchmark) benchmarks:

```sh
./gradlew benchmark
```

Print solutions for the inputs provided in local data files:

```sh
./gradlew runJvm
```

Generate [Dokka](https://github.com/Kotlin/dokka) API documentation:

```sh
./gradlew dokkaHtml
```

Run all checks, including [Detekt](https://detekt.github.io/) static code analysis and [ktlint](https://ktlint.github.io/) formatter:

```sh
./gradlew check
```

Check for newer versions of dependencies:

```sh
./gradlew dependencyUpdates
```
