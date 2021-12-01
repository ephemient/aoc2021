# [Advent of Code 2021](https://adventofcode.com/2021)
### my answers

## Haskell

[Haddock](haddock/index.html)

[Criterion](aoc2021-bench.html)

## Kotlin

[Dokka](dokka/index.html)

{% for suite in site.data.benchmarks %}
<details>
  <summary>{{ suite[0] }}</summary>
  <pre></pre>
  <script>
    document.currentScript.previousElementSibling.innerText = JSON.stringify({{ suite[1] | jsonify }}, null, 4);
  </script>
</details>
{% endfor %}
