# Ditaa Pandoc Filter

This is a Pandoc filter to convert Ditaa code blocks to diagrams.

## Install

The recommended way is using `stack install`.

## Usage

```
$ pandoc input.md -t json --filter ditaa-filter -o output.md
```

* input.md

    ```` markdown
    # markdown

    ``` ditaa
    +------+      +------+
    | hoge +----->+ fuga |
    +------+      +------+
    ```
    ````

## Contribution

To check the filter, do as follows.

```
$ pandoc input.md -t json | stack exec ditaa-filter | pandoc -f json -o output.md
```

