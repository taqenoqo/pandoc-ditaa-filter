# Ditaa Pandoc Filter

This is a Pandoc filter to convert Ditaa code blocks to diagrams.

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

To check the filter, do as follows.

```
$ pandoc input.md -t json | stack exec ditaa-filter | pandoc -f json -o output.md
```

