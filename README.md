# Ditaa Pandoc Filter

This can convert Ditaa code blocks in markdown to diagrams.

## Usage

* input.md

    ```` markdown
    # markdown

    ``` ditaa
    +------+      +------+
    | hoge +----->+ fuga |
    +------+      +------+
    ```
    ````

```
$ pandoc input.md -t json --filter ditaa-filter -o output.md
```

or

```
$ pandoc input.md -t json | stack exec ditaa-filter | pandoc -f json -o output.md
```

