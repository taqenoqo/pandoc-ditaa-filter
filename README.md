# Pandoc Ditaa Filter

A Pandoc filter to convert Ditaa code blocks to images.

## Install

The recommended way is using `stack install`.

## Usage

```
$ pandoc input.md --filter ditaa-filter -o output.md
```

* input.md

    ```` markdown
    # markdown

    ``` ditaa
    +------+      +------+
    | hoge |----->| fuga |
    +------+      +------+
    ```
    ````

## Contribution

To check the filter, do as follows.

```
$ pandoc input.md -t json | stack exec ditaa-filter | pandoc -f json -o output.md
```

