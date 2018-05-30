# Pandoc Ditaa Filter

A Pandoc filter to convert Ditaa code blocks to images.

## Install

The recommended way is using `stack install`.

## Usage

```
$ pandoc input.md --filter pandoc-ditaa-filter -o output.md
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

To pass filter options, do as follows.

```
$ pandoc input.md -t json | pandoc-ditaa-filter --some-option | pandoc -f json -o output.md
```

## Contribution

To check the filter, do as follows.

```
$ pandoc input.md -t json | stack exec pandoc-ditaa-filter | pandoc -f json -o output.md
```

