# json-ts-mode

[![CI](https://github.com/konomanoasa/json-ts-mode/actions/workflows/ci.yaml/badge.svg)](https://github.com/konomanoasa/json-ts-mode/actions/workflows/ci.yaml)

[Tree-sitter](https://tree-sitter.github.io/tree-sitter/)-based
[Emacs](https://www.gnu.org/software/emacs/) major mode for
JavaScript Object Notation and JSON with comments.

## Requirement

Emacs 31.1 or later.

## Installation

```elisp
(package-vc-install "https://github.com/konomanoasa/json-ts-mode")
```

## Automatic Activation

Enabled for `.json` and `.jsonc` files.

## Features

- Comment Commands (JSONC)
- Electric Pair
- Font Lock
- Imenu
- Indentation
- Navigation
- Syntax Table

## Font Lock

Supports `treesit-font-lock-level`.

| Level | Font Lock                             |
| ----- | ------------------------------------- |
| 1     | Comments (JSONC), booleans, and null  |
| 2     | Property names and strings            |
| 3     | Numbers and escapes                   |
| 4     | Delimiters, punctuation, and brackets |

## JSON Dialect

Select the dialect for matching files in `.editorconfig`.

```ini
[config.json]
json_dialect = jsonc_trailing
```

| Dialect          | Comments | Trailing commas |
| ---------------- | -------- | --------------- |
| `json`           | No       | No              |
| `jsonc`          | Yes      | No              |
| `jsonc_trailing` | Yes      | Yes             |

Without `json_dialect`, `tsconfig.json` and `jsconfig.json` use
`jsonc_trailing`, `.jsonc` files use `jsonc`, and other files or unnamed
buffers use `json`.

## Grammar

[tree-sitter-json](https://github.com/konomanoasa/tree-sitter-json)

## License

[MIT](LICENSE)
