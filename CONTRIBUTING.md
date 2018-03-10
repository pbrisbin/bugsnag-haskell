## Getting Started

```
make
```

## Pull Request Etiquette

1. Don't bump version or write a CHANGELOG
1. Consider your commits and messages as part of the PR
1. When in doubt, open an Issue first
1. Please comment if I'm not responsive

## Coding Conventions

1. Abide by surrounding style
1. Exported functions SHOULD be documented
1. Modules SHOULD be documented
1. Always include tests
1. In addition to passing tests, code MUST pass `--pedantic`, Stylish Haskell,
   HLint, and Weeder
1. Favor doctests for testing user-visible behavior
1. If making a substantial change, locally modify relevant examples to use your
   own test API key and verify that everything works
