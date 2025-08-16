# 🛠️ Developer documentation

This is a documentation file for Findata Funnel’s developers.

## Dev environment setup

This project requires [Lefthook](https://github.com/evilmartians/lefthook) and
[Commitlint](https://github.com/conventional-changelog/commitlint).

### Install Git hook tools

1. Install lefthook:

   ```shell
   lefthook install
   ```

## Operations

### Updating GHC & dependencies

1. Update the Stackage repo used in `cabal.project`.
2. Update the base requirement in `package.yaml`.
3. Regenerate the Cabal manifest with `hpack`.
