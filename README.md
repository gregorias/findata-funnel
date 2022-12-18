# findata-funnel

This app moves fetches, moves, and executes processing of financial data files
(e.g. receipts, statements). The app's overarching purpose is to move desired
financial data into my ledger.

## Installation

Install the binary with

```shell
stack install
```

### \[Optional\] Shell completion

To provide [fish][fish] shell completion run

```sh
findata-funnel --fish-completion-script (which findata-funnel) > ~/.config/fish/completions/findata-funnel.fish
```

For other shells, check out [optparse-applicative's documentation](https://hackage.haskell.org/package/optparse-applicative#:~:text=revoir%20%20%20%20%20%20%20%20%20%20%20%20%20%20%20%20Say%20goodbye-,Bash%2C%20Zsh%2C%20and%20Fish%20Completions,-optparse%2Dapplicative%20has) and adapt accordingly. findata-funnel uses [optparse-applicative][optparse-applicative] as the CLI framework.

## Development

This section is intended for developrs.

### Dev environment setup

This project requires [Lefthook](https://github.com/evilmartians/lefthook) and
[Commitlint](https://github.com/conventional-changelog/commitlint).

#### Install Git hook tools

1. Install lefthook:

   ```shell
   lefthook install
   ```
