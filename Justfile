build:
  cabal build

test:
  cabal test

run *ARGS:
  @cabal run findata-funnel -- {{ARGS}}
