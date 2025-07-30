# Scm-checker
(WIP) Code checker for Scheme.

## usage
```bash
# Run once.
git submodule update --init

gosh -I ./src -I ./src/scheme-reader/ src/main.scm test-resources/source1.scm

# OUTPUT
# test-resources/source1.scm:3:9:W:Duplicate import.
# test-resources/source1.scm:6:1:W:Use case.
```

## Native build
```bash
git submodule update --init
chicken-install r7rs
chicken-install srfi-113
make
```
