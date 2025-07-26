# Scm-checker
Code checker for Scheme.

## usage
```bash
gosh -I ./src -I ./src/scheme-reader/ src/main.scm test-resources/source1.scm

# OUTPUT
# test-resources/source1.scm:3:9:W:Duplicate import.
# test-resources/source1.scm:6:1:W:Use case.
```

## Native build
```bash
chicken-install r7rs
make
```
