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

## Usage Extensions
### Vim
You can use this software with [dense-analysis/ale](https://github.com/dense-analysis/ale) in Vim for on-the-fly linting.
Just use it together with the provided config file:  [misc/ale_linters/scm_checker.vim](https://github.com/niyarin/scm-checker/blob/main/misc/ale_linters/scm_checker.vim)
Copy it to your ALE linters directory or load it via your Vim config.
Make sure the compiled binary of this software is placed somewhere in your $PATH, so ALE can invoke it properly.

### Github Actions
By using [niyarin/scm-checker-action](https://github.com/niyarin/scm-checker-action), you can use this application with GitHub Actions.
