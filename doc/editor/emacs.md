# Using scm-checker with Emacs

## Flycheck Integration

You can use Flycheck to check Scheme code in Emacs with scm-checker.

Add the following configuration to your `.emacs` or `init.el`:

```elisp
(flycheck-define-checker scm-checker
  "Custom linter for output: PATH:LINE:COL:E/W: message"
  :command ("scm-checker" source)
  :error-patterns
  ((error   line-start (file-name) ":" line ":" column ":E:" (message) line-end)
   (warning line-start (file-name) ":" line ":" column ":W:" (message) line-end))
  :modes (scheme-mode))

;; Enable flycheck-mode for scheme-mode
(add-hook 'scheme-mode-hook #'flycheck-mode)

;; Automatically select scm-checker for scheme files
(add-hook 'scheme-mode-hook
          (lambda ()
            (flycheck-select-checker 'scm-checker)))
```

This configuration enables automatic Scheme code checking in scheme-mode, displaying errors and warnings in real-time.

### Prerequisites

- Flycheck must be installed
- `scm-checker` command must be available in your PATH
