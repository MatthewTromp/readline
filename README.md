# readline.el
This file adds completions extracted from bash to your shell command completions.
When you request completions for a shell command,
readline.el starts an instance of bash,
sends it the text of your command,
requests completions (using the `export-completions` bindable command)
and parses the resulting output.
This allows you to benefit from all the completions available to your shell,
just like if you pressed tab while running bash in a terminal emulator.

## Requirements
You must be running at least version 5.3.0(18)-beta of bash (not yet released).

## Usage
Add `readline-completion` to `shell-dynamic-complete-functions`, in between `shell-c-a-p-replace-by-expanded-directory` and `pcomplete-completions-at-point`.
```
(custom-set-variables 
    [...]
    '(shell-dynamic-complete-functions
      '(comint-c-a-p-replace-by-expanded-history 
        shell-environment-variable-completion 
        shell-command-completion 
        shell-c-a-p-replace-by-expanded-directory 
        readline-completion 
        pcomplete-completions-at-point
        shell-filename-completion
        comint-filename-completion))
```
If bash has no completions, Emacs will fall back to pcomplete.
