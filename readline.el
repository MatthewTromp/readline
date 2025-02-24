;;; readline.el --- gnu readline completions export support -*- lexical-binding: t -*-

;; Copyright 2024 Matthew Tromp.

;; Author: Matthew Tromp <matthewktromp@gmail.com>
;; Maintainer: matthewktromp@gmail.com

;;; Commentary:

;; This file defines functions for getting completions data from bash
;; using the readline `export-completions' functionality.

;; When readline completions are first requested for a buffer
;; (currently this is only enabled in minibuffers, like when running
;; `async-shell-command'), readline.el creates a new "readline buffer"
;; and associates a new bash instance with that buffer.  All
;; completions requests for the original buffer are sent to this bash
;; instance.  When the original buffer exits, the readline buffer and
;; bash process are killed.

;; The readline buffer and bash process are also killed
;; `readline-cleanup-timeout' seconds after the last readline
;; completions request for that buffer, to avoid filling the machine
;; with idle bash processes in the event that a user opens many
;; buffers and requests completions in each of them.  Currently this
;; is not an issue but could become one when e.g. readline.el is used
;; to provide completions in shell scripts.

;; You can enable readline completions by setting
;; `readline-use-minibuffer-completion' to t.

;; In the future, this could be used for completion in:
;; - `eshell'
;; - `shell-script-mode'
;; - comint buffers running bash
;; - comint buffers with non-bash readline programs (e.g. python-shell)

;; Todo:
;; - Remote support
;; - Don't suggest completion when there's only 1 and it's identical
;;   to the input?
;; - Why is quit inhibited sometimes ("Blocking call to
;;   accept-process-output with quit inhibited!!") and what should
;;   be done about that?
;; - Check bash version to ensure compatibility?

;;; Code:

(require 'cl-macs)

(defcustom readline-completions-timeout 0.5
  "Timeout for requesting readline completions.
If bash takes longer than this to return completions, readline
completion aborts."
  :type '(choice (number) (const nil))
  :group 'shell
  :version "30.1")

(setopt readline-completions-timeout nil)

(defcustom readline-use-minibuffer-completion nil
  "Whether to use readline completions in minibuffers."
  :type 'boolean
  :group 'shell
  :version "30.1")
;; (setopt readline-use-minibuffer-completion nil)
(setopt readline-use-minibuffer-completion t)

;; Local variable allows for disabling readline for specific buffers
;; e.g. if a particular buffer is using a shell other than bash
(defvar-local readline-completions-enabled 'unset)

;; The character sent to readline to export completions.
(defconst readline-export-completions-char "")
;; The inputrc name of 'shell-readline-export-completions-char.
(defconst readline-export-completions-char-name "C-^")

(defvar-local readline-buffer nil)
(defvar-local readline-outstanding-requests 0)

(defun readline--getline (proc &optional terminator)
  "Return the next line of data from the given process's output.

PROC must be configured to write its output to the current
buffer.

A line is everything from point to the first occurence of
TERMINATOR (defaults to `\n').  This function sets point to the
first character after TERMINATOR, so it can be used repeatedly."
  (let ((start-point (point))
        (terminator (or terminator "\n")))
    (while (not (search-forward terminator nil t))
      (when (not (accept-process-output proc readline-completions-timeout))
          (error "Timeout expired.  Are you sure you're using the right version of bash?")))
    (buffer-substring start-point (- (point) 1))))

;; XX technically a completion table is something passable to
;; `all-completions' and other low-level completion functions, see
;; (info "(elisp) Programmed Completion")
;;
;; this returns something obeying the `completion-at-point-functions'
;; interface (idk what to call that, "capf list"?)
(defun readline--completions-from-buffer (proc)
  "Extract readline export formatted completions from the given process.

Process output is written to the current buffer.  Completions are
returned as a capf list (the `completion-at-point-functions'
interface).

After parsing, point is left at the start of the first line after
the final completion.  Point is only moved if parsing was
completed.

PROC is the bash process providing completions."
  ;; Readline exported completions are formatted as:
  ;; newline
  ;; N - The number of matches, INCLUDING the longest common prefix at
  ;;     the start
  ;; T - The word being completed
  ;; S:E - The start and end offsets of T in the line buffer
  ;; Then one match per line
  (let ((new-point 0)
        completions
        num-completions
        word-start
        word-end)
    
    ;; All readline--getline calls need to be in save-excursion since they modify point
    (save-excursion
      ;; Skip initial newline
      (readline--getline proc)
      
      ;; Read header information
      (setq num-completions (string-to-number (readline--getline proc))) ; N
      (progn (readline--getline proc))                                           ; T (target word), unused
      (setq word-start (string-to-number (readline--getline proc ":")))  ; S
      (setq word-end (string-to-number (readline--getline proc)))        ; E
      
      ;; Collect completions
      (setq completions (readline--collect-completions proc num-completions))
      
      ;; Remember where to move point after we're done
      (setq new-point (point)))
    
    ;; Update state with inhibit-quit
    (let ((inhibit-quit t))
      (goto-char new-point)
      (setq readline-outstanding-requests (- readline-outstanding-requests 1)))
    
    ;; Return formatted result
    (if (< num-completions 1)
        nil  ; No completions
      (list word-start
            word-end
            (if (< num-completions 2)
                completions  ; Single completion - return full list
              (cdr completions))))))  ; Multiple completions - skip common prefix

(defun readline--collect-completions (proc num-completions)
  "Collect completion strings from process output.

PROC is the bash process providing completions.
NUM-COMPLETIONS is the number of completions to fetch."
  (let ((so-far 0)
        (last-point (point))
        (completions nil))
    
    (while (< so-far num-completions)
      ;; Try to find completions in current buffer content
      (while (and (< so-far num-completions)
                  (search-forward "\n" nil t))
        (let ((comp (buffer-substring last-point (1- (point)))))
          (push comp completions)
          (setq so-far (1+ so-far))
          (setq last-point (point))))
      
      ;; If we need more completions, wait for more process output
      (when (< so-far num-completions)
        (accept-process-output proc readline-completions-timeout)))
    
    (nreverse completions)))

(defun readline--system-uses-bash ()
  "Return t if the current system uses bash as its shell."
  (< 0 (string-match "\\<bash\\>" shell-file-name)))

(defun readline-completion ()
  "Get completions from readline."
  ;; First time initialization of readline completions
  (if (eq readline-completions-enabled 'unset)
      ;; Only works in minibuffer for now
      (if (minibufferp)
          (setq-local readline-completions-enabled readline-use-minibuffer-completion)
        nil)
    nil)
  (if (not readline-completions-enabled)
      nil
    (when (not (readline--system-uses-bash))
      (error "Readline completions only work with bash.  Disable readline completions by setting `readline-use-minibuffer-completion' to nil"))
    (if (minibufferp (current-buffer))
        (readline-get-completions-reuse (minibuffer-contents) (minibuffer-prompt-end) (- (point-max) (point)))
      nil)))

(defun readline--filter (proc out)
  "Filter for readline processes.
Outputs text to the end of the process buffer with a
`save-excursion' because point is used to track parsing of the
readline responses.

PROC is the process.
OUT is the text recieved from PROC."
  (with-current-buffer (process-buffer proc)
    (save-excursion (goto-char (point-max))
                    (insert out))))

(defun readline--send-request (proc input backs-needed)
  "Send a readline completions request.

PROC is the bash process.
INPUT is the string to complete.
BACKS-NEEDED is the number of characters between the completion
point and the end of INPUT."
  (let ((string-to-send (concat input (make-string backs-needed ?\C-b) "2" readline-export-completions-char)))
    (let ((inhibit-quit t))
      (process-send-string proc string-to-send)
      (setq-local readline-outstanding-requests (+ readline-outstanding-requests 1)))))

(defun readline--add-completion-offset (completions input-offset)
  "Offset completion region by `input-offset`.

COMPLETIONS is a capf completions list (completion region start,
completion region end, (completions)).
INPUT-OFFSET is the amount to shift region."
  (if (not completions)
      nil
    (list (+ (car completions) input-offset) (+ (cadr completions) input-offset) (caddr completions))))

(defun readline--get-most-recent-response (proc input-offset)
  "Get response to most recent readline request.

PROC is the bash process.
READLINE-BUF is the buffer PROC writes into.
INPUT-OFFSET is the byte index of the start of input in the
origin buffer."
  ;; Continue getting completions until we've parsed all the
  ;; outstanding requests
  (let ((completions (readline--completions-from-buffer proc)))
    (if (= readline-outstanding-requests 0)
        (readline--add-completion-offset completions input-offset)
      (readline--get-most-recent-response proc input-offset))))


(defun readline--query-process (proc readline-buf input input-offset point-offset)
  "Get readline completions from PROC.

PROC is the process to query.
READLINE-BUF is the buffer to write data from PROC into and
parse.
INPUT is the text being completed over.
INPUT-OFFSET is the byte index of the start of input in the
origin buffer, which is necessary to output a correct capf list.
POINT-OFFSET is the number of characters between the completion
point and the end of INPUT."
  (set-process-filter proc #'readline--filter)
  (when (= readline-outstanding-requests 0)
    ;; Clear the buffer to get rid of what was left by the last
    ;; completions invocation.
    (with-current-buffer readline-buf (erase-buffer)))

  (with-current-buffer readline-buf
    (readline--send-request proc input point-offset)
    (readline--get-most-recent-response proc input-offset)))

(defun readline--delete-readline-buffer (name)
  "Kill the readline buffer NAME and any associated process."
  (when-let ((buf (get-buffer name)))
    (kill-buffer buf)
    (setq readline-buffer nil)
    (when-let ((proc (get-buffer-process buf)))
      (kill-process proc))))

(defun readline--buffer-name (buf)
  "Create a readline buffer name for a buffer.

BUF is the name of the buffer."
  (make-temp-name (concat " *readline completions*<" buf ">")))

(defun readline-cleanup-hook ()
  "Hook to clean up readline variables for the current buffer.

This kills the buffer's associated readline buffer."
  (when readline-buffer
    (readline--delete-readline-buffer readline-buffer)))

(defun readline--get-bash-command ()
  "Return command list to initiate bash for completions."
  (list shell-file-name))

(defun readline--get-bash-command ()
  "."
  '("~/src/bash/bash"))

(defun readline--get-readline-buffer ()
  "Get the readline buffer for the current buffer.

Creates a new readline buffer and associated bash instance if one
doesn't already exist.  Resets the kill timer for the readline
buffer."
  (if (and (buffer-live-p readline-buffer) (get-buffer-process readline-buffer))
        readline-buffer
    ;; We need to create a new readline buffer
    (add-hook 'minibuffer-exit-hook #'readline-cleanup-hook nil t)
    (let* ((buf-name (readline--buffer-name (buffer-name)))
           (buf (get-buffer-create buf-name t)))   ;; Create a new readline buffer
      (setq-local readline-buffer buf)
      ;; Create the bash processs
      (with-current-buffer buf
        ;; Create an inputrc file binding export-completions
        (let ((tempfile (make-temp-file "inputrc")))
          (with-temp-file tempfile
            (insert (concat readline-export-completions-char-name ": export-completions\n")))
          (with-environment-variables (("INPUTRC" tempfile))
            (make-process :name "readline completions" :command (readline--get-bash-command) :buffer buf))))
      buf)))

(defvar readline--cache-key nil)
(defvar readline--cache-value nil)

(defun readline-get-completions-reuse (input input-offset point-offset)
  "Get completions from readline by querying the cached instance of bash.

INPUT is the relevant text.
INPUT-OFFSET is the offset of the start of input in the origin
buffer.
POINT-OFFSET is the number of characters between the completion
point and the end of INPUT."
  (let ((buf (readline--get-readline-buffer)))
    (let ((key (list buf input input-offset point-offset)))
      (if (equal key readline--cache-key)
          readline--cache-value
        ;; If default directories don't match, completions will be
        ;; wrong.  This shouldn't currently be possible but if
        ;; completions are used for normal buffers, those buffers'
        ;; default directories could change.
        (cl-assert (equal default-directory (with-current-buffer buf default-directory)))
        (let ((proc (get-buffer-process buf)))
          (cl-assert proc)
          (let ((out (readline--query-process proc buf input input-offset point-offset))
                (inhibit-quit t))
            (setq readline--cache-key key)
            (setq readline--cache-value out)
            out))))))

(provide 'readline)

;;; readline.el ends here

(ert-deftest readline--interrupted-request ()
  (with-temp-buffer
    (unwind-protect
        (let* ((readline-buf (readline--get-readline-buffer))
               (proc (get-buffer-process readline-buf)))
          (set-process-filter proc #'readline--filter)
          (with-current-buffer readline-buf
            (readline--send-request proc "" 0)
            (readline--send-request proc "git in" 0)
            (readline--send-request proc "git ad" 0)
            (should (= readline-outstanding-requests 3))
            (should (equal (readline--get-most-recent-response proc 0) '(4 6 ("add "))))))
      (kill-buffer readline-buffer))))
  
  
