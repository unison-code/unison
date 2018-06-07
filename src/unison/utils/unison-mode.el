;;; unison-mode.el --- Major mode for Unison (based on LLVM's).

;; Roberto Castaneda Lozano <roberto.castaneda@ri.se>
;; http://www.sics.se/people/rcas

(defvar unison-bound-face 'unison-bound-face)
(defface  unison-bound-face
  '((((class color) (background light)) (:foreground "gray"))
    (((class color) (background dark))  (:foreground "yellow"))
    (((class grayscale))                (:foreground "black"))
    (t                                  ()))
  "Face used for bound operands in Unison"
  :group 'unison)

(defvar unison-font-lock-keywords
(list
;; Comments
'("//.*" . font-lock-comment-face)
;; Bound operands
`("{fi, [-]?[0-9]+}" . unison-bound-face)
`("{imm, [-]?[0-9]+}" . unison-bound-face)
`("{ga, {[^}]+}}" . unison-bound-face)
`("{sym, {[^}]+}}" . unison-bound-face)
`("{reg, -}" . unison-bound-face)
;; Temps
'("\\bt[0-9]+\\b" . font-lock-variable-name-face)
;; Operands
'("\\bp[0-9]+\\b" . font-lock-constant-face)
;; Operations
;;'("o[0-9]+" . font-lock-function-name-face)
;; Block labels
'("b[0-9]+" . font-lock-function-name-face)
;; Keywords
`(,(regexp-opt '("function" "adjacent" "fixed\-frame" "frame" "jump\-table" "goal" "source" "rematerializable" "stack\-pointer\-offset" "stack-arg-size" "removed-freqs" "constants") 'symbol) . font-lock-keyword-face)
;; Builtins
'("\\bcall\\b" . font-lock-builtin-face)
'("\\mem-[0-9]+\\b" . font-lock-builtin-face)
`(,(regexp-opt '("entry" "freq" "exit" "return" "freq" "writes" "reads" "activators" "mem" "virtualcopy" "remat\-origin" "remat" "jtblocks" "split\-barrier" "split" "taken") 'symbol) . font-lock-builtin-face)
;; Virtual operations
`(,(regexp-opt '("define" "in" "out" "combine" "kill" "copy" "fun" "pack" "high" "low" "destroy" "setup" "split2" "split4" "phi") 'symbol) . font-lock-builtin-face)
)
"Syntax highlighting for Unison."
)

(defvar unison-mode-hook nil)
;;;###autoload
(defun unison-mode ()
"Major mode for Unison.
\\{unison-mode-map}
Runs `unison-mode-hook' on startup."
(interactive)
(kill-all-local-variables)
(setq major-mode 'unison-mode)
(make-local-variable 'font-lock-defaults)
(setq major-mode 'unison-mode
mode-name "Unison"
font-lock-defaults `(unison-font-lock-keywords))
(run-hooks 'unison-mode-hook))
;; Associate .uni files with unison-mode
;;;###autoload
(add-to-list 'auto-mode-alist (cons (purecopy "\\.uni\\'") 'unison-mode))
(provide 'unison-mode)
;;; unison-mode.el ends here
