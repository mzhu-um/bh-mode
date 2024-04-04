;;; bh-customize.el --- Customization settings -*- lexical-binding: t -*-

;; Copyright (c) 2014 Chris Done. All rights reserved.
;;               2020 Marc Berkowitz <mberkowitz@github.com>

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Code:

(require 'cl-lib)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Customization variables

(defcustom bh-process-load-or-reload-prompt nil
  "Nil means there will be no prompts on starting REPL. Defaults will be accepted."
  :type 'boolean
  :group 'bh-interactive)

(defgroup bh nil
  "Major mode for editing Bh programs."
  :link '(custom-manual "(bh-mode)")
  :group 'languages
  :prefix "bh-")

(defvar bh-mode-pkg-base-dir (file-name-directory load-file-name)
  "Package base directory of installed `bh-mode'.
Used for locating additional package data files.")

(defcustom bh-completing-read-function 'ido-completing-read
  "Default function to use for completion."
  :group 'bh
  :type '(choice
          (function-item :tag "ido" :value ido-completing-read)
          (function-item :tag "helm" :value helm--completing-read-default)
          (function-item :tag "completing-read" :value completing-read)
          (function :tag "Custom function")))

(defcustom bh-process-type
  'auto
  "The inferior Bh process type to use.

Customize this variable to see the supported symbol values.

When set to \\='auto (the default), the directory contents and
available programs will be used to make a best guess at the
process type and the project directory.

Emacs looks in the current directory and then in its parents for
a file \"cabal.sandbox.config\" or \"cabal.project\". its
location is the project directory, and \"cabal\" will be used.

Otherwise if a file \"stack.yaml\" is found, its location is the
project directory, and stack will be used
Otherwise if a file \"*.cabal\" is found, its location is the
project directory, and cabal will be used.
If none of the above apply, ghc will be used.

(The value cabal-new-repl is obsolete, equivalent to cabal-repl)."
  :type '(choice (const auto)
                 (const ghci)
                 (const stack-ghci)
                 (const cabal-repl)
                 (const cabal-new-repl))
  :group 'bh-interactive)

(defcustom bh-process-wrapper-function
  #'identity
  "Wrap or transform bh process commands using this function.

Can be set to a custom function which takes a list of arguments
and returns a possibly-modified list.

The following example function arranges for all bh process
commands to be started in the current nix-shell environment:

  (lambda (argv) (append (list \"nix-shell\" \"-I\" \".\" \"--command\" )
                    (list (mapconcat \\='identity argv \" \"))))

See Info Node `(emacs)Directory Variables' for a way to set this option on
a per-project basis."
  :group 'bh-interactive
  :type '(choice
          (function-item :tag "None" :value identity)
          (function :tag "Custom function")))

(defcustom bh-session-kill-hook nil
  "Hook called when the interactive session is killed.
You might like to call `projectile-kill-buffers' here."
  :group 'bh-interactive
  :type 'hook)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Configuration

(defcustom bh-doc-prettify-types t
  "Replace some parts of types with Unicode characters like \"∷\"
when showing type information about symbols."
  :group 'bh-doc
  :type 'boolean
  :safe 'booleanp)

(defvar bh-process-ended-functions (list 'bh-process-prompt-restart)
  "Hook for when the bh process ends.")

(defgroup bh-interactive nil
  "Settings for REPL interaction via `bh-interactive-mode'"
  :link '(custom-manual "(bh-mode)bh-interactive-mode")
  :group 'bh)

(defcustom bh-process-path-ghci
  "ghci"
  "The path for starting ghci.
This can either be a single string or a list of strings, where the
first elements is a string and the remaining elements are arguments,
which will be prepended to `bh-process-args-ghci'."
  :group 'bh-interactive
  :type '(choice string (repeat string)))

(defcustom bh-process-path-cabal
  "cabal"
  "Path to the `cabal' executable.
This can either be a single string or a list of strings, where the
first elements is a string and the remaining elements are arguments,
which will be prepended to `bh-process-args-cabal-repl'."
  :group 'bh-interactive
  :type '(choice string (repeat string)))

(defcustom bh-process-path-stack
  "stack"
  "The path for starting stack.
This can either be a single string or a list of strings, where the
first elements is a string and the remaining elements are arguments,
which will be prepended to `bh-process-args-stack-ghci'."
  :group 'bh-interactive
  :type '(choice string (repeat string)))

(defcustom bh-process-args-ghci
  '("-ferror-spans")
  "Any arguments for starting ghci."
  :group 'bh-interactive
  :type '(repeat (string :tag "Argument")))

(defcustom bh-process-args-cabal-repl
  '("--ghc-option=-ferror-spans")
  "Additional arguments for `cabal repl' invocation.
Note: The settings in `bh-process-path-ghci' and
`bh-process-args-ghci' are not automatically reused as `cabal repl'
currently invokes `ghc --interactive'. Use
`--with-ghc=<path-to-executable>' if you want to use a different
interactive GHC frontend; use `--ghc-option=<ghc-argument>' to
pass additional flags to `ghc'."
  :group 'bh-interactive
  :type '(repeat (string :tag "Argument")))

(defcustom bh-process-args-stack-ghci
  '("--ghci-options=-ferror-spans" "--no-build" "--no-load")
  "Additional arguments for `stack ghci' invocation."
  :group 'bh-interactive
  :type '(repeat (string :tag "Argument")))

(defcustom bh-process-do-cabal-format-string
  ":!cd %s && %s"
  "The way to run cabal commands.
It takes two arguments -- the directory and the command.
See `bh-process-do-cabal' for more details."
  :group 'bh-interactive
  :type 'string)

(defcustom bh-process-log
  nil
  "Enable debug logging to \"*bh-process-log*\" buffer."
  :type 'boolean
  :group 'bh-interactive)

(defcustom bh-process-show-debug-tips
  t
  "Show debugging tips when starting the process."
  :type 'boolean
  :group 'bh-interactive)

(defcustom bh-process-show-overlays
  t
  "Show in-buffer overlays for errors/warnings.
Flycheck users might like to disable this."
  :type 'boolean
  :group 'bh-interactive)

(defcustom bh-notify-p
  nil
  "Notify using notifications.el (if loaded)?"
  :type 'boolean
  :group 'bh-interactive)

(defcustom bh-process-suggest-no-warn-orphans
  t
  "Suggest adding -fno-warn-orphans pragma to file when getting orphan warnings."
  :type 'boolean
  :group 'bh-interactive)

(defcustom bh-process-suggest-hoogle-imports
  nil
  "Suggest to add import statements using Hoogle as a backend."
  :type 'boolean
  :group 'bh-interactive)

(defcustom bh-process-suggest-bh-docs-imports
  nil
  "Suggest to add import statements using bh-docs as a backend."
  :type 'boolean
  :group 'bh-interactive)

(defcustom bh-process-suggest-add-package
  t
  "Suggest to add packages to your .cabal file when Cabal says it
is a member of the hidden package, blah blah."
  :type 'boolean
  :group 'bh-interactive)

(defcustom bh-process-suggest-language-pragmas
  t
  "Suggest adding LANGUAGE pragmas recommended by GHC."
  :type 'boolean
  :group 'bh-interactive)

(defcustom bh-process-suggest-remove-import-lines
  nil
  "Suggest removing import lines as warned by GHC."
  :type 'boolean
  :group 'bh-interactive)

(defcustom bh-process-suggest-overloaded-strings
  t
  "Suggest adding OverloadedStrings pragma to file.
It is used when getting type mismatches with [Char]."
  :type 'boolean
  :group 'bh-interactive)

(defcustom bh-process-check-cabal-config-on-load
  t
  "Check changes cabal config on loading Bh files and
restart the GHCi process if changed.."
  :type 'boolean
  :group 'bh-interactive)

(defcustom bh-process-prompt-restart-on-cabal-change
  t
  "Ask whether to restart the GHCi process when the Cabal file
has changed?"
  :type 'boolean
  :group 'bh-interactive)

(defcustom bh-process-auto-import-loaded-modules
  nil
  "Auto import the modules reported by GHC to have been loaded?"
  :type 'boolean
  :group 'bh-interactive)

(defcustom bh-process-reload-with-fbytecode
  nil
  "When using -fobject-code, auto reload with -fbyte-code (and
then restore the -fobject-code) so that all module info and
imports become available?"
  :type 'boolean
  :group 'bh-interactive)

(defcustom bh-process-use-presentation-mode
  nil
  "Use presentation mode to show things like type info instead of
  printing to the message area."
  :type 'boolean
  :group 'bh-interactive)

(defcustom bh-process-suggest-restart
  t
  "Suggest restarting the process when it has died"
  :type 'boolean
  :group 'bh-interactive)

(defcustom bh-interactive-popup-errors
  t
  "Popup errors in a separate buffer."
  :type 'boolean
  :group 'bh-interactive)

(defcustom bh-interactive-mode-collapse
  nil
  "Collapse printed results."
  :type 'boolean
  :group 'bh-interactive)

(defcustom bh-interactive-types-for-show-ambiguous
  t
  "Show types when there's no Show instance or there's an
ambiguous class constraint."
  :type 'boolean
  :group 'bh-interactive)

(defcustom bh-interactive-prompt "λ> "
  "The prompt to use."
  :type 'string
  :group 'bh-interactive)

(define-obsolete-variable-alias 'bh-interactive-prompt2 'bh-interactive-prompt-cont "17.1")

(defcustom bh-interactive-prompt-cont (replace-regexp-in-string
                                            "> $"
                                            "| "
                                            bh-interactive-prompt)
  "The multi-line prompt to use.
The default is `bh-interactive-prompt' with the last > replaced with |."
  :type 'string
  :group 'bh-interactive)

(defcustom bh-interactive-mode-eval-mode
  nil
  "Use the given mode's font-locking to render some text."
  :type '(choice function (const :tag "None" nil))
  :group 'bh-interactive)

(defcustom bh-interactive-mode-hide-multi-line-errors
  nil
  "Hide collapsible multi-line compile messages by default."
  :type 'boolean
  :group 'bh-interactive)

(defcustom bh-interactive-mode-delete-superseded-errors
  t
  "Whether to delete compile messages superseded by recompile/reloads."
  :type 'boolean
  :group 'bh-interactive)

(defcustom bh-interactive-mode-include-file-name
  t
  "Include the file name of the module being compiled when
printing compilation messages."
  :type 'boolean
  :group 'bh-interactive)

(defcustom bh-interactive-mode-read-only
  t
  "Non-nil means most GHCi/bh-interactive-mode output is read-only.
This does not include the prompt.  Configure
`bh-interactive-prompt-read-only' to change the prompt's
read-only property."
  :type 'boolean
  :group 'bh-interactive)

(defcustom bh-interactive-prompt-read-only
  bh-interactive-mode-read-only
  "Non-nil means the prompt (and prompt2) is read-only."
  :type 'boolean
  :group 'bh-interactive)

(defcustom bh-import-mapping
  '()
  "Support a mapping from module to import lines.

E.g. \\='((\"Data.Map\" . \"import qualified Data.Map as M
import Data.Map (Map)
\"))

This will import

import qualified Data.Map as M
import Data.Map (Map)

when Data.Map is the candidate.

"
  :type '(repeat (cons (string :tag "Module name")
                       (string :tag "Import lines")))
  :group 'bh-interactive)

(defcustom bh-language-extensions
  '()
  "Language extensions in use. Should be in format: -XFoo,
-XNoFoo etc. The idea is that various tools written with HSE (or
any bh-mode code that needs to be aware of syntactical
properties; such as an indentation mode) that don't know what
extensions to use can use this variable. Examples: hlint,
hindent, structured-bh-mode, tool-de-jour, etc.

You can set this per-project with a .dir-locals.el file"
  :group 'bh
  :type '(repeat 'string))

(defcustom bh-stylish-on-save nil
  "Whether to run stylish-bh on the buffer before saving.
If this is true, `bh-add-import' will not sort or align the
imports."
  :group 'bh
  :type 'boolean)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Accessor functions

(defvar inferior-bh-root-dir nil
  "The path which is considered as project root, this is determined by the
presence of a *.cabal file or stack.yaml file or something similar.")

(defun bh-build-type ()
  "Looks for cabal and stack spec files.
   When found, returns a pair (TAG . DIR)
   where TAG is \\='cabal-project, \\='cabal-sandbox. \\='cabal, or \\='stack;
   and DIR is the directory containing cabal or stack file.
   When none found, DIR is nil, and TAG is \\='ghc"
  ;; REVIEW maybe just 'cabal is enough.
  (let ((cabal-project (locate-dominating-file default-directory "cabal.project"))
        (cabal-sandbox (locate-dominating-file default-directory "cabal.sandbox.config"))
        (stack         (locate-dominating-file default-directory "stack.yaml"))
        (cabal         (locate-dominating-file
                        default-directory
                        (lambda (d)
                          (cl-find-if
                           (lambda (f) (string-match-p ".\\.cabal\\'" f))
                           (directory-files d))))))
    (cond
     ((and cabal-project (executable-find "cabal"))
      (cons 'cabal-project cabal-project))
     ((and cabal-sandbox (executable-find "cabal"))
      (cons 'cabal-sandbox cabal-sandbox))
     ((and stack (executable-find "stack"))
      (cons 'stack stack))
     ((and cabal (executable-find "cabal"))
      (cons 'cabal cabal))
     ((executable-find "ghc") (cons 'ghc nil))
     (t (error "Could not find any installation of GHC.")))))

(defun bh-process-type ()
  "Return `bh-process-type', or a guess if that variable is \\='auto.
   Converts the obsolete \\='cabal-new-repl to its equivalent \\='cabal-repl.
   May also set `inferior-bh-root-dir'"
  (cond
   ((eq 'cabal-new-repl bh-process-type)
    (warn "bh-process-type has obsolete value 'cabal-new-repl, changing it to 'cabal-repl")
    (setq bh-process-type 'cabal-repl) ;to avoid repeating the same warning
    'cabal-repl)
   ((eq 'auto bh-process-type)
    (let* ((r (bh-build-type))
           (tag (car r))
           (dir (cdr r)))
      (setq inferior-bh-root-dir (or dir default-directory))
      (cdr (assq tag '((cabal-project . cabal-repl)
                       (cabal-sandbox . cabal-repl)
                       (cabal . cabal-repl)
                       (stack . stack-ghci)
                       (ghc . ghci))))))
   (t bh-process-type)))

(provide 'bh-customize)
