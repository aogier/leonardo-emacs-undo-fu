;;; undo-fu.el --- Undo helper with redo -*- lexical-binding: t -*-

;; Copyright (C) 2019  Campbell Barton

;; Author: Campbell Barton <ideasman42@gmail.com>

;; URL: https://gitlab.com/ideasman42/emacs-undo-fu
;; Version: 0.2
;; Package-Requires: ((emacs "24.3"))

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; Undo/redo convenience wrappers to Emacs default undo commands.
;;
;; The redo operation can be accessed from a key binding
;; and stops redoing once the initial undo action is reached.
;;
;; If you want to cross the initial undo boundary to access
;; the full history, running [keyboard-quit] (typically C-g).
;; lets you continue redoing for functionality not typically
;; accessible with regular undo/redo.
;;

;;; Usage

;; ;; Bind the keys
;; (global-unset-key (kbd "C-z"))
;; (global-set-key (kbd "C-z")   'undo-fu-only-undo)
;; (global-set-key (kbd "C-S-z") 'undo-fu-only-redo)

;;; Code:

;; ---------------------------------------------------------------------------
;; Custom Variables

(defcustom undo-fu-allow-undo-in-region nil
  "When t, use `undo-in-region' when a selection is present.
Otherwise `undo-in-region' is never used, since it doesn't support `undo-only',
causing undo-fu to work with reduced functionality when a selection exists."
  :group 'undo-fu
  :type 'boolean)

;; ---------------------------------------------------------------------------
;; Internal Variables

;; Apply undo/redo constraints to stop redo from undoing or
;; passing the initial undo checkpoint.
(defvar-local undo-fu--respect t)
;; Initiated an undo-in region (don't use `undo-only').
;; Only use when `undo-fu-allow-undo-in-region' is true.
(defvar-local undo-fu--in-region nil)

;; ---------------------------------------------------------------------------
;; Internal Functions/Macros

(defun undo-fu--checkpoint-disable ()
  "Disable using the checkpoint, allowing the initial boundary to be crossed when redoing."
  (setq undo-fu--respect nil)
  (setq undo-fu--in-region nil))

(defmacro undo-fu--with-message-suffix (suffix &rest body)
  "Add text after the message output.
Argument SUFFIX is the text to add at the end of the message.
Optional argument BODY runs with the message suffix."
  (declare (indent 1))
  (let ((message-orig (cl-gensym "--message-suffix-")))
    `
    (cl-letf*
      (
        (,message-orig (symbol-function 'message))
        ((symbol-function 'message)
          (lambda (arg &rest args)
            (apply ,message-orig (append (list (concat arg "%s")) args (list ,suffix))))))
      ,@body)))

(defun undo-fu--last-change-was-undo-p (undo-list)
  (while (and (consp undo-list) (eq (car undo-list) nil))
    (setq undo-list (cdr undo-list)))
  (gethash undo-list undo-equiv-table))

(defun undo-fu--redo (&optional arg)
  "Undo the last ARG undos."
  (interactive "*p")
  (cond
    ((not (undo-fu--last-change-was-undo-p buffer-undo-list))
      (user-error "No undo to undo"))
    (t
      (let*
        (
          (ul buffer-undo-list)
          (new-ul
            (let ((undo-in-progress t))
              (while (and (consp ul) (eq (car ul) nil))
                (setq ul (cdr ul)))
              (primitive-undo arg ul)))
          (new-pul (undo-fu--last-change-was-undo-p new-ul)))
        (message
          "Redo%s"
          (if undo-in-region
            " in region"
            ""))
        (setq this-command 'undo)
        (setq pending-undo-list new-pul)
        (setq buffer-undo-list new-ul)))))

(defun undo-fu--next-step (list)
  "Get the next undo step in the list.

Argument LIST compatible list `buffer-undo-list'."
  (while (car list)
    (setq list (cdr list)))
  (while (and list (null (car list)))
    (setq list (cdr list)))
  list)

;; ---------------------------------------------------------------------------
;; Public Functions

;;;###autoload
(defun undo-fu-only-redo-all ()
  "Redo all actions until the initial undo step.

wraps the `undo' function."
  (interactive "*")
  (undo-fu--with-message-suffix " All" (undo-fu-only-redo most-positive-fixnum)))

;;;###autoload
(defun undo-fu-only-redo (&optional arg)
  "Redo an action until the initial undo action.

wraps the `undo' function.

Optional argument ARG The number of steps to redo."
  (interactive "*p")

  (let*
    ( ;; Assign for convenience.
      (was-undo (not (null (member last-command '(undo undo-fu-only-undo)))))
      (was-redo (not (null (member last-command '(undo-fu-only-redo)))))
      (was-undo-or-redo (or was-undo was-redo)))

    ;; Reset the option to not respect the checkpoint
    ;; after running non-undo related commands.
    (unless undo-fu--respect
      (unless was-undo-or-redo
        (when undo-fu-allow-undo-in-region
          (setq undo-fu--in-region nil))
        (setq undo-fu--respect t)))

    (when (region-active-p)
      (if undo-fu-allow-undo-in-region
        (progn
          (message "Undo in region in use. Undo end-point ignored!")
          (undo-fu--checkpoint-disable)
          (setq undo-fu--in-region t))
        ;; Default behavior, just remove selection.
        (deactivate-mark)))

    ;; Allow crossing the boundary, if we press [keyboard-quit].
    ;; This allows explicitly over-stepping the boundary,
    ;; in cases when users want to bypass this constraint.
    (when undo-fu--respect
      (when (string-equal last-command 'keyboard-quit)
        (undo-fu--checkpoint-disable)
        (message "Redo end-point stepped over!")))

    ;; Ensure the undo checkpoint is usable.
    (when undo-fu--respect
      (when (eq last-command 'undo)
        (undo-fu--checkpoint-disable)
        (message "Redo 'undo' called, not 'undo-fu-only-undo', checkpoint disabled!")))

    (when undo-fu--respect
      (unless
        ;; Ensure the next steps is a redo action.
        (let ((list (undo-fu--next-step buffer-undo-list)))
          (and list (gethash list undo-equiv-table)))
        (user-error
          "Redo step not found (%s to ignore)"
          (substitute-command-keys "\\[keyboard-quit]"))))

    (when undo-fu--respect
      (unless (undo-fu--last-change-was-undo-p buffer-undo-list)
        (user-error
          "Redo end-point hit (%s to step over it)"
          (substitute-command-keys "\\[keyboard-quit]"))))

    (let*
      (
        ;; It's important to clamp the number of steps before assigning
        ;; 'last-command' since it's used when checking the available steps.
        (steps (or arg 1))
        (last-command
          (cond
            (was-undo
              ;; Break undo chain, avoid having to press [keyboard-quit].
              'ignore)
            (was-redo
              ;; Checked by the undo function.
              'undo)
            (t
              ;; No change.
              last-command)))
        (_success
          (condition-case err
            (progn
              (if undo-fu--respect
                (undo-fu--redo steps)
                (undo-fu--with-message-suffix " (unconstrained)" (undo steps)))
              t)
            (error
              (progn
                (message "%s" (error-message-string err))
                nil)))))))

  (setq this-command 'undo-fu-only-redo))

;;;###autoload
(defun undo-fu-only-undo (&optional arg)
  "Undo the last action.

wraps the `undo-only' function.

Optional argument ARG the number of steps to undo."
  (interactive "*p")

  (let*
    ( ;; Assign for convenience.
      (was-undo (not (null (member last-command '(undo undo-fu-only-undo)))))
      (was-redo (not (null (member last-command '(undo-fu-only-redo)))))
      (was-undo-or-redo (or was-undo was-redo)))

    ;; Reset the option to not respect the checkpoint
    ;; after running non-undo related commands.
    (unless undo-fu--respect
      (unless was-undo-or-redo
        (when undo-fu-allow-undo-in-region
          (setq undo-fu--in-region nil))
        (setq undo-fu--respect t)))

    (when (region-active-p)
      (if undo-fu-allow-undo-in-region
        (progn
          (message "Undo in region in use. Undo end-point ignored!")
          (undo-fu--checkpoint-disable)
          (setq undo-fu--in-region t))
        ;; Default behavior, just remove selection.
        (deactivate-mark)))

    ;; Allow crossing the boundary, if we press [keyboard-quit].
    ;; This allows explicitly over-stepping the boundary, in cases where it's needed.
    (when undo-fu--respect
      (when (string-equal last-command 'keyboard-quit)
        (undo-fu--checkpoint-disable)
        (message "Undo end-point ignored!")))

    (let*
      ;; Swap in 'undo' for our own function name.
      ;; Without this undo won't stop once the first undo step is reached.
      (
        (steps (or arg 1))
        (last-command
          (cond
            (was-undo-or-redo
              ;; Checked by the undo function.
              'undo)
            (t
              ;; No change.
              last-command)))
        (_success
          (condition-case err
            (progn
              (undo-fu--with-message-suffix
                (if undo-fu--respect
                  ""
                  " (unconstrained)")
                (if undo-fu--in-region
                  (undo steps)
                  (undo-only steps)))
              t)
            (error
              (progn
                (message "%s" (error-message-string err))
                nil)))))))
  (setq this-command 'undo-fu-only-undo))

;; Evil Mode (setup if in use)
;;
;; Don't let these commands repeat.
(declare-function evil-declare-not-repeat "ext:evil-common")
(eval-after-load
  'evil
  '
  (progn
    (evil-declare-not-repeat 'undo-fu-only-undo)
    (evil-declare-not-repeat 'undo-fu-only-redo)
    (evil-declare-not-repeat 'undo-fu-only-redo-all)))

(provide 'undo-fu)

;;; undo-fu.el ends here
