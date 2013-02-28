;;; keyadvice.el --- Advice keybindings to run additional user defined code.

;; Copyright (C) 2013 Matus Goljer

;; Author: Matus Goljer <matus.goljer@gmail.com>
;; Maintainer: Matus Goljer <matus.goljer@gmail.com>
;; Created: 28 Feb 2013
;; Version: 0.0.1
;; Keywords: extensions, lisp, tools
;; URL: https://github.com/Fuco1/keyadvice.el

;; This file is not part of GNU Emacs.

;;; License:

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; See github readme at https://github.com/Fuco1/keyadvice.el

;; This package provides a functionality similar to `advice' package
;; but for key bindings.  You can advice any keybinding to run your
;; own code before or after the original action.  This is useful if
;; you want to run some conditional code together with the original
;; (whatever that might be) binding.  The original binding is a
;; binding this key would normally invoke.

;; Inside the forms you provide to the macro you can use a special
;; value "keyadvice-do-it" that will be replaced with code that will
;; call the original function.

;;; Code:

(defvar keyadvice-mode-map (make-sparse-keymap)
  "Keymap used for advices.")

(defvar keyadvice--map-alist
  (list (cons 'keyadvice--emu-mode-map 'keyadvice-mode-map))
  "Used in `emulation-mode-map-alists' to override all the other minor modes.")

(defvar keyadvice-advice-list nil
  "List of adviced bindings.")

(defgroup keyadvice nil
  "keyadvice group."
  :group 'emacs)

(defun keyadvice--subst-original (form)
  "Substitute \"keyadvice-do-it\" with code to call the original funciton."
  (cond
   ((listp form)
    (mapcar 'keyadvice--subst-original form))
   (t (if (eq form 'keyadvice-do-it)
          `(let ((keyadvice-mode nil))
             (let ((com (key-binding ,binding)))
               (when com
                 (call-interactively com))))
        `,form))))

;;;###autoload
(defmacro keyadvice-add-advice (binding &rest forms)
  "Advice the command executed by BINDING.

Replace the command bound to BINDING with function executing
FORMS.  If FORMS contain special value \"keyadvice-do-it\", this
will expand to code that will call the original function.
Original function is whatever command would run if this binding
weren't adviced.

See also emacs function advices (`defadvice') if you don't quite
understand why is this useful."
  (declare (indent 1))
  `(keyadvice--add-advice ,binding ',@forms))

(defun keyadvice--add-advice (binding &rest forms)
  "Advice the routine executed by BINDING.
The token \"keyadvice-do-it\" will expand to the original binding."
  (let* ((binding-name
          (replace-regexp-in-string
           " " "-" (format "%s" binding)))
         (orig-binding (let ((keyadvice-mode-map nil)) (key-binding binding)))
         (funct (if (member orig-binding keyadvice-advice-list)
                    (intern (symbol-name orig-binding))
                  (intern (concat binding-name "$" (symbol-name orig-binding))))))
    (fset funct `(lambda () (interactive) ,@(mapcar 'keyadvice--subst-original forms)))
    (add-to-list 'keyadvice-advice-list funct)
    (define-key keyadvice-mode-map binding funct)))

;;;###autoload
(define-minor-mode keyadvice-mode
  "Toggle keyadvice mode."
  :init-value nil
  :lighter " KA"
  :group 'keyadvice
  :keymap keyadvice-mode-map
  (if keyadvice-mode
      (progn
        (add-to-ordered-list 'emulation-mode-map-alists 'keyadvice--map-alist 1))
    (setq emulation-mode-map-alists (remove 'keyadvice--map-alist emulation-mode-map-alists))))

;;;###autoload
(define-globalized-minor-mode keyadvice-global-mode
  keyadvice-mode
  turn-on-keyadvice-mode)

;;;###autoload
(defun turn-on-keyadvice-mode ()
  "Turn on `keyadvice-mode'."
  (interactive)
  (keyadvice-mode t))

;;;###autoload
(defun turn-off-keyadvice-mode ()
  "Turn off `keyadvice-mode'."
  (interactive)
  (keyadvice-mode nil))

(provide 'keyadvice)

;;; keyadvice.el ends here
