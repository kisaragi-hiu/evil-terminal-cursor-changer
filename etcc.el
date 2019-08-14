;;; etcc.el --- Change cursor shape and color by evil state in terminal  -*- lexical-binding: t -*-
;;
;; Author: 7696122
;; Maintainer: 7696122
;; Fork: Kisaragi Hiu
;; Version: 0.0.4
;; Package-Requires: ((emacs "24.1") (evil "1.0.8"))
;; URL: https://github.com/kisaragi-hiu/evil-terminal-cursor-changer
;; Keywords: evil, terminal, cursor
;;
;;
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 3, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth
;; Floor, Boston, MA 02110-1301, USA.
;;
;;; Commentary:
;; Evil terminal cursor changer.
;;; Code:

(require 'evil)
(require 'color)

(defgroup etcc nil
  "Cursor changer for evil on terminal."
  :group 'cursor
  :prefix "etcc-")

(defcustom etcc-use-color nil
  "Whether to cursor color."
  :type 'boolean
  :group 'etcc)

(defcustom etcc-use-blink t
  "Whether to cursor blink."
  :type 'boolean
  :group 'etcc)

(defun etcc--in-dumb? ()
  "Running in dumb."
  (string= (getenv "TERM") "dumb"))

(defun etcc--in-iterm? ()
  "Running in iTerm."
  (string= (getenv "TERM_PROGRAM") "iTerm.app"))

(defun etcc--in-xterm? ()
  "Runing in xterm."
  (getenv "XTERM_VERSION"))

(defun etcc--in-gnome-terminal? ()
  "Running in gnome-terminal."
  (string= (getenv "COLORTERM") "gnome-terminal"))

(defun etcc--in-konsole? ()
  "Running in konsole."
  (getenv "KONSOLE_PROFILE_NAME"))

(defun etcc--in-apple-terminal? ()
  "Running in Apple Terminal"
  (string= (getenv "TERM_PROGRAM") "Apple_Terminal"))

(defun etcc--in-tmux? ()
  "Running in tmux."
  (getenv "TMUX"))

(defun etcc--get-current-gnome-profile-name ()
  "Return Current profile name of Gnome Terminal."
  ;; https://github.com/helino/current-gnome-terminal-profile/blob/master/current-gnome-terminal-profile.sh
  (if (etcc--in-gnome-terminal?)
      (let ((cmd "#!/bin/sh
FNAME=$HOME/.current_gnome_profile
gnome-terminal --save-config=$FNAME
ENTRY=`grep ProfileID < $FNAME`
rm $FNAME
TERM_PROFILE=${ENTRY#*=}
echo -n $TERM_PROFILE"))
        (shell-command-to-string cmd))
    "Default"))

(defun etcc--color-name-to-hex (color)
  "Convert color name to hex value."
  (apply 'color-rgb-to-hex (color-name-to-rgb color)))

(defun etcc--make-tmux-seq (seq)
  "Make escape sequence for tumx."
  (let ((prefix "\ePtmux;\e")
        (suffix "\e\\"))
    (concat prefix seq suffix)))

(defun etcc--make-konsole-cursor-shape-seq (shape)
  "Make escape sequence for konsole."
  (let ((prefix  "\e]50;CursorShape=")
        (suffix  "\x7")
        (box     "0")
        (bar     "1")
        (hbar    "2")
        (seq     nil))
    (unless (member shape '(box bar hbar))
      (setq shape 'box))
    (cond ((eq shape 'box)
           (setq seq (concat prefix box suffix)))
          ((eq shape 'bar)
           (setq seq (concat prefix bar suffix)))
          ((eq shape 'hbar)
           (setq seq (concat prefix hbar suffix))))
    (if (etcc--in-tmux?)
        (etcc--make-tmux-seq seq)
      seq)))

(defun etcc--make-gnome-terminal-cursor-shape-seq (shape)
  "Make escape sequence for gnome terminal."
  (let* ((profile (etcc--get-current-gnome-profile-name))
         (prefix  (format "gconftool-2 --type string --set /apps/gnome-terminal/profiles/%s/cursor_shape "
                          profile))
         (box     "block")
         (bar     "ibeam")
         (hbar    "underline"))
    (unless (member shape '(box bar hbar))
      (setq shape 'box))
    (cond ((eq shape 'box)
           (concat prefix box))
          ((eq shape 'bar)
           (concat prefix bar))
          ((eq shape 'hbar) hbar))))

(defun etcc--make-xterm-cursor-shape-seq (shape)
  "Make escape sequence for XTerm."
  (let ((prefix      "\e[")
        (suffix      " q")
        (box-blink   "1")
        (box         "2")
        (hbar-blink  "3")
        (hbar        "4")
        (bar-blink   "5")
        (bar         "6"))
    (unless (member shape '(box bar hbar))
      (setq shape 'box))
    (cond ((eq shape 'box)
           (setq seq (concat prefix (if (and etcc-use-blink blink-cursor-mode) box-blink box) suffix)))
          ((eq shape 'bar)
           (setq seq (concat prefix (if (and etcc-use-blink blink-cursor-mode) bar-blink bar) suffix)))
          ((eq shape 'hbar)
           (setq seq (concat prefix (if (and etcc-use-blink blink-cursor-mode) hbar-blink hbar) suffix))))
    (if (etcc--in-tmux?)
        (etcc--make-tmux-seq seq)
        seq)))

(defun etcc--make-cursor-shape-seq (shape)
  "Make escape sequence for cursor shape."
  (cond ((or (etcc--in-xterm?)
             (etcc--in-apple-terminal?)
             (etcc--in-iterm?)
             (etcc--in-dumb?))
         (etcc--make-xterm-cursor-shape-seq shape))
        ((etcc--in-konsole?)
         (etcc--make-konsole-cursor-shape-seq shape))
        ((etcc--in-gnome-terminal?)
         (etcc--make-gnome-terminal-cursor-shape-seq shape))))

(defun etcc--make-cursor-color-seq (color)
  "Make escape sequence for cursor color."
  (let ((hex-color (etcc--color-name-to-hex color)))
    (if hex-color
        ;; https://www.iterm2.com/documentation-escape-codes.html
        (let ((prefix (if (etcc--in-iterm?)
                          "\e]Pl"
                        "\e]12;"))
              (suffix (if (etcc--in-iterm?)
                          "\e\\"
                        "\a")))
          (concat prefix
                  ;; https://www.iterm2.com/documentation-escape-codes.html
                  ;; Remove #, rr, gg, bb are 2-digit hex value for iTerm.
                  (if (and (etcc--in-iterm?)
                           (string-prefix-p "#" hex-color))
                      (substring hex-color 1)
                    hex-color)
                  suffix)))))

(defun etcc--apply-to-terminal (seq)
  "Send to escape sequence to terminal."
  (if (and seq
           (stringp seq))
      (send-string-to-terminal seq)))

(defun etcc--evil-set-cursor-color (color)
  "Set cursor color."
  (etcc--apply-to-terminal (etcc--make-cursor-color-seq color)))

(defun etcc--evil-set-cursor ()
  "Set cursor color type."
  (unless (display-graphic-p)
    (if (symbolp cursor-type)
        (etcc--apply-to-terminal (etcc--make-cursor-shape-seq cursor-type))
      (if (listp cursor-type)
          (etcc--apply-to-terminal (etcc--make-cursor-shape-seq (car cursor-type)))))))

(defun etcc-on ()
  "Enable evil terminal cursor changer."
  (interactive)
  (if etcc-use-blink (add-hook 'blink-cursor-mode-hook #'etcc--evil-set-cursor))
  ;; (ad-activate 'evil-set-cursor)
  ;; (advice-add 'evil-set-cursor :after 'etcc--evil-set-cursor)
  ;; (advice-add 'evil-set-cursor :after #'etcc--evil-set-cursor)
  ;; (advice-add 'evil-set-cursor-color :after #'etcc--evil-set-cursor-color)
  (add-hook 'pre-command-hook 'etcc--evil-set-cursor)
  (add-hook 'post-command-hook 'etcc--evil-set-cursor))

(defun etcc-off ()
  "Disable evil terminal cursor changer."
  (interactive)
  (if etcc-use-blink (remove-hook 'blink-cursor-mode-hook 'etcc--evil-set-cursor))
  ;; (ad-deactivate 'evil-set-cursor)
  ;; (advice-remove 'evil-set-cursor 'etcc--evil-set-cursor)
  ;; (advice-add 'evil-set-cursor 'etcc--evil-set-cursor)
  ;; (advice-remove 'evil-set-cursor-color 'etcc--evil-set-cursor-color)
  (remove-hook 'pre-command-hook 'etcc--evil-set-cursor)
  (remove-hook 'post-command-hook 'etcc--evil-set-cursor))

(define-obsolete-function-alias
  'evil-terminal-cursor-changer-activate 'etcc-on "2019-08-15")
(define-obsolete-function-alias
  'evil-terminal-cursor-changer-deactivate 'etcc-off "2019-08-15")

;;;###autoload
(define-minor-mode etcc-mode
  "Toggle Evil terminal cursor changer mode.

Change cursor shape and color by evil state in terminal."
  :group 'etcc
  :global t :lighter " Etcc"
  (if etcc-mode (etcc-on) (etcc-off)))

(provide 'etcc)

;;; evil-terminal-cursor-changer.el ends here
