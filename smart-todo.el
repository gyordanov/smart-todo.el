;;; smart-todo.el  --- makes it easier to quickly add todos

;; Copyrigth (C) 2011  Galin Yordanov

;; Author: Galin Yordanov <gyordanov(at)gmail.com>
;; Version: 0.1
;; URL: http://github.com/gyordanov/smart-todo.el
;;

;; This file is *NOT* part of GNU Emacs.

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2, or (at
;; your option) any later version.

;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING. If not, write to the
;; Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.


;;; Description:
;;
;; I love org-mode but keeping my todo list separated for each project was pain ... also i needed to add
;; task fast like taskwarrior, so here is a tiny extension that does that for me
;;

;;; Installation:
;;
;;   To use smart-todo.el, put it in your load-path and add
;;   the following to your .emacs
;;
;;   (require 'smart-todo)

;;; Configuration:
;;
;; configure your projects like this
;; (setq smart-todo-project-dir "/full/path/to/the/root/org-directory/")
;; (setq smart-todo-projects (quote (
;;                                  ("Work/company/project_a" . "project_a")
;;                                  ("Work/someone_else/project_c" . "project_c")
;;                                  ("Work/third/newproj" . "newproj")
;;                                  ("Work/github/smart-todo.el" . "smart-todo")
;;                                  )))
;;
;; what this few lines will do:
;;  * when you cann smart-todo it will examine the current buffer path, if the match matches any of the paths on the
;;    smart-todo-projects alist - it will use that project. for example /home/user/Work/github/smart-todo.el/README will save the todo in
;;    /full/path/to/the/root/org-directory/smart-todo.org
;;  * if it cannot match any known path it will prompt for project name
;;  * you can also include due date in the todo body in a format like d:{NUM}[w|d] ... like:
;;    - d:1d - will have due date in 1 day
;;    - d:4d - will have due date in 4 days
;;    - d:2w - will have due date in 2 weeks
;;
;; you can also bind a key for easy access
;; (define-key global-map [f11] 'smart-todo)
;;
;; TODO
;; * if the buffer is open we do clean the empty lines, try to do the same stuff while saving to a file that is not opened

(defgroup smart-todo nil
  "Smart todo for org-mode."
  :link '(emacs-library-link :tag "Source Lisp File" "smart-todo.el")
  :group 'general
  :prefix "smart-todo")

(defcustom smart-todo-project-dir "/tmp/"
  "Base directory where we will save the org files"
  :type 'directory
  :group 'smart-todo)

(defcustom smart-todo-projects nil
  "Key-value pairs where the key is a simple string/regex and the value is project name,\n if the regex matches the current buffer path, the project will be used, if not the user will be prompted for a project\nMake sure you use double-quotes for both key and value"
  :type 'alist
  :group 'smart-todo)


(defun append-string-to-file (string file)
  (with-temp-buffer
    (insert string)
    (when (file-writable-p file)
      (write-region (point-min)
                    (point-max)
                    file t))))


(defun smart-todo-project-search ()
  (assoc-default buffer-file-name smart-todo-projects (lambda (c k)
                                                        (string-match c k)
                                                        ))
  )
(defun smart-todo-guess-project ()
  (let ((found (smart-todo-project-search)))
    (smart-todo-append-to-project
     (if found
         found
       (read-from-minibuffer "Project: ")))
    )
  )

(defun smart-todo-append-to-buffer (buf body)
  (let ((old_buff (current-buffer)))
    (set-buffer buf)
    (goto-char (point-max))
    (delete-blank-lines)
    (goto-char (point-max))
    (insert body)
    (save-some-buffers 1 (lambda() (if (eq (current-buffer) buf) t nil)))
    (set-buffer old_buff)
    )
  )

(defun smart-todo-parse-due-data-helper-day (str period)
  (let ((dtime (seconds-to-time (+ (float-time) (* (string-to-number period) 86400)))))
    (list str (format-time-string "\n  DEADLINE: <%Y-%m-%d %a>\n" dtime))
    )
  )

(defun smart-todo-parse-due-data-helper-week (str period)
  (let ((dtime (seconds-to-time (+ (float-time) (* (string-to-number period) 86400 7)))))
    (list str (format-time-string "\n  DEADLINE: <%Y-%m-%d %a>\n" dtime))
    )
  )


(defun smart-todo-parse-due-data-helper (str)
  (let (
        (num (match-string 1 str))
        (period (match-string 2 str))
        (newstr (replace-regexp-in-string "\\(\sd:[0-9]+\\)[dw]+" "" str))
        )
    (if (equal period "d")
        (smart-todo-parse-due-data-helper-day newstr num)
      (smart-todo-parse-due-data-helper-week newstr num)
      )
    )
  )

(defun smart-todo-parse-due-date (str)
  (if (string-match "d:\\([0-9]+\\)\\([dw]+\\)" str)
      (smart-todo-parse-due-data-helper str)
    (list str "\n"))
  )

(defun smart-todo-insert-current-file()
  (if buffer-file-name
      (concat "\t \[\[file:" buffer-file-name "::" (number-to-string (point)) "\]\]\n")
    ""
    )
  )

(defun smart-todo-append-to-project (prj)
  (let* (
        (txt (read-from-minibuffer (concat "Todo [" prj "]: ")))
        (file (concat smart-todo-project-dir  prj ".org"))
        (body (concat "* TODO " (mapconcat 'identity (smart-todo-parse-due-date txt) "") (smart-todo-insert-current-file)))
        (todo_buff (get-file-buffer file))
        )

    (if todo_buff
        (smart-todo-append-to-buffer todo_buff body)
      (append-string-to-file (concat "\n" body) file))
    )

)

(defun smart-todo-ask-for-project ()
  ;; (message "ask for it")
  (smart-todo-append-to-project (read-from-minibuffer "Project: "))
  )


(defun smart-todo ()
  "Add a smart todo if the todo contains text like d:1d d:2w the todo will have a deadline in 1 day or 2 weeks"
  (interactive)
  (if buffer-file-name
      (smart-todo-guess-project)
    (smart-todo-ask-for-project))
  )

(provide 'smart-todo)

