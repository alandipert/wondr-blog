#!/bin/sh
":"; exec emacs --quick --script "$0" "$@" # -*- mode: emacs-lisp; lexical-binding: t; -*-

(setq debug-on-error t
      dired-use-ls-dired nil)

(require 'avl-tree)
(require 'cl-lib)
(require 'json)
(require 'parse-time)

(cl-defun sh (&rest args)
  (shell-command-to-string
   (mapconcat #'identity args " ")))

(cl-defun reading-time (file &optional (wpm 275.0))
  (with-temp-buffer
    (insert-file-contents file)
    (let* ((minutes (/ (count-words-region (point-min) (point-max)) wpm)))
      (format "%s minute read" (ceiling minutes)))))

(cl-defun parse-iso-date (date-string)
  (let* ((day-month-year (cl-subseq (parse-time-string date-string) 3 6)))
    (apply #'encode-time `(0 0 0 ,@day-month-year))))

(defmacro with-tmp (file-name-var suffix &rest body)
  (declare (indent defun))
  `(let* ((,file-name-var (make-temp-file "with-tmp" nil ,suffix)))
     (unwind-protect (progn ,@body)
       (delete-file ,file-name-var))))

(cl-defun script-command-meta-json (file)
  "The metadata of a Pandoc markdown file formatted as JSON."
  (with-tmp tmp ".plain"
    (with-temp-file tmp (insert "$meta-json$"))
    (sh "pandoc -t plain --template" (file-name-sans-extension tmp) file)))

(cl-defun script-command-post (file)
  "Generates the HTML for a single blog entry."
  (let* ((meta (json-read-from-string (script-command-meta-json file)))
         (time (parse-iso-date (alist-get 'date meta))))
    (sh "pandoc -f markdown -t HTML"
        "--template=templates/article"
        "--highlight-style=kate"
        "-V" (format-time-string "footer-year=%Y" (current-time))
        "-V" (format "iso-date=%s" (format-time-string "%FT%TZ" time))
        "-V" (format "short-iso-date='%s'" (format-time-string "%F" time))
        "-V" (format "reading-time='%s'" (reading-time file))
        file)))

(defmacro with-let (binding &rest body)
  (declare (indent defun))
  `(let* (,binding) ,@body ,(car binding)))

(cl-defun post-before? (x y)
  (apply #'time-less-p (mapcar (apply-partially #'alist-get 'time) (list x y))))

(cl-defun replace-extension (filename new-ext)
  (concat (file-name-sans-extension filename) "." new-ext))

(cl-defun complement (func)
  (lambda (&rest args)
    (not (apply func args))))

(cl-defun sorted-posts (posts-dir compare-func)
  (cl-loop with avl-tree = (avl-tree-create compare-func)
           finally return avl-tree
           for post in (directory-files posts-dir nil "\\.md$" t)
           for md-path = (concat (file-name-as-directory posts-dir) post)
           for meta = (json-read-from-string (script-command-meta-json md-path))
           for time = (parse-iso-date (alist-get 'date meta))
           do (avl-tree-enter avl-tree
                              `((path . ,(replace-extension md-path "html"))
                                (title . ,(alist-get 'title meta))
                                (iso-date . ,(format-time-string "%FT%TZ" time))
                                (id . ,(alist-get 'id meta))
                                (abstract . ,(alist-get 'abstract meta))
                                (short-iso-date . ,(format-time-string "%F" time))
                                (time . ,time)))))

(cl-defun render-attrs (attrs)
  (mapconcat (lambda (pair)
               (format "%s=\"%s\"" (car pair) (cdr pair)))
             attrs
             " "))

(cl-defun render-html (dom)
  (if (stringp dom)
      dom
    (cl-destructuring-bind (tag attrs &rest kids) dom
      (let* ((rendered-attrs (render-attrs attrs)))
        (format "<%s%s%s>%s</%s>\n"
                tag
                (if (zerop (length rendered-attrs)) "" " ")
                rendered-attrs
                (mapconcat #'render-html kids "")
                tag)))))

(cl-defun index-post-entry (post)
  `(article ((class . "list-item"))
            (div ((class . "meta"))
                 (span nil
                       (span ((class . "screen-reader")) "Posted on")
                       (time ((datetime . ,(alist-get 'iso-date post)))
                             ,(alist-get 'short-iso-date post))))
            (header ((class . "list-item-header"))
                    (h3 ((class . "list-item-title"))
                        (a ((href . ,(alist-get 'path post)))
                           ,(alist-get 'title post))))))

(cl-defun script-command-index
    (posts-dir template-file)
  "Generates the blog index page."
  (let* ((posts (avl-tree-mapcar
                 #'index-post-entry
                 (sorted-posts posts-dir (complement #'post-before?)))))
    (with-temp-buffer
      (insert-file-contents-literally template-file)
      (unless (re-search-forward "\\$posts\\$" nil t)
        (error "Couldn't find $posts$ variable in index template."))
      (replace-match (mapconcat #'render-html posts ""))
      (buffer-string))))

(cl-defun script-command-atom
    (posts-dir
     feed-title
     feed-author
     feed-baseurl
     feed-id
     entry-baseurl)
  "Generates the Atom feed."
  (let* ((feed-updated (format-time-string "%FT%TZ" nil t))
         (entries (avl-tree-mapcar
                   (lambda (entry)
                     `(entry ()
                       (title () ,(alist-get 'title entry))
                       (link ((href . ,(concat entry-baseurl "/" (alist-get 'path entry)))))
                       (id () ,(alist-get 'id entry))
                       (updated () ,feed-updated)
                       (summary () ,(alist-get 'abstract entry))))
                   (sorted-posts posts-dir #'post-before?))))
    (concat "<?xml version=\"1.0\" encoding=\"utf-8\"?>\n"
            (render-html
             `(feed ((xmlns . "http://www.w3.org/2005/Atom"))
                    (title () ,feed-title)
                    (link ((href . ,feed-baseurl)))
                    (link ((rel . "self")
                           (href . ,(concat feed-baseurl "/atom.xml"))))
                    (updated () ,feed-updated)
                    (author () (name () ,feed-author))
                    (id () ,feed-id)
                    ,@entries)))))



(cl-defun print-usage ()
  (princ "Available commands:\n")
  (let* ((commands nil)
         (command-prefix "script-command-"))
    (mapatoms (lambda (x)
                (when (string-prefix-p "script-command-" (symbol-name x))
                  (push x commands))))
    (dolist (command commands)
      (princ (format " %s: %s\n"
                     (substring (symbol-name command) (length command-prefix))
                     (documentation command))))))

(when noninteractive
  (if (= (length argv) 0)
      (progn
        (print-usage)
        (kill-emacs 1))
    (cl-destructuring-bind (command &rest args) argv
      (let* ((command-sym (intern (concat "script-command-" command))))
        (if (fboundp command-sym)
            (princ (apply command-sym args))
          (error (format "Unknown command: %s" command)))))))
