;; -*- mode: emacs-lisp; lexical-binding: t; -*-

(setq debug-on-error t
      dired-use-ls-dired nil)

(require 'avl-tree)
(require 'cl-lib)
(require 'json)
(require 'parse-time)

(cl-defun sh (command &rest args)
  "Runs the shell command with args and returns its standard output as a string."
  (shell-command-to-string
   (mapconcat #'identity (cons command args) " ")))

(cl-defun reading-time (file &optional (wpm 275.0))
  "Estimates the reading time of the text file. Defaults to a wpm
of 275, same as Medium."
  (with-temp-buffer
    (insert-file-contents file)
    (let* ((minutes (/ (count-words-region (point-min) (point-max)) wpm)))
      (format "%s minute read" (ceiling minutes)))))

(cl-defun parse-iso-date (date-string)
  "Parses an ISO date string of the form YYYY-MM-DD and converts
it to an Emacs 'internal time'."
  (let* ((day-month-year (cl-subseq (parse-time-string date-string) 3 6)))
    (apply #'encode-time `(0 0 0 ,@day-month-year))))

(defmacro with-tmp (file-name-var suffix &rest body)
  "Evaluates body in a context with file-name-var bound to the
name of a temporary file. Deletes the temporary file and then
returns the result of body."
  (declare (indent defun))
  `(let* ((,file-name-var (make-temp-file "with-tmp" nil ,suffix)))
     (unwind-protect (progn ,@body)
       (delete-file ,file-name-var))))

(defmacro defcommand (name &rest defun-spec)
  "Defines a 'command', which is a function accessible as a
command argument to this script. A command is any function with
the prefix script-command-, this is just a convenience macro for
creating one."
  `(cl-defun ,(intern (concat "script-command-" (symbol-name name)))
       ,@defun-spec))

(cl-defun script-command-meta-json (file)
  "The metadata of a Pandoc markdown file formatted as JSON."
  (with-tmp tmp ".plain"
    (with-temp-file tmp (insert "$meta-json$"))
    (sh "pandoc -t plain --template" (file-name-sans-extension tmp) file)))

(defcommand post (file)
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
  "Evaluates body with binding, and returns the value of the
binding variable. Equivalent to (let* ((x ...)) ... x)"
  (declare (indent defun))
  `(let* (,binding) ,@body ,(car binding)))

(cl-defun post-before? (x y)
  "True when post x has an 'internal time' time property that
precedes that of post y."
  (apply #'time-less-p (mapcar (apply-partially #'alist-get 'time) (list x y))))

(cl-defun replace-extension (filename new-ext)
  "Replaces the extension of filename with new-ext."
  (concat (file-name-sans-extension filename) "." new-ext))

(cl-defun complement (func)
  "Returns the complement of predicate func."
  (lambda (&rest args)
    (not (apply func args))))

(cl-defun sorted-posts (posts-dir compare-func)
  "Provided a directory of posts and a function to compare them
by, returns an avl tree of the posts sorted by compare-func.
Posts are alists various properties given by the path to
posts-dir and the metadata in the posts."
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
                       (updated () ,(alist-get 'iso-date entry))
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
