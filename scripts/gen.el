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

(cl-defun blog-command-meta-json (file)
  "The metadata of a Pandoc markdown file formatted as JSON."
  (with-tmp tmp ".plain"
    (with-temp-file tmp (insert "$meta-json$"))
    (sh "pandoc -t plain --template" (file-name-sans-extension tmp) file)))

(defun find-tag (html-tree tag-name)
  (when (listp html-tree)
    (nconc (when (eq (car html-tree) tag-name)
             (list html-tree))
           (cl-mapcan (lambda (x)
                        (find-tag x tag-name))
                      (cddr html-tree)))))

(cl-defun blog-command-syntax-highlight-css ()
  "Produces the highlighting CSS used by pandoc"
  (with-tmp html-out ".html"
    (with-tmp md-in ".md"
      (with-temp-file md-in (insert "```{.r}\n```"))
      (sh "pandoc -f markdown -t HTML --standalone" "-o" html-out md-in)
      (with-temp-buffer
        (insert-file-contents html-out)
        (let* ((html (libxml-parse-html-region (point-min) (point-max)))
               (style-tags (find-tag html 'style))
               (style-strings (mapcar (lambda (tag) (nth 2 tag)) style-tags)))
          (apply #'concat style-strings))))))

(cl-defun blog-command-post (file)
  "Generates the HTML for a single blog entry."
  (let* ((meta (json-read-from-string (blog-command-meta-json file)))
         (time (parse-iso-date (alist-get 'date meta))))
    (sh "pandoc -f markdown -t HTML"
        "--template=templates/article"
        "-V" (format-time-string "footer-year=%Y" (current-time))
        "-V" (format "iso-date=%s" (format-time-string "%FT%TZ" time))
        "-V" (format "short-iso-date='%s'" (format-time-string "%F" time))
        "-V" (format "reading-time='%s'" (reading-time file))
        file)))

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

(cl-defun sorted-posts (posts-dir compare-func &aux (avl-tree (avl-tree-create compare-func)))
  "Provided a directory of posts and a function to compare them
by, returns an avl tree of the posts sorted by compare-func.
Posts are alists various properties given by the path to
posts-dir and the metadata in the posts."
  (dolist (post (directory-files posts-dir nil "\\.md$" t) avl-tree)
    (let* ((md-path (concat (file-name-as-directory posts-dir) post))
           (meta (json-read-from-string (blog-command-meta-json md-path)))
           (time (parse-iso-date (alist-get 'date meta))))
      (avl-tree-enter avl-tree
                      `((path . ,(replace-extension md-path "html"))
                        (title . ,(alist-get 'title meta))
                        (iso-date . ,(format-time-string "%FT%TZ" time))
                        (id . ,(alist-get 'id meta))
                        (abstract . ,(alist-get 'abstract meta))
                        (short-iso-date . ,(format-time-string "%F" time))
                        (time . ,time))))))

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

(cl-defun blog-command-index (posts-dir template-file)
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

(cl-defun blog-command-atom
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
         (command-prefix "blog-command-"))
    (mapatoms (lambda (x)
                (when (string-prefix-p "blog-command-" (symbol-name x))
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
    ;; Any functions named blog-command-* are accessible from the command line
    ;; as an "action" argument to this script. Subsequent arguments are passed
    ;; to the function as arguments.
    (cl-destructuring-bind (command &rest args) argv
      (let* ((command-sym (intern (concat "blog-command-" command))))
        (if (fboundp command-sym)
            (princ (apply command-sym args))
          (error (format "Unknown command: %s" command)))))))
