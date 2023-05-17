
(require 'org)
(require 'ox-html)
(require 'yaml)
(require 'xml)

(defvar iweblog-rss-template
  '(rss ((version . "2.0"))
        (channel nil
                 (title nil "Iacob的网志")
                 (description nil "Iacob的网志")
                 (link nil "https://iacob.github.io")))
  "RSS template")

(defvar iweblog-base-uri "https://iacob.github.io" "Base URI")

(defun iweblog-convert-list-to-rss ()
  (let (posts-list-text
        posts-list
        rss-list
        (rss-full (copy-tree iweblog-rss-template))
        fn-htmlfile)

    (setq fn-htmlfile (lambda (file)
                        (concat (car (split-string file "\\.")) "." "html")))

    (with-temp-buffer
      (insert-file-contents "list.yml")
      (setq posts-list-text
            (buffer-substring-no-properties (point-min) (point-max)))
      (setq posts-list (yaml-parse-string posts-list-text)))
    
    (dolist (post (seq-into posts-list 'list))
      (add-to-list 'rss-list
                   (list 'item nil
                         (list 'title nil (gethash 'title post))
                         (list 'link nil
                               (concat iweblog-base-uri "/"
                                       (funcall fn-htmlfile
                                                (gethash 'file post))))
                         (list 'pubDate nil (gethash 'time post))
                         (list 'description))
                   't))


    (let (channel-items)
      (setq channel-items (cdr (nth 2 rss-full)))
      (setcdr (nth 2 rss-full) (append channel-items rss-list)))


    (with-temp-buffer
      (xml-print (list rss-full))
      (write-file "output/rss.xml" 't))))


(defun iweblog-refresh ()
  (interactive)
  (when (not (file-directory-p "output"))
    (mkdir "output"))
  (iweblog-convert-list-to-rss))

;; (let (article-buffer)
;;   (save-window-excursion
;;     (setq article-buffer (find-file "index.org"))
;;     (org-html-export-as-html)
;;     (switch-to-buffer "*Org HTML Export*")
;;     (write-file "1.html" 't)
;;     (kill-buffer)
;;     (kill-buffer article-buffer)
;;     )
;;   )
