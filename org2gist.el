;;; org2gist --- Easily share an org subtree as a gist

;;; Commentary:
;;; Often, I find myself wanting to share a subtree from my notes file,
;;; publicly.  It is convenient to use gists for this, since GitHub renders the
;;; org syntax correctly.  This package makes it easy to do that.

;;; Code:

(require 'gist)
(require 'ox-hugo) ;; FIXME: Remove dependency in favor of other slug function

(defun org-subtree-to-gist-dwim ()
  "Post or update current org subtree as a gist."
  (interactive)
  (save-window-excursion
    (save-excursion
      (let* ((org-export-with-toc nil)
             (org-export-with-sub-superscripts '{})
             (switch-to-buffer-preserve-window-point t)
             (gist-id (org-entry-get (point) "GIST_ID"))
             (title (org-get-heading t t t t))
             (filename (format "%s.org" (org-hugo-slug title)))
             (content-buffer (current-buffer))
             (export-buffer (org-org-export-as-org nil t nil t))
             gist)
        (if (null gist-id)
            (flet ((gist-ask-for-description-maybe () ((lambda () title))))
              (rename-buffer filename)
              (gist-region (point-min) (point-max) t)
              (switch-to-buffer content-buffer)
              (org-set-property "GIST_ID" (car (last (split-string (current-kill 0 t) "/")))))
          (progn
            (gist-fetch gist-id)
            (replace-buffer-contents export-buffer)
            (gist-mode-edit-buffer filename)
            (setq gist (gist-list-db-get-gist gist-id))
            (kill-new (oref gist :html-url))
            ;; Edit description, if required
            (unless (string= title (oref gist :description))
              (let ((api (gist-get-api t))
                    (g (clone gist
                              :files nil
                              :description title)))
                (gh-gist-edit api g)))))
        (kill-buffer export-buffer)
        (message (format "Gist URL: %s (copied to clipboard)" (car kill-ring)))))))

(provide 'org2gist)
;;; org2gist.el ends here
