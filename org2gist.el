;;; org2gist.el --- Easily share an org subtree as a gist

;; Copyright (C) 2022 Puneeth Chaganti

;; Author: Puneeth Chaganti <punchagan+emacs@muse-amuse.in>
;; Created: 2022 March 08
;; Version: 0.2
;; Package-Requires: ((emacs "26.1") (gist "1.4.0") (s "1.12.0"))
;; Keywords: org, lisp, gist, github
;; URL: https://github.com/punchagan/org2gist/

;; This file is not part of GNU Emacs.

;; This program is free software: you can redistribute it and/or modify
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

;; Often, I find myself wanting to share a subtree from my notes file,
;; publicly.  It is convenient to use gists for this, since GitHub renders the
;; org syntax correctly.  This package makes it easy to do that.

;;; Code:

(require 'gist)
(require 's)
(require 'org)

(defun org2gist-subtree-dwim (&optional public)
  "Post or update current org subtree as a gist.

If PUBLIC is non-nil, the gist is posted as a public gist.  Call
the function with a prefix arg (`universal-argument') to post a
public gist.  NOTE: The argument only works for new gists.  It
doesn't toggle the public/private status when editing gists."

  (interactive "p")
  (save-window-excursion
    (save-excursion
      (let* ((org-export-with-toc nil)
             (org-export-with-sub-superscripts '{})
             (switch-to-buffer-preserve-window-point t)
             (gist-id (org-entry-get-with-inheritance "GIST_ID"))
             (_ (and gist-id (goto-char (org-find-property "GIST_ID" gist-id))))
             (title (org-get-heading t t t t))
             (filename (format "%s.org" (s-dashed-words title)))
             (content-buffer (current-buffer))
             (export-buffer (org-org-export-as-org nil t nil t))
             gist)
        (if (null gist-id)
            (cl-letf (((symbol-function 'gist-ask-for-description-maybe) (lambda () title)))
              (rename-buffer filename)
              (gist-region (point-min) (point-max) (= public '1))
              (switch-to-buffer content-buffer)
              (org-set-property "GIST_ID" (car (last (split-string (current-kill 0 t) "/")))))
          (gist-fetch gist-id)
          (replace-buffer-contents export-buffer)
          (gist-mode-edit-buffer filename)
          (setq gist (gist-list-db-get-gist gist-id))
          (kill-new (slot-value gist 'html-url))
          ;; Edit description, if required
          (unless (string= title (slot-value gist 'description))
            (let ((api (gist-get-api t))
                  (g (clone gist
                            :files nil
                            :description title)))
              (gh-gist-edit api g))))
        (kill-buffer export-buffer)
        (let ((url (car kill-ring)))
          (message "Gist URL: %s (copied to clipboard)" (car kill-ring))
          url)))))

(provide 'org2gist)
;;; org2gist.el ends here
