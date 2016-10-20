(require 'url)
(require 'json)


(defun retrieve-from-json-url (url)
  (with-current-buffer
      (url-retrieve-synchronously url)
    (goto-char (+ 1 url-http-end-of-headers))
    (json-read-object)
    ))

(defun airbrake-projects (user-key)
  (retrieve-from-json-url (format "https://airbrake.io/api/v3/projects?key=%s" user-key))
  )

(defun airbrake-groups (user-key project-id)
  (retrieve-from-json-url (format "https://airbrake.io/api/v3/projects/%s/groups?key=%s" project-id user-key))
  )

(defvar sample-projects (airbrake-projects airbrake-user-id))

(defun format-project-for-display (project)
  (cdr (assoc 'name project)))

(defun helm-airbrake-project-search ()
  (mapcar (lambda (project)
            (cons (format-project-for-display project)
                  project))
          (cdr (assoc 'projects sample-projects))))

(defun formatted-errors (errors)
  (format "Errors placeholder")
  )

(defun formatted-group (group)
  (format "--------------\n Id: %s \n Resolved: %s \n Errors: %s \n" (cdr (assoc 'id group))  (cdr (assoc 'resolved group)) (formatted-errors (cdr (assoc 'errors group))))
  )

(defun helm-airbrake-project-show (project)
  (switch-to-buffer (get-buffer-create (cdr (assoc 'name project))))
  (erase-buffer)
  (insert (format "Recent groups for project: %s \n\n" (cdr (assoc 'name project))))
  (mapcar (lambda (group)
            (insert (formatted-group group)))
          (cdr (assoc 'groups (airbrake-groups airbrake-user-id (cdr (assoc 'id project)))))))

(defvar helm-source-airbrake-project-list
  '((name . "Airbrake Projects")
    (candidates . helm-airbrake-project-search)
    (action . (("Go" . helm-airbrake-project-show)))))

(helm :sources helm-source-airbrake-project-list)
