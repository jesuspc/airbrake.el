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

(defun airbrake-notices (user-key project-id group-id)
  (retrieve-from-json-url (format "https://airbrake.io/api/v3/projects/%s/groups/%s/notices?key=%s" project-id group-id user-key))
  )

(defvar sample-projects (airbrake-projects airbrake-user-id))

(defun format-project-for-display (project)
  (cdr (assoc 'name project)))

(defun helm-airbrake-project-search ()
  (mapcar (lambda (project)
            (cons (format-project-for-display project)
                  project))
          (cdr (assoc 'projects sample-projects))))

(defun formatted-error-name (group)
  (let ((err (elt (cdr (assoc 'errors group)) 0)))
    (format "%s => %s" (cdr (assoc 'type err)) (cdr (assoc 'message err)))))

(defun append-formatted-backtrace-line (host backtrace-line)
  (insert (format "/%s:%s :%d \n" host (cdr (assoc 'file backtrace-line)) (cdr (assoc 'line backtrace-line)))))

(defun append-formatted-stack (notices)
  (let ((backtrace-lines (cdr (assoc 'backtrace (elt (cdr (assoc 'errors (elt notices 0))) 0)))))
    (mapcar (lambda (backtrace-line)
              (append-formatted-backtrace-line (cdr (assoc 'hostname (cdr (assoc 'context (elt notices 0))))) backtrace-line))
            backtrace-lines)))

(defun append-formatted-notices (group)
  (insert "Stack: \n")
  (append-formatted-stack (cdr (assoc 'notices (airbrake-notices airbrake-user-id (cdr (assoc 'projectId group)) (cdr (assoc 'id group)))))))

(defun append-formatted-group (group)
  (insert "----------------------\n")
  (insert (format "%s \n" (formatted-error-name group)))
  (insert (format "ID: %s \n" (cdr (assoc 'id group))))
  (append-formatted-notices group))

(defun helm-airbrake-project-show (project)
  (switch-to-buffer (get-buffer-create (cdr (assoc 'name project))))
  (erase-buffer)
  (insert (format "Recent groups for project: %s \n\n" (cdr (assoc 'name project))))
  (mapcar (lambda (group)
            (append-formatted-group group))
          (cdr (assoc 'groups (airbrake-groups airbrake-user-id (cdr (assoc 'id project)))))))

(defvar helm-source-airbrake-project-list
  '((name . "Airbrake Projects")
    (candidates . helm-airbrake-project-search)
    (action . (("Go" . helm-airbrake-project-show)))))


(helm :sources helm-source-airbrake-project-list)
