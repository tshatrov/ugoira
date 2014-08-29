;;;; ugoira.lisp

(in-package #:ugoira)

(defparameter *suffixes* '("1920x1080"))

(defparameter *out-dir* #P"d:/art/out/")

(defun download-with-ref (out url referer)
  (multiple-value-bind (content code) (drakma:http-request url :additional-headers `(("Referer" . ,referer)))
    (when (= code 200)
      (with-open-file (stream out :direction :output
                              :if-exists :supersede
                              :element-type :default)
        (write-sequence content stream)))
    code))

(defun get-file-name (url)
  (let* ((pos (position #\/ url :from-end t))
         (fname (subseq url (1+ pos))))
    fname))

(defun download-ugoira (id)
  (let* ((ref-url (format nil "http://www.pixiv.net/member_illust.php?mode=medium&illust_id=~a" id))
         (main-content (webgunk:http-request ref-url))
         (ugoira-url (delete #\\ (ppcre:scan-to-strings "http:.*img-zip-ugoira.*\\.zip" main-content)))
         (ugoira-base (ppcre:regex-replace "\\d+x\\d+\\.zip$" ugoira-url ""))
         (urls (mapcar (lambda (s) (concatenate 'string ugoira-base s ".zip")) *suffixes*)))
    (push ugoira-url urls)
    (loop for url in urls
         for fname = (merge-pathnames *out-dir* (get-file-name url))
         do (format t "Downloading ~a...~%" url) (force-output)
         do (let ((code (download-with-ref fname url ref-url)))
              (if (= code 200)
                  (format t "~a~%" fname)
                  (format t "Error: ~a~%" code))))))

(defun download-manga (id)
  (let* ((manga-url (format nil "http://www.pixiv.net/member_illust.php?mode=manga&illust_id=~a" id))
         (manga-content (webgunk:parse-url manga-url)))
    (loop for img in (css:query "img[data-filter=\"manga-image\"]" manga-content)
          for img-url = (dom:get-attribute img "data-src")
          for url = (ppcre:regex-replace "_p\\d*\\." img-url "_big\\&")
          do (format t "Downloading ~a...~%" url) (force-output)
          do (download-with-ref (merge-pathnames *out-dir* (get-file-name url)) url manga-url))))


(defun download-image (id)
  (let* ((ref-url (format nil "http://www.pixiv.net/member_illust.php?mode=medium&illust_id=~a" id))
         (main-content (webgunk:parse-url ref-url))
         (link-href (dom:get-attribute (car (css:query "div.img-container > a" main-content)) "href")))
    (cond ((css:query "div.img-container a.full-screen" main-content)
           (download-ugoira id))
          ((search "mode=manga" link-href)
           (download-manga id))
          (t (let* ((img-url (dom:get-attribute (car (css:query "div.img-container img" main-content)) "src"))
                    (url (ppcre:regex-replace "_.\\." img-url ".")))
               (format t "Downloading ~a...~%" url) (force-output)
               (download-with-ref (merge-pathnames *out-dir* (get-file-name url)) url ref-url))))))
         

