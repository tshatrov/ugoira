;;;; ugoira.lisp

(in-package #:ugoira)

(defparameter *suffixes* '("1920x1080"))

(defparameter *out-dir* #P"d:/art/out/")

(defparameter *image-extensions* '("png" "jpg" "gif" "jpeg"))

(defun download-with-ref (out url referer)
  (multiple-value-bind (content code) (drakma:http-request url :additional-headers `(("Referer" . ,referer)))
    (when (= code 200)
      (with-open-file (stream out :direction :output
                              :if-exists :supersede
                              :element-type :default)
        (write-sequence content stream)))
    code))

(defun get-original-url (img-url)
  (if (search "img-master" img-url)
      (values (ppcre:regex-replace "(/c/[^/]+)?/img-master/"
                                   (ppcre:regex-replace "_[^_]+\\..*$" img-url ".")
                                   "/img-original/")
              t)
      (values img-url nil)))

(defun download-with-extensions (url ref-url)
  (multiple-value-bind (url with-extensions) (get-original-url url)
    (loop for extension in (if with-extensions *image-extensions* '(""))
       for iurl = (concatenate 'string url extension)
       do
         (format t "Downloading ~a...~%" iurl) (force-output)
         (let ((code (download-with-ref (merge-pathnames *out-dir* (webgunk:get-url-file-name iurl)) iurl ref-url)))
           (when (= code 200) (loop-finish))))))

(defun download-url (url &optional (ref-url "http://www.pixiv.net/"))
  (let ((fname (merge-pathnames *out-dir* (webgunk:get-url-file-name url))))
    (format t "Downloading ~a...~%" url) (force-output)
    (let ((code (download-with-ref fname url ref-url)))
      (if (= code 200)
          (format t "~a~%" fname)
          (format t "Error: ~a~%" code)))))

(defun download-ugoira (id)
  (let* ((ref-url (format nil "http://www.pixiv.net/member_illust.php?mode=medium&illust_id=~a" id))
         (main-content (webgunk:http-request ref-url))
         (ugoira-url (delete #\\ (ppcre:scan-to-strings "https?:.*img-zip-ugoira.*\\.zip" main-content)))
         (ugoira-base (ppcre:regex-replace "\\d+x\\d+\\.zip$" ugoira-url ""))
         (urls (mapcar (lambda (s) (concatenate 'string ugoira-base s ".zip")) *suffixes*)))
    (push ugoira-url urls)
    (loop for url in urls do (download-url url ref-url))))

(defun download-manga (id)
  (let* ((manga-url (format nil "http://www.pixiv.net/member_illust.php?mode=manga&illust_id=~a" id))
         (manga-content (webgunk:parse-url manga-url)))
    (loop for img in (css:query "img[data-filter=\"manga-image\"]" manga-content)
          for img-url = (dom:get-attribute img "data-src")
          for url = (ppcre:regex-replace "_p\\d*\\." img-url "_big\\&")
         do (download-with-extensions url manga-url))))

(defun download-image (id)
  (let* ((ref-url (format nil "http://www.pixiv.net/member_illust.php?mode=medium&illust_id=~a" id))
         (main-content (webgunk:parse-url ref-url))
         (link-href (dom:get-attribute (car (css:query "div.img-container > a" main-content)) "href")))
    (cond ((css:query "div.img-container a.full-screen" main-content)
           (download-ugoira id))
          ((search "mode=manga" link-href)
           (download-manga id))
          (t (let* ((img-url (dom:get-attribute (car (css:query "div.img-container img" main-content)) "src")))
               (download-with-extensions img-url ref-url))))))


;; instagram

(defun parse-instagram-script (text)
  (let* ((json (jsown:parse (string-trim "; " (second (ppcre:split " = " text :limit 2)))))
         (mm (car (webgunk:jsown-filter json "entry_data" "PostPage" "graphql" "shortcode_media"))))
    (if (jsown:keyp mm "edge_sidecar_to_children")
        (webgunk:jsown-filter mm "edge_sidecar_to_children" "edges" "node" "display_url")
        (list (jsown:val mm "display_url")))))

(defun download-instagram* (url ref-url)
  (format t "Downloading ~a...~%" url)
  (let ((fname (merge-pathnames *out-dir* (webgunk:get-url-file-name url))))
    (download-with-ref fname url ref-url)))

(defun download-instagram (id-or-url)
  (let* ((url (if (alexandria:starts-with-subseq "http" id-or-url) id-or-url
                  (format nil "https://www.instagram.com/p/~a/" id-or-url)))
         (content (webgunk:parse-url url))
         (scripts (dom:get-elements-by-tag-name content "script")))
    (dom:do-node-list (script scripts)
      (let ((nt (webgunk:node-text script)))
        (when (search "display_url" nt)
          (dolist (download-url (parse-instagram-script nt))
            (download-instagram* download-url url))
          (return))))))

;; 4chan

(defun get-title (node)
  (let ((title (dom:get-attribute node "title")))
    (unless (zerop (length title)) title)))

(defun download-storytime (url)
  (let* ((content (webgunk:parse-url url))
         (links (css:query "div.fileText a" content))
         (thread-id (aref (nth-value 1 (ppcre:scan-to-strings "thread/(\\d+)" url)) 0))
         (thread-dir (uiop/pathname:ensure-directory-pathname (merge-pathnames *out-dir* thread-id))))
    (ensure-directories-exist thread-dir)
    (loop for link in links
       for href = (format nil "https:~a" (dom:get-attribute link "href"))
       for orig-name = (or (get-title link)
                           (get-title (dom:parent-node link))
                           (webgunk:node-text link))
       for fid = (pathname-name (webgunk:get-url-file-name href))
       for fname = (merge-pathnames thread-dir (uiop:parse-native-namestring (format nil "~a ~a" fid orig-name)))
       do (format t "Downloading ~a to ~a...~%" href fname)
         (download-with-ref fname href url))))
