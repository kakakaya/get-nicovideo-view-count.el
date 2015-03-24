;;; get-nicovideo-view-count.el --- Get nicovideo's view count in Emacs(originally from http://bakedroy.hatenablog.com/entry/2015/03/22/033247).

;; Copyright (C) 2015  kakakaya

;; Author: kakakaya <kakakaya AT gmail.com>
;; Keywords: games, abbrev

;; Licence:
;; The MIT License (MIT)

;; Copyright (c) 2015 kakakaya

;; Permission is hereby granted, free of charge, to any person obtaining a copy
;; of this software and associated documentation files (the "Software"), to deal
;; in the Software without restriction, including without limitation the rights
;; to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
;; copies of the Software, and to permit persons to whom the Software is
;; furnished to do so, subject to the following conditions:

;; The above copyright notice and this permission notice shall be included in all
;; copies or substantial portions of the Software.

;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
;; IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
;; FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
;; AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
;; LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
;; OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
;; SOFTWARE.

;;; Commentary:

;; Usage:
;; (require 'get-nicovideo-view-count)
;; (get-nicovideo-view-count-by-id 1397552685)
;; => gochiusa ep01's views count
;; M-x get-nicovideo-view-count-by-id RET 1398130645 RET
;; => gochiusa ep02's views count
;; (get-nicovideo-view-count-by-gochiusa-ep 3)
;; => gochiusa ep03's views count
;; C-u M-x get-nicovideo-view-count-by-gochiusa-ep RET
;; => gochiusa ep04's views count

;;; Code:
(require 'xml)
(require 'url-http)

(defun get-nicovideo-view-count-by-id (id)
  "get view count by Nicovideo's video id."
  (interactive "nEnter nicovideo ID to get view count>")
  (message (format "Retrieving: http://ext.nicovideo.jp/api/getthumbinfo/%d" id))
  (setq nicovideo-xml
        (let ((url-request-method "GET")
              (url-request-extra-headers '(("content-type" . "text/xml")))
              (buffer (url-retrieve-synchronously (format "http://ext.nicovideo.jp/api/getthumbinfo/%d" id))))
          (save-excursion
            (set-buffer buffer)
            (goto-char (point-min))
            (concat (buffer-substring-no-properties (point) (point-max))))))
  (string-match "<view_counter>\\(.*\\)</view_counter>" nicovideo-xml)
  (match-string 1 nicovideo-xml))

(defun get-nicovideo-view-count-by-gochiusa-ep (epnum)
  "get view count by Gochiusa's ep number."
  (interactive "nGochiusa EP number>")
  (get-nicovideo-view-count-by-id
   (nth (- epnum 1) '(1397552685            ;01
                      1398130645            ;02
                      1398329907            ;03
                      1399000431            ;04
                      1399877907            ;05
                      1400488944            ;06
                      1401083851            ;07
                      1401697707            ;08
                      1401697707            ;09
                      1402900128            ;10
                      1403585196            ;11
                      1404190412            ;12
                      ))))

(provide 'get-nicovideo-view-count)
;;; get-nicovideo-view-count.el ends here
