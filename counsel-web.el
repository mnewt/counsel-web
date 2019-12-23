;;; counsel-web.el --- Search the Web using Ivy -*- lexical-binding: t -*-

;; Author: Matthew Sojourner Newton
;; Maintainer: Matthew Sojourner Newton
;; Version: "0.1"
;; Package-Requires: ((emacs "25.1") (swiper "0.12.0") (request "0.3.0"))
;; Homepage: https://github.com/mnewt/counsel-web
;; Keywords: convenience hypermedia
;; Prefix: counsel-web


;; This file is not part of GNU Emacs

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; For a full copy of the GNU General Public License
;; see <http://www.gnu.org/licenses/>.


;;; Commentary:

;; Search the web with dynamic suggestions and browse the results -- all from
;; the comfort of Emacs and ivy.

;;; Code:

(require 'dom)
(require 'request)
(require 'counsel)



;;;; Variables

(defgroup counsel-web nil
  "Search the web with Emacs and ivy."
  :group 'counsel
  :prefix "counsel-web-")

(defcustom counsel-web-engine-alist
  '((duckduckgo
     :suggest counsel-web-suggest--duckduckgo
     :search counsel-web-search--duckduckgo)
    (google
     :suggest counsel-web-suggest--google
     :search counsel-web-search--google))
  "Alist of search engine configurations."
  :group 'counsel-web
  :type '(alist :key-type symbol :value-type plist))

(defcustom counsel-web-engine 'duckduckgo
  "The search engine used to provide suggestions and search results.

See `counsel-web-engine-alist' for the possible choices."
  :group 'counsel-web
  :type 'symbol)


(defcustom counsel-web-suggest-action #'counsel-web-search
  "The function used when a suggestion candidate is selected."
  :group 'counsel-web
  :type 'symbol)

(defcustom counsel-web-search-action #'eww
  "The function used when a search candidate is selected."
  :group 'counsel-web
  :type 'symbol)

(defcustom counsel-web-search-alternate-action #'browse-url-default-browser
  "The function used when a search candidate is selected (alternate)."
  :group 'counsel-web
  :type 'symbol)

(defcustom counsel-web-search-dynamic-update nil
  "If non-nil, update search with each change in the minibuffer."
  :group 'counsel-web
  :type 'boolean)

(defcustom counsel-web-thing 'word
  "The type of thing to search for in `counsel-web-thing-at-point'."
  :group 'counsel-web
  :type 'symbol)

(defvar counsel-web-suggest-history nil
  "History for `counsel-web-suggest'.")

(defvar counsel-web-search-history nil
  "History for `counsel-web-search'.")

(defvar counsel-web-search-minibuffer-map
  (let ((map (make-sparse-keymap)))
    (define-key map [remap ivy-alt-done] 'counsel-web-search-refresh)
    map)
  "Keymap added to `counsel-web-search' minibuffers.")

(defvar counsel-web--suggest-function nil
  "The function used to retrieve suggestions.")

(defcustom counsel-web--search-function nil
  "The function used to retrieve search results.")


 ;;;; Functions

(defun counsel-web--format-candidate (text url)
  "Format TEXT and URL as an `ivy-read' candidate."
  (let ((url (url-unhex-string url)))
    (propertize (concat text "\n" (propertize url 'face 'shadow)) 'shr-url url)))

(cl-defun counsel-web--handle-error (&rest args &key error-thrown &allow-other-keys)
  "Handle error from `request' with ARGS.

Display a message with the ERROR-THROWN."
  (message "Web search error: %S" error-thrown))

(cl-defun counsel-web--async-sentinel (&key data &allow-other-keys)
  "Process returned DATA for an asynchronous counsel web request.

Adapted from `counsel--async-sentinel'."
  (when data
    (ivy--set-candidates (ivy--sort-maybe data))
    (when counsel--async-start
      (setq counsel--async-duration
            (time-to-seconds (time-since counsel--async-start))))
    (let ((re (ivy-re-to-str (funcall ivy--regex-function ivy-text))))
      (if ivy--old-cands
          (if (eq (ivy-alist-setting ivy-index-functions-alist)
                  'ivy-recompute-index-zero)
              (ivy-set-index 0)
            (ivy--recompute-index ivy-text re ivy--all-candidates))
        (unless (ivy-set-index
                 (ivy--preselect-index
                  (ivy-state-preselect ivy-last)
                  ivy--all-candidates))
          (ivy--recompute-index ivy-text re ivy--all-candidates))))
    (setq ivy--old-cands ivy--all-candidates)
    (if ivy--all-candidates
        (ivy--exhibit)
      (ivy--insert-minibuffer ""))))

(defun counsel-web--request (url parser &optional placeholder)
  "Search using the given URL and PARSER.

PLACEHOLDER is returned for immediate display by `ivy-read'. The
actual list of candidates is later updated by the \:success
function."
  (if counsel-web-search-dynamic-update
      (progn
        (request
         url
         :headers '(("User-Agent" . "Emacs"))
         :parser parser
         :error #'counsel-web--handle-error
         :success #'counsel-web--async-sentinel)
        (list "" placeholder))
    (let (candidates)
      (request
       url
       :sync t
       :headers '(("User-Agent" . "Emacs"))
       :parser parser
       :error #'counsel-web--handle-error
       :success (cl-function (lambda (&key data &allow-other-keys)
                               (setq candidates data))))
      candidates)))

(defun counsel-web-suggest--collection-function (string)
  "Retrieve search suggestions for STRING."
  (or (ivy-more-chars)
      (funcall counsel-web--suggest-function string)))

(defun counsel-web-search--collection-function (string)
  "Retrieve search results for STRING asynchronously."
  (or (ivy-more-chars)
      (funcall counsel-web--search-function string)))

(defun counsel-web-search--browse-first-result (string)
  "Immediately browse the first result the search for STRING."
  (funcall (counsel-web-search--call-with-url counsel-web-search-action)
           (car (let ((counsel-web-search-dynamic-update nil))
                  (counsel-web-search--collection-function string)))))

(defun counsel-web-search--call-with-url (function)
  "Wrap FUNCTION so it can be used as an action."
  (lambda (candidate)
    (funcall function (get-text-property 0 'shr-url candidate))))

(defun counsel-web-search--call-in-other-window (function)
  "Wrap FUNCTION so it opens in other window."
  (lambda (candidate)
    (other-window 1)
    (funcall (counsel-web-search--call-with-url function) candidate)))

(defun counsel-web-search-refresh ()
  "Perform a new search using the entered text as the input."
  (interactive)
  (ivy-quit-and-run (counsel-web-search ivy-text)))



;;;; Search Engines

(defun counsel-web-suggest--duckduckgo (string)
  "Retrieve search suggestions from DuckDuckGo for STRING."
  (counsel-web--request
   (format "https://ac.duckduckgo.com/ac/?q=%s&amp;type=list"
           (url-hexify-string string))
   (lambda ()
     (mapcar
      (lambda (e) (cdr (car e)))
      (append (json-read) nil)))))

(defun counsel-web-search--duckduckgo (string)
  "Retrieve search results from DuckDuckGo for STRING."
  (counsel-web--request
   (concat "https://duckduckgo.com/html/?q=" (url-hexify-string string))
   (lambda ()
     (mapcar
      (lambda (a)
        (let* ((href (assoc-default 'href (dom-attributes a))))
          (counsel-web--format-candidate
           (dom-texts a)
           (substring href (string-match "http" href)))))
      (dom-by-class (libxml-parse-html-region (point-min) (point-max)) "result__a")))
   "Searching DuckDuckGo..."))

(defun counsel-web-suggest--google (string)
  "Retrieve search suggestions from Google for STRING."
  (counsel-web--request
   (concat "https://suggestqueries.google.com/complete/search?output=firefox&q="
           (url-hexify-string string))
   (lambda () (append (elt (json-read) 1) nil))))

(defun counsel-web-search--google (string)
  "Retrieve search results from Google for STRING."
  (counsel-web--request
   (concat "https://www.google.com/search?q=" (url-hexify-string string))
   (lambda ()
     (cl-loop for a in (dom-by-tag (libxml-parse-html-region (point-min) (point-max)) 'a)
              for href = (assoc-default 'href (dom-attributes a))
              when (string-match "/url\\?q=\\(http[^&]+\\)" href)
              unless (string-match "accounts.google.com" href)
              collect
              (counsel-web--format-candidate
               (dom-texts a)
               (substring (assoc-default 'href (dom-attributes a))
                          (match-beginning 1) (match-end 1)))))
   "Searching Google..."))



;;;; Commands

;;;###autoload
(defun counsel-web-suggest (&optional initial-input prompt suggest-function action)
  "Perform a web search with asynchronous suggestions.

INITIAL-INPUT can be given as the initial minibuffer input.
PROMPT, if non-nil, is passed as `ivy-read' prompt argument.
SUGGEST-FUNCTION, if non-nil, is called to perform the search.
ACTION, if non-nil, is called to load the selected candidate."
  (interactive)
  (let ((counsel-web--suggest-function
         (or suggest-function
             (plist-get (alist-get counsel-web-engine counsel-web-engine-alist)
                        :suggest)))
        (counsel-web-suggest-action (or action counsel-web-suggest-action)))
    (ivy-read (or prompt "Web Search: ")
              #'counsel-web-suggest--collection-function
              :initial-input initial-input
              :dynamic-collection t
              :history 'counsel-web-suggest-history
              :action counsel-web-suggest-action
              :unwind #'counsel-delete-process
              :caller 'counsel-web-suggest)))

(ivy-add-actions
 'counsel-web-suggest
 `(("f" counsel-web-search--browse-first-result "first candidate")))

;;;###autoload
(defun counsel-web-search (&optional string prompt search-function action)
  "Interactively search the web for STRING.

PROMPT, if non-nil, is passed as `ivy-read' prompt argument.
SEARCH-FUNCTION, if non-nil, is called to perform the search.
ACTION, if non-nil, is called to load the selected candidate."
  (interactive)
  (let ((counsel-web--search-function
         (or search-function
             (plist-get (alist-get counsel-web-engine counsel-web-engine-alist)
                        :search)))
        (counsel-web-search-action (or action counsel-web-search-action))
        (string (if counsel-web-search-dynamic-update
                    string
                  (or string
                      (read-string "Web Search: " nil 'counsel-web-search-history)))))
    (ivy-read (or prompt "Browse: ")
              (if counsel-web-search-dynamic-update
                  #'counsel-web-search--collection-function
                (counsel-web-search--collection-function string))
              :initial-input nil
              :dynamic-collection counsel-web-search-dynamic-update
              :require-match t
              :history 'counsel-web-search-history
              :keymap counsel-web-search-minibuffer-map
              :action (counsel-web-search--call-with-url counsel-web-search-action)
              :unwind #'counsel-delete-process
              :caller 'counsel-web-search)))

(ivy-add-actions
 'counsel-web-search
 `(("j"
    ,(counsel-web-search--call-in-other-window counsel-web-search-action)
    "other window")
   ("m"
    ,(counsel-web-search--call-with-url counsel-web-search-alternate-action)
    "alternate browser")))

;;;###autoload
(defun counsel-web-thing-at-point (thing)
  "Interactively search the web for the THING at point."
  (interactive (list (thing-at-point counsel-web-thing)))
  (counsel-web-search thing))

(provide 'counsel-web)

;;; counsel-web.el ends here
