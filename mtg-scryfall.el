(require 'url)
(require 'json)
(require 'cl-lib)
(require 'mtg)


;;;; General purpose functions

(defun mtg-scryfall-api-get (url)
  "Fetch a Scryfall API endpoint."
  (let* ((buffer (url-retrieve-synchronously (concat "https://api.scryfall.com" url)))
         (json-array-type 'list)
         json-data)
    (with-current-buffer buffer
      (goto-char (point-min))
      (re-search-forward "^$")
      (setq json-data (json-read)))
    (kill-buffer buffer)
    json-data))

(defun mtg-scryfall-api-search (query)
  "Search the Scryfall API."
  (let* ((url (format "/cards/search?q=%s" query))
         (res (mtg-scryfall-api-get url))
         (cards (cdr (assoc 'data res))))
    cards))


;;;; Specific downloaders

(defun mtg-scryfall-download-mana-symbol (symbol)
  "Download a single mana symbol image."
  (let* ((symbol (string-replace "/" "" symbol))
         (url (format "https://svgs.scryfall.io/card-symbols/%s.svg" symbol))
         (local-filename (concat mtg-mana-symbol-dir symbol ".svg")))
    (condition-case nil
        (progn
          (unless (file-exists-p local-filename)
            (make-directory (file-name-directory local-filename) t)
            (url-copy-file url local-filename t))
          local-filename)
      (error nil))))


;;;; Bulk data downloaders

(defun mtg-scryfall-fetch-card-names ()
  "Fetch a list of Magic: The Gathering card names from Scryfall API."
  (let ((resp (mtg-scryfall-api-get "/catalog/card-names")))
    (cdr (assoc 'data resp))))

(defun mtg-scryfall--fetch-bulk-data-index ()
  "Fetch the index of the daily updated bulk data."
  (let ((resp (mtg-scryfall-api-get "/bulk-data")))
    (cdr (assoc 'data resp))))

(defun mtg-scryfall-download-bulk-data ()
  "Download the latest bulk oracle data."
  (let* ((index (mtg-scryfall--fetch-bulk-data-index))
         (oracle (cl-find "oracle_cards" index
                          :key (lambda (obj) (cdr (assoc 'type obj)))
                          :test #'equal)))
    (let-alist oracle
      (let ((buffer (url-retrieve-synchronously .download_uri)))
        (with-current-buffer buffer
          (make-directory mtg-data-dir t)
          (goto-char (point-min))
          (write-region (re-search-forward "^$") (point-max)
                        (concat mtg-data-dir "oracle_data.json")
                        nil 'novisit))))))


(provide 'mtg-scryfall)
