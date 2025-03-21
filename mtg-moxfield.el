(require 'mtg)
(require 'json)
(require 'url)


(defun mtg-moxfield-api--ensure-absolute (url)
  "Prefix URL with Moxfield domain if URL is relative."
  (let ((base "https://api.moxfield.com/v2"))
    (if (string-prefix-p base url)
        url
      (concat base url))))

(defun mtg-moxfield-api-get (url)
  "Fetch a Moxfield API endpoint.
URL can be either absolute or relative."
  (let* ((url (mtg-moxfield-api--ensure-absolute url))
         (buffer (url-retrieve-synchronously url))
         (json-array-type 'list)
         json-data)
    (with-current-buffer buffer
      (goto-char (point-min))
      (re-search-forward "^$")
      (setq json-data (json-read)))
    (kill-buffer buffer)
    json-data))

(defun mtg-moxfield-list-decks (user)
  "List decks by USER."
  (let ((url (format "/users/%s/decks?pageNumber=1&pageSize=9999" user)))
    (mapcar (lambda (deck)
              (let-alist deck
                (cons .publicId .name)))
            (alist-get 'data
                       (mtg-moxfield-api-get url)))))

(defun mtg-moxfield-get-deck (id)
  "Get deck with given ID."
  (mtg-moxfield-api-get (format "/decks/all/%s" id)))


(defun mtg-moxfield-deck-to-decklist (deck)
  "Convert a Moxfield JSON deck to a plaintext decklist."
  (let* ((mainboard (alist-get 'mainboard deck)))
    (string-join
     (mapcar (lambda (card)
               (let-alist (cdr card)
                 (format "%d %s" .quantity .card.name)))
             mainboard)
     "\n")))



;;;; Convenience functions for opening things in browser

(defun mtg-moxfield-browse-user (user)
  "Browse USER on Moxfield."
  (browse-url (format "https://moxfield.com/users/%s" user)))

(defun mtg-moxfield-browse-deck (id)
  "Browse deck on Moxfield."
  (browse-url (format "https://moxfield.com/decks/%s/edit" id)))

(defun mtg-moxfield-edit-deck (id)
  "Open deck for editing on Moxfield in browser."
  (browse-url (format "https://moxfield.com/decks/%s/edit" id)))



(provide 'mtg-moxfield)
