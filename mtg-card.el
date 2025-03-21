(require 'url)
(require 'json)
(require 'mtg)
(require 'mtg-scryfall)


;;;; Card data accessor functions

(defun mtg-card-name (card)
  "Return the name of CARD."
  (alist-get 'name card))

(defun mtg-card-oracle-id (card)
  "Return the oracle ID of CARD."
  (alist-get 'oracle_id card))

(defun mtg-card-oracle-text (card)
  "Return the oracle text of CARD."
  (mtg-replace-mana-symbols
   (alist-get 'oracle_text card)))

(defun mtg-card-cost (card)
  "Return the mana cost of CARD."
  (mtg-replace-mana-symbols
   (alist-get 'mana_cost card)))

(defun mtg-card-type (card)
  "Return the type line of CARD."
  (alist-get 'type_line card))

(defun mtg-card-power (card)
  "Return the power of CARD."
  (alist-get 'power card))

(defun mtg-card-toughness (card)
  "Return the toughness of CARD."
  (alist-get 'toughness card))

(defun mtg-card-power-toughness (card)
  "Return the power and toughness of CARD."
  (let-alist card
    (when (or .power .toughness)
      (concat .power "/" .toughness))))

(defun mtg-card-color (card)
  "Return the colors of CARD."
  (alist-get 'colors card))

(defun mtg-card-identity (card)
  "Return the color identity of CARD."
  (or (alist-get 'color_identity card) ""))

(defun mtg-card-legality (card)
  "Return the legalities of CARD."
  (thread-last card
               (alist-get 'legalities)
               (seq-filter (lambda (f) (string= "legal" (cdr f))))
               (mapcar #'car)))

(defun mtg-card-price (card)
  "Return the price of CARD."
  (let-alist card
    (string-to-number (or .prices.usd "0"))))

(defun mtg-card--image-uri (card &optional image-type)
  "Return the image URIs of CARD."
  (let-alist card
    (let ((uris (or .image_uris
                    (let-alist (car .card_faces)
                      .image_uris))))
      (alist-get (or image-type 'normal) uris))))

(defun mtg-card-image (card)
  "Return the image of CARD"
  (let ((image-file (concat mtg-image-dir (mtg-card-oracle-id card) ".jpg")))
    (unless (file-exists-p image-file)
      (make-directory mtg-image-dir t)
      (url-copy-file (mtg-card--image-uri card) image-file t))
    image-file))

(defun mtg-card-art-image (card)
  "Return the art image of CARD"
  (let-alist card
    (let ((image-file (concat mtg-image-dir .oracle_id "_art.jpg")))
      (unless (file-exists-p image-file)
        (make-directory mtg-image-dir t)
        (url-copy-file (mtg-card--image-uri card 'art_crop) image-file t))
      image-file)))

(defun mtg-card-rulings (card)
  "Return the rulings of CARD.
Returns a list of alists."
  (alist-get 'data
             (mtg-scryfall-api-get
              (alist-get 'rulings_uri card))))


(defun mtg-card-scryfall-uri (card)
  "Return the Scryfall URI of CARD."
  (alist-get 'scryfall_uri card))

(defun mtg-card-scryfall-uri (card)
  "Return the Scryfall URI of CARD."
  (alist-get 'scryfall_uri card))


;;;; Display and formatting functions

(defun mtg--truncate-mana-cost (cost width)
  "Truncate COST to WIDTH."
  (if (length< cost width)
      (let* ((pad-length (- width (length cost)))
             (pad-pixel-width (* (line-pixel-height) pad-length))
             (pad-display-props `(space :width (,pad-pixel-width))))
        (concat cost (propertize " " 'display pad-display-props)))
    (truncate-string-to-width cost width)))


(defun mtg--render-card (card &optional width)
  "Return a propertized string of the card image with metadata."
  (let ((card (if (stringp card)
                  (mtg-lookup-card-name card)
                card)))
    (let-alist card
      (propertize " "
                  'display
                  (create-image (mtg-card-image card) nil nil
                                :ascent 'center
                                :width (or width 200))
                  :oracle-id .oracle_id))))

(defun mtg-replace-mana-symbols (str)
  "Replace mana symbols like {R} with corresponding images."
  (when (stringp str)
    (save-excursion
      (with-temp-buffer
        (insert str)
        (goto-char (point-min))
        (while (re-search-forward "{\\([^}]+\\)}" nil t)
          (when-let* ((mana-symbol (match-string 1))
                      (image-file (mtg-scryfall-download-mana-symbol mana-symbol))
                      (_ (file-exists-p image-file))
                      (image (create-image image-file 'svg nil
                                           :ascent 'center
                                           :height (line-pixel-height)))
                      (image-string (propertize (format " " mana-symbol)
                                                'display image)))
            (replace-match image-string t t)))
        (buffer-string)))))


;;;; Completion

(defun mtg-card-name-completion-at-point ()
  "Completion at point function for Magic: The Gathering card names."
  (unless mtg-card-names
    (setq mtg-card-names (mtg-scryfall-fetch-card-names)))
  (let ((bounds (mtg-bounds-of-card-at-point)))
    (when bounds
      ;; Return a list with start, end, and collection for completion
      ;; Use `all-completions` to provide a list of possible completions
      (list (car bounds) (cdr bounds)
            mtg-card-names
            :annotation-function #'mtg-card-annotation
            :company-doc-buffer #'mtg-doc-buffer))))

(defun mtg-doc-buffer (card-name)
  "Documentation to show in corfu-popupinfo-mode."
  (with-current-buffer (get-buffer-create " *mtg-doc-buffer*" t)
    (erase-buffer)
    (insert (mtg-card-oracle-text (mtg-lookup-card-name card-name)))
    (current-buffer)))

(defun mtg-card-annotation (card-name)
  (mtg-card-type (mtg-lookup-card-name card-name)))


;;;; Helper functions

(defun mtg-bounds-of-card-at-point ()
  "Return bounds of card at point."
  (cons
   (save-excursion
      (beginning-of-line)
      ;; Skip past any numbers and whitespace
      (skip-chars-forward "0-9 ")
      (point))
   (point)))

(defun mtg-card-at-point ()
  "Return MTG card at point."
  (let (card-name
        card)
    (save-excursion
      (beginning-of-line)
      ;; Skip past any numbers and whitespace
      (skip-chars-forward "0-9 ")
      (setq card-name (buffer-substring-no-properties (point) (line-end-position)))
      (setq card (or (get-text-property (point) 'mtg-card)
                     (mtg-lookup-card-name card-name))))))


;;;; Eldoc functions

(defun mtg-card-image-eldoc-function (callback)
  "Return eldoc information for card at point."
  (mtg--render-card (mtg-card-at-point)))

(defun mtg-card-name-eldoc-function (callback)
  "Return eldoc information for card at point."
  (mtg-card-name (mtg-card-at-point)))

(defun mtg-card-text-eldoc-function (callback)
  "Return eldoc information for card at point."
  (mtg-card-oracle-text (mtg-card-at-point)))

(defun mtg-card-all-info-eldoc-function (callback)
  "Return eldoc information for card at point."
  (if-let ((card (mtg-card-at-point)))
      (concat (mtg-card-name card) " " (mtg-card-cost card) "\n"
              (propertize "x" 'display (create-image (mtg-card-art-image card) nil nil
                                                     :ascent 'center
                                                     :width 200))
              "\n"
              (mtg-card-type card) "\n"
              (mtg-card-oracle-text card) "\n"
              (mtg-card-power-toughness card))))

(provide 'mtg-card)
