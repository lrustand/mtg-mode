(require 'mtg)
(require 'mtg-card)

;; Create the syntax table for this mode.
(defvar mtg-deck-mode-syntax-table
  (let ((st (make-syntax-table)))
    (modify-syntax-entry ?\n ">" st)
    (modify-syntax-entry ?\; "<" st)
    st)
  "Syntax table used while in `mtg-deck-mode'.")

(define-derived-mode mtg-deck-mode nil "MTG Decklist"
  "Major mode for editing MTG decklists."
  :syntax-table mtg-deck-mode-syntax-table
  (add-hook 'eldoc-documentation-functions #'mtg-card-all-info-eldoc-function nil t)
  (add-hook 'completion-at-point-functions #'mtg-card-name-completion-at-point nil t)
  (setq-local comment-start ";")
  (setq-local font-lock-defaults '(nil))
  (setq-local indent-line-function #'ignore))

(add-to-list 'auto-mode-alist '("\\.mtg" . mtg-deck-mode))


;;;; Helper functions

(defun mtg-deck-read-decklist ()
  "Read the decklist in the current buffer."
  (let ((decklist (make-hash-table :size 150)))
    (save-excursion
      (goto-char (point-min))
      (while (not (eobp))
        (when-let* ((card (mtg-card-at-point))
                    (id (mtg-card-oracle-id card))
                    (count (or (mtg-deck-count-at-point) 1)))
          (puthash id (+ count (gethash id decklist 0)) decklist))
        (forward-line)))
    decklist))

(defun mtg-deck-count-at-point ()
  "Return the count at the line which contains point."
  (save-excursion
    (beginning-of-line)
    (when (> (skip-chars-forward "0-9") 0)
      (string-to-number
       (buffer-substring-no-properties (line-beginning-position) (point))))))

(defun mtg-deck-unique-cards (deck)
  "Return a list of unique card objects from DECK."
  (mapcar #'mtg-lookup-card-id (hash-table-keys deck)))

;;;; Various deck stats

(defun mtg-deck-card-count (deck)
  "Return the number of cards in DECK."
  (interactive (list (mtg-deck-read-decklist)))
  (message "Count: %d"
  (apply '+ (hash-table-values deck))))

(defun mtg-deck-price (deck)
  "Return the total price of DECK."
  (interactive (list (mtg-deck-read-decklist)))
  (apply '+ (mapcar (lambda (id)
                      (let* ((card (mtg-lookup-card-id id))
                             (price (mtg-card-price card))
                             (count (gethash id deck)))
                      (* price count)))
                    (hash-table-keys deck))))

(defun mtg-deck-legality (deck)
  "Return the legality of DECK."
  (interactive (list (mtg-deck-read-decklist)))
  (reduce #'cl-intersection
          (mapcar #'mtg-card-legality
                  (mtg-deck-unique-cards deck))))

(defun mtg-deck-identity (deck)
  "Return the color identity of DECK."
  (interactive (list (mtg-deck-read-decklist)))
  (thread-last deck
               (mtg-deck-unique-cards)
               (mapcar #'mtg-card-identity)
               (apply #'append)
               (seq-uniq)
               (apply #'concat)))


(provide 'mtg-deck)
