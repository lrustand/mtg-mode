;;; mtg.el --- Magic: the Gathering decklist and card search    -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Lars Rustand.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; if not, write to the Free Software
;; Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA 02110-1301 USA

;; Author: Lars Rustand
;; URL: https://github.com/lrustand/mtg-mode
;; Version: 0
;; Package-Requires: ((emacs "29.1"))

;;; Commentary:

;;; Change Log:

;;; Code:

(require 'cl-lib)
(require 'url)
(require 'json)
(require 'mtg-scryfall)

;; TODO: Moxfield GET and POST decklist
;; TODO: Register card collection
;; TODO: Create git repo for decklists
;; TODO: Make mechanism for setting game format of a decklist file.
;;       Could be based on:
;;       - file/dir-local variables
;;       - Special syntax
;;       - File/directory name/suffix
;; TODO: Implement automatic filtering of search queries for format legality and color identity.
;; IDEA: (de)/serialize Scryfall query syntax to/from sexps? Could make local implementation

(defvar mtg-data-dir (concat user-emacs-directory "mtg/")
  "Directory where to store card data.")

(defvar mtg-image-dir (concat mtg-data-dir "images/")
  "Directory where to store card images.")

(defvar mtg-mana-symbol-dir (concat mtg-data-dir "mana-symbols/")
  "Directory to store cached mana symbol images.")

(defvar mtg-oracle-json-file (concat mtg-data-dir "oracle_data.json")
  "The JSON file containing the oracle card data.")

(defvar mtg--last-oracle-update
  (or (file-attribute-modification-time (file-attributes mtg-oracle-json-file)) -1)
  "Timestamp of when the local oracle data was updated.")

(defvar mtg-card-names nil
  "List of Magic: The Gathering card names for completion.")

(defvar mtg-oracle-data nil
  "A JSON file containing one Scryfall card object for each Oracle ID on Scryfall.
The chosen sets for the cards are an attempt to return the most up-to-date recognizable version of the card.")

(defun mtg--oracle-outdated-p ()
  "Return non-nil if the local oracle data is older than 24 hours."
  (let ((age (time-convert (time-since mtg--last-oracle-update)
                           'integer))
        (max-age (* 24 60 60)))
    (> age max-age)))

(defun mtg-load-bulk-data ()
  "Load the bulk oracle data."
  (unless mtg-oracle-data
    (when (mtg--oracle-outdated-p)
        (mtg-scryfall-download-bulk-data))
    (let ((json-array-type 'list))
      (setq mtg-oracle-data (json-read-file mtg-oracle-json-file)))))

(defun mtg-card-names ()
  "Return a list of all card anmes."
  (or mtg-card-names
      (setq mtg-card-names (mtg-fetch-card-names))))

(defun mtg-lookup-card-name (card-name)
  "Lookup oracle data for CARD-NAME."
  (mtg-load-bulk-data)
  (cl-find card-name mtg-oracle-data
           :key (lambda (obj) (cdr (assoc 'name obj)))
           :test #'equal))


(provide 'mtg)
