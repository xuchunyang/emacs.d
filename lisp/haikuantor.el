;;; haikuantor.el --- Heroku-like random name generator.  -*- lexical-binding: t; -*-

;; Copyright (C) 2019  Xu Chunyang

;; Author: Xu Chunyang <mail@xuchunyang.me>

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; Other idea
;; https://github.com/aceakash/project-name-generator#readme

;;; Code:

;;;###autoload
(defun haikunator ()
  "Adapted from URL `https://github.com/usmanbashir/haikunator'."
  (interactive)
  (let ((adjectives
         (eval-when-compile
           (split-string
            "autumn hidden bitter misty silent empty dry dark summer
        icy delicate quiet white cool spring winter patient
        twilight dawn crimson wispy weathered blue billowing
        broken cold damp falling frosty green long late lingering
        bold little morning muddy old red rough still small
        sparkling throbbing shy wandering withered wild black
        young holy solitary fragrant aged snowy proud floral
        restless divine polished ancient purple lively nameless")))
        (nouns
         (eval-when-compile
           (split-string
            "waterfall river breeze moon rain wind sea morning
        snow lake sunset pine shadow leaf dawn glitter forest
        hill cloud meadow sun glade bird brook butterfly
        bush dew dust field fire flower firefly feather grass
        haze mountain night pond darkness snowflake silence
        sound sky shape surf thunder violet water wildflower
        wave water resonance sun wood dream cherry tree fog
        frost voice paper frog smoke star"))))
    (let ((name (format "%s-%s-%04d"
                        (seq-random-elt adjectives)
                        (seq-random-elt nouns)
                        (random 10000))))
      (condition-case nil
          (insert name)
        ;; eval: Buffer is read-only: #<buffer *Messages*>
        ;; eval: Text is read-only
        (error
         (kill-new name)
         (message "Copied: %s" name))))))

(provide 'haikuantor)
;;; haikuantor.el ends here
