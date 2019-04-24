;;; haikunator.el --- Heroku-like random name generator.  -*- lexical-binding: t; -*-

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

;; Adapted from URL `https://github.com/usmanbashir/haikunator'.
;; 
;; Other idea
;; https://github.com/aceakash/project-name-generator#readme

;;; Code:

(defconst haikunator-adjectives
  (eval-when-compile
    (vconcat
     (split-string
      "autumn hidden bitter misty silent empty dry dark summer
        icy delicate quiet white cool spring winter patient
        twilight dawn crimson wispy weathered blue billowing
        broken cold damp falling frosty green long late lingering
        bold little morning muddy old red rough still small
        sparkling throbbing shy wandering withered wild black
        young holy solitary fragrant aged snowy proud floral
        restless divine polished ancient purple lively nameless"))))

(defconst haikunator-nouns
  (eval-when-compile
    (vconcat
     (split-string
      "waterfall river breeze moon rain wind sea morning
        snow lake sunset pine shadow leaf dawn glitter forest
        hill cloud meadow sun glade bird brook butterfly
        bush dew dust field fire flower firefly feather grass
        haze mountain night pond darkness snowflake silence
        sound sky shape surf thunder violet water wildflower
        wave water resonance sun wood dream cherry tree fog
        frost voice paper frog smoke star"))))


(defun haikunator-insert ()
  (interactive)
  (let ((adjective (seq-random-elt haikunator-adjectives))
        (noun (seq-random-elt haikunator-nouns))
        (token (format "%04d" (random 10000))))
    (if (derived-mode-p 'python-mode)
        (if (string=
             "class"
             (string-trim
              (buffer-substring-no-properties
               (line-beginning-position) (line-end-position))))
            (insert (format "%s-%s" (capitalize adjective) (capitalize noun)))
          (insert "%s_%s_%s"
                  (capitalize adjective)
                  (capitalize noun)
                  token))
      (insert (format "%s-%s-%s" adjective noun token)))))

(provide 'haikunator)
;;; haikunator.el ends here
