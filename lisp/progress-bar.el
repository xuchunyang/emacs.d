;;; progress-bar.el --- Progress Bar                 -*- lexical-binding: t; -*-

;; Copyright (C) 2017, 2018  Xu Chunyang

;; Author: Xu Chunyang <mail@xuchunyang.me>
;; Keywords:

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Ruby:
;; - https://github.com/jfelchner/ruby-progressbar
;; - https://github.com/paul/progress_bar

;;; Code:

(defun progress-bar-test ()
  "Draw a progress bar in the Echo Area."
  (interactive)
  (let ((i 0)
        (t0 (current-time)))
    (while (<= i 100)
      (let (message-log-max)
        (message "Time: %s [%-100s] %3d%% Progress"
                 (format-time-string "%T" (time-subtract (current-time) t0))
                 (make-string i ?=) i))
      (sit-for (/ (1+ (random 10)) 100.0))
      (setq i (1+ i)))))

(provide 'progress-bar)
;;; progress-bar.el ends here
