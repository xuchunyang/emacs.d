;;; chunyang-chinese.el --- Chinese (中文) Supports  -*- lexical-binding: t; -*-

;; Copyright (C) 2017, 2018  Xu Chunyang

;; Author: Xu Chunyang <mail@xuchunyang.me>
;; Keywords: convenience

;;; Commentary:

;;

;;; Code:


;;; 中文标点符号

(defvar chunyang-chinese-marks
  '((:句号 "。")
    (:逗号 "，")
    (:顿号 "、")
    (:分号 "；")
    (:双引号 "“”")
    (:单引号 "‘’")
    (:圆括号 "（）")
    (:冒号 "：")
    (:省略号 "……")
    (:感叹号 "！")
    (:破折号 "——")
    (:短横线连接号 "-")
    (:一字连接号 "—")
    (:波浪连接号 "～")
    (:书名号 "《》"))
  "一些常用的标点符号.")

;; NOTE: macOS 下用 Option + Shift + B 输入比较特殊的中文标点
(defun chunyang-chinese-insert-mark ()
  "输入中文标点符号."
  (interactive)
  (require 'helm)
  (helm :sources
        (helm-build-sync-source "中文标点符号"
          :candidates
          (lambda ()
            (loop for (k v) in chunyang-chinese-marks
                  collect (cons (format "%-12s\t%s"
                                        (substring (symbol-name k) 1)
                                        v)
                                v)))
          :action
          (helm-make-actions
           "Insert Mark"
           (lambda (mark)
             "Insert MARK, if there is region, wrap the region with the mark."
             (if (and (= (length mark) 2) (use-region-p))
                 (insert (concat (substring mark 0 1)
                                 (delete-and-extract-region (region-beginning)
                                                            (region-end))
                                 (substring mark 1)))
               (insert mark)))))
        :buffer "*helm 输入中文标点*"))


;;; 中文分词

;; For testing:
;; 如何实现中文取词

(defvar mark-chinese-word--words '("如何" "实现" "中文" "取词"))

(defun mark-chinese-word--substrings (string nth)
  "Return all substring in STRING which contains NTH."
  (let (before
        (before-bound (1+ nth))
        after
        (after-bound (1+ (- (length string) nth)))
        result)
    (setq before 0)
    (while (< before before-bound)
      (setq after 1)
      (while (< after after-bound)
        (push (cons (substring string (- nth before) (+ nth after))
                    (cons before after))
              result)
        (incf after))
      (incf before))
    result))

;; (mark-chinese-word--substrings "中文取词" 2)
;; => (("中文取词" 2 . 2) ("中文取" 2 . 1) ("文取词" 1 . 2) ("文取" 1 . 1) ("取词" 0 . 2) ("取" 0 . 1))

(defun mark-chinese-word ()
  "Mark a Chinese word at point."
  (interactive)
  (let ((str (thing-at-point 'word))
        (nth (- (point) (car (bounds-of-thing-at-point 'word)))))
    (let ((word
           (loop for s in (mark-chinese-word--substrings str nth)
                 when (member (car s) mark-chinese-word--words)
                 return s)))
      (if word
          (progn (set-mark (- (point) (cadr word)))
                 (goto-char (+ (point) (cddr word))))
        (set-mark (point))
        (forward-char 1)))))


;;; Treat "M-。" as "M-."

(defvar chinese-punctuation-alist
  '(("，" . ",")
    ("《" . "<")
    ("。" . ".")
    ("》" . ">")
    ("／" . "/")
    ("？" . "?")
    ("‘" . "'")
    ("’" . "'")
    ("“" . "\"")
    ("”" . "\"")
    ("；" . ";")
    ("：" . ":")
    ("【" . "[")
    ("「" . "{")
    ("】" . "]")
    ("」" . "}")
    ("、" . "\\")
    ("·" . "`")
    ("～" . "~")
    ("！" . "!")
    ("（" . "(")
    ("）" . ")")
    ;; Not working: edmacro-parse-keys: C- must prefix a single character, not ——
    ;; ("——" . "_")
    ))

;; 注意：改了这个值之后，用 `C-M-x' 重新加载 `chinese-punctuation-mode' 方能生效
(defvar chinese-punctuation-mode-map
  (let ((map (make-sparse-keymap)))
    (dolist (prefix '("C-x " "C-c " "C-" "M-"))
      (loop for (i . j) in chinese-punctuation-alist
            for k1 = (concat prefix i)
            for k2 = (concat prefix j)
            do (define-key map (kbd k1)
                 `(lambda ()
                    (interactive)
                    (let ((func (key-binding ,(kbd k2))))
                      (cond ((null func)
                             (call-interactively 'undefined))
                            ((keymapp func)
                             (error "I don't know how to deal with keymap (yet)"))
                            (t
                             (call-interactively func))))))))
    map))

(define-minor-mode chinese-punctuation-mode
  "映射一些含中文全角标点的按键组合至英文，如 \[C-x 。] 至 \[C-x .].

\\{chinese-punctuation-mode-map}"
  :global t
  :lighter " Chinese")


;;; Search Chinese via Pinyin

(defvar chunyang-pinyin-map nil)

(defun chunyang-pinyin-map ()
  (unless chunyang-pinyin-map
    (setq chunyang-pinyin-map
          (with-temp-buffer
            (insert-file-contents "~/src/emacs/leim/MISC-DIC/pinyin.map")
            (goto-char (point-min))
            (re-search-forward "^[^%]")
            (let (al)
              (while (not (eobp))
                (push (split-string (buffer-substring-no-properties
                                     (line-beginning-position)
                                     (line-end-position)))
                      al)
                (forward-line))
              (nreverse al)))))
  chunyang-pinyin-map)

(defun chunyang-pinyin-build-regexp (pinyin)
  (mapconcat (lambda (s)
               (let ((chars (cadr (assoc s (chunyang-pinyin-map)))))
                 (if chars
                     (concat "[" chars "]")
                   (error "不合法的拼音 %s" s))))
             (split-string pinyin)
             ""))

;;;###autoload
(defun chunyang-pinyin-occur (pinyin)
  "用拼音搜索对应的中文."
  (interactive "s拼音 (用空格隔开): ")
  (occur (chunyang-pinyin-build-regexp pinyin)))

(provide 'chunyang-chinese)
;;; chunyang-chinese.el ends here
