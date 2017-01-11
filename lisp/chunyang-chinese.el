;;; chunyang-chinese.el --- Chinese (中文) Supports  -*- lexical-binding: t; -*-

;; Copyright (C) 2017  Chunyang Xu

;; Author: Chunyang Xu <mail@xuchunyang.me>
;; Keywords: convenience

;;; Commentary:

;;

;;; Code:

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

(provide 'chunyang-chinese)
;;; chunyang-chinese.el ends here
