;;; vm.el --- Simple Virtual Machine                 -*- lexical-binding: t; -*-

;; Copyright (C) 2017  Xu Chunyang

;; Author: Xu Chunyang <mail@xuchunyang.me>
;; Keywords: lisp

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

;; [[https://github.com/felixangell/mac][felixangell/mac: virtual machine in c]]
;;
;; Instructions:
;;
;; | op  | usage   | function                    |
;; |-----+---------+-----------------------------|
;; | PSH | psh val | push <val> to stack         |
;; | POP | pop     | pop value from stack        |
;; | add | add     | add top two values on stack |
;;
;; Example:
;;    (vm '(PSH 5 PSH 6 ADD POP)) => 11
;;    (vm '(PSH 1 PSH 2 PSH 3 ADD ADD POP)) => 6

;; TODO: Implement the improved version (more instructions and registers)

;;; Code:

(defvar vm-instruction-set '(PSH ADD POP))

(defvar vm-program nil)

(defvar vm-stack nil)

(defun vm-eval (instruction)
  (case instruction
    (PSH (push (pop vm-program) vm-stack))
    (POP (pop vm-stack))
    (ADD (push (+ (pop vm-stack) (pop vm-stack)) vm-stack))
    (t (error "Unknown instruction %s" instruction))))

(defun vm (program)
  (let ((vm-program program)
        vm-stack
        rtv)
    (while vm-program
      (setq rtv (vm-eval (pop vm-program))))
    rtv))

(provide 'vm)
;;; vm.el ends here
