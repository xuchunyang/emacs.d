## 1. `'VAL`

值相等时匹配。

```
(pcase 'foo
  ('foo 123))
;; => 123
```

当 `VAL` 是整数、字符串或关键字，引号可省略。

## 2. `SYMBOL`

匹配一切值，绑定 `SYMBOL`（符号 `_` 除外）

```
(pcase 1
  (x (1+ x)))
;; => 2
```

## 3. `_`

匹配一切值，但不绑定值。

```
(pcase 1
  (_ "hello"))
;; => "hello"
```

## 4. `` ` ``

匹配列表。支持 (elisp) Dotted Pair Notation 指定 cdr。

```
(pcase '(1 2 3 4)
  (`(,a ,_ . ,rest)
   (list :a a
         :rest rest)))
;; => (:a 1 :rest (3 4))
```

## 5. `(guard BOOLEXP)`

匹配 BOOLEXP 为真。

```
(pcase nil
  ((guard (> 2 1)) "二大于一"))
;; => "二大于一"
```

## 6. `(pred FUN)`

匹配 `(fun EXPVAL)` 为真。

```
(pcase "hello"
  ((pred stringp) "this is a string")
  ((pred numberp) "this is a number"))
;; => "this is a string"
```

`FUN` 除了直接为函数名，还可以写成一个列表，当作函数调用，`pcase` 会把 `EXPVAL` 加到最后位置，也就是你写的 `(FUN arg0 arg1 ... argn)` 变成 `(FUN arg0 arg1 ... argn EXPVAL)`

```
(pcase 1
  ((pred (< 0)) "positive"))
;; => "positive"
```

上面 `(< 0)` 变成 `(< 0 1)`。

## 7. `(and PAT...)`

当所有 `PAT` 全匹配时。

```
(pcase "hello"
  ((and (pred stringp)
        s
        (guard (> (length s) 0)))
   (format "%S 是字符串，且非空" s)))
;; => "\"hello\" 是字符串，且非空"
```

这里 `and` 是 `pcase` 模式，不是 Lisp 函数 `and`。`pcase` 模式和函数调用很像，需要区分，比如 `(guard (> (length s) 0))` 是 `pcase` 模式，但里面的 `(> (length s) 0)` 是正常的 Lisp 代码。

`and` 模式里面是三个模式依次匹配，第二个模式符号 `s` 匹配一切并绑定值，类似于 `let*`，后续匹配可以访问。

## 8. `or`

当任意一个 `PAT` 匹配时。

```
(pcase 2
  ((or 1 2 3) "1 to 3"))
;; => "1 to 3"
```

## 9. `(let PAT EXPR)`

当 `PAT` 匹配 `EXPR` 时匹配。类似于在一个 `pcase` 里使用另一个 `pcase`。

```
(pcase 1
  ((and n (let 2 (1+ n)))
   "haha"))
;; => "haha"
```

`2` 是模式 `PAT`，`(1+ n)` 是表达式 `EXPR`。

## 10. `(app FUN PAT)`

`app` 和 `pred` 非常像，区别在于匹配条件，`pred` 在 `FUN` 为真时匹配，而 `app` 在 `FUN` 结果的匹配 `PAT` 时才匹配。

`app` 效果类似于 `pred` + `let` 的组合。

```
(pcase 1
  ((app 1+ 2) 100))
;; => 100
```

## 11. `seq`

`seq` 模式由 `seq.el` 定义（所以需要 `(require 'seq)` 才能用）

```
(pcase '(1 2 3 4)
  ((seq x _ y)
   (list x y)))
;; => (1 3)
```

前面提到过 `` ` `` 模式用 (elisp) Dotted Pair Notation 匹配 cdr，但是 `seq` 不是，它用 `&rest`，和 `cl-destructuring-bind` 一样。

```
(pcase '(1 2 3)
  ((seq a &rest r)
   (list a r)))
;; => (1 (2 3))
```

## 12. `map`

由 `map.el` 定义。

```
(pcase '((a . 1) (b . 2))
  ((map a b) (list a b)))
```

还可以重命名变量：

```
(pcase '((a . 1) (b . 2))
  ((map ('a val-of-a))
   val-of-a))
;; => 1
```

## 13. `rx`

由 `rx.el` 定义。

正则表达式匹配时。

```
(pcase "192.168.1.1"
  ((rx (1+ (or num ".")))
   "IP address"))
;; => "IP address"
```

恐怕取出匹配更加有用：

```
(pcase "11:30"
  ((rx (let hour (= 2 num)) ":"
       (let minute (= 2 num)))
   (list hour minute)))
;; => ("11" "30")
```

`backref` 除了接受数字（匹配分组，和原始 `rx` 宏一样），还可以跟 `let` 结合起来

```
(pcase "12 = 12"
  ((rx (let x (1+ num))
       " = "
       (backref x))
   "n = n"))
;; => "n = n"
```

## 14. `cl-struct`

当类型匹配时匹配。

```
(cl-defstruct (dog (:constructor dog-create))
  name age)

(pcase (dog-create :name "Snoopy" :age 47)
  ((cl-struct dog name age)
   (format "%s is %s" name age)))
;; => "Snoopy is 47"
```

## 15. `eieio`

当 `EXPVAL` 是 EIEIO 类时匹配。

```
(pcase (person :name "Charlie" :age 4)
  ((eieio name age)
   (format "%s is %d" name age)))
;; => "Charlie is 4"
```

## 16. `(radix-tree-leaf VPAT)`

由 `radix-tree.el` 定义。

匹配 radix tree 数据结构的叶子，`VPAT` 绑定值，只有在 `radix-tree-iter-mappings` 内部用到过，我不清楚它的用途，感觉只是用来在递归这个数据结构时，用来退出的。

```
(pcase 123
  ((radix-tree-leaf v) v))
;; => 123
```
