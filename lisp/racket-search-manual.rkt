#lang racket

(require setup/xref
         scribble/core
         scribble/xref
         scribble/manual-struct)

(define xref (load-collections-xref))
(define index (xref-index xref))

(for ([entry (in-list index)])
  (match (entry-desc entry)
    [(exported-index-desc name from-libs)
     (printf "~a provided from ~a\n"
             name
             (string-join (map ~a from-libs) ", "))]
    [_ (void)]))
