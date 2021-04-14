(set not (lambda (x) (if x $F $T)))
(set 1st (lambda (x) (nth 0 x)))
(set 2nd (lambda (x) (nth 1 x)))
(set when
     (lambda-reflect
      (exp env cont)
      (normalize (pcons 'if (up [ (down (1st exp))
                                  (down (2nd exp))
                                  $F ])) env cont)))
(set for-each
     (lambda (f lis)
       (when (not (empty lis))
         (begin (f (1st lis)) (for-each f (rest lis))))))

(set search
     (lambda-reflect
      (exp env cont)
      (normalize (2nd exp) env
                 (lambda (list!)
                   (for-each (lambda (el)
                               (when ((down (binding (1st exp) env)) el)
                                 (cont el)))
                             (down list!))))))

(set advise-before
     (lambda (closure advice)
       (set-body closure (pcons 'begin (rcons advice (body closure))))))

(set foo (lambda (x) (+ x x)))
(foo 5)
(advise-before (up foo) '(print "foo called"))
(foo 5)
