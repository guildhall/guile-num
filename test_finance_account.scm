(use-modules (finance account))

(define (assert . a)
  (map (lambda (x) (display x) (newline)) a)
  (newline))

(define account-types
  '(income expense asset liability cash bank stock))

(define (date year month day)
;;;  (mktime (list->vector (list 0 0 0 day (- month 1) (- year 1900)))))
  (+ (* year 372) (* 31 (- month 1)) (- day 1)))

(assert (equal? (balanced-transaction?
		 (make-transaction (date 2002 3 1)
				   (list (make-split "s1" 'bank 12)
					 (make-split "s3" 'bank 11)
					 (make-split "s2" 'cash  -23))))
		#t)
	(equal? (balanced-transaction?
		 (make-transaction (date 2002 3 1)
				   (list (make-split "s1" 'bank 23)
					 (make-split "s2" 'cash  -22))))
		#f))

(let ((transactions
       (list (make-transaction (date 2002 3 1)
			       (list (make-split "s1" 'bank  12)
				     (make-split "s3" 'house 11)
				     (make-split "s2" 'cash   -23)))
	     (make-transaction (date 2002 3 1)
			       (list (make-split "s1" 'bank 10)
				     (make-split "s2" 'cash  -10))))))
  (assert (eq? (balance-to-date (date 2002 2 28) 'bank transactions) 0)
	  (eq? (balance-to-date (date 2002 2 28) 'cash transactions) 0)
	  (eq? (balance-to-date (date 2002 2 28) 'house transactions) 0)
	  (eq? (balance-to-date (date 2002 3 1) 'bank transactions) 22)
	  (eq? (balance-to-date (date 2002 3 1) 'cash transactions) -33)
	  (eq? (balance-to-date (date 2002 3 1) 'house transactions) 11)))

(define initial-transactions
  (list (make-transaction (date 2002 3 1)
			  (list (make-split "opening balance"
					    'bank 1500)
				(make-split "opening balance"
					    'opening-balance -1500)))
	(make-transaction (date 2002 3 1)
			  (list (make-split "opening balance"
					    'savings 0)
				(make-split "opening balance"
					    'opening-balance 0)))))

(define (monthly-transactions description from to amount
			      year month day recurrence)
  (define (template-transaction year month day)
    (make-transaction (date year month day)
		      (list (make-split description from (- amount))
			    (make-split description to   amount))))

  (cond ((<= recurrence 0) '())
	((> month 12)
	 (cons (template-transaction (+ year 1) 1 day)
	       (monthly-transactions description from to amount
				     (+ year 1) 1 day
				     (- recurrence 1))))
	 (else
	  (cons (template-transaction year month day)
		(monthly-transactions description from to amount
				      year (+ month 1) day
				      (- recurrence 1))))))

(let ((transactions (monthly-transactions "food" 'bank 'food
					  (/ 3000 12)
					  2002 1 1 13)))
  (assert (eq? (balance-to-date (date 2002 1 2) 'bank transactions)
	       -250)
	  (eq? (balance-to-date (date 2003 1 2) 'bank transactions)
	       -3250)
	  (eq? (balance-to-date (date 2003 1 1) 'bank transactions)
	       -3250)
	  (eq? (balance-to-date (date 2002 12 31) 'bank transactions)
	       -3000)))

(define (mortgage-transactions description
			       bank-account asset-account expense-account
			       amount percentage
			       year month day recurrence)
  (define (template-transaction year month day interest redemption)
    (make-transaction (date year month day)
		      (list (make-split description bank-account
					(- (+ interest redemption)))
			    (make-split description asset-account redemption)
			    (make-split description expense-account interest))))

  (let ((redemption (/ amount recurrence))
	(interest (* amount percentage)))
    (cond ((<= recurrence 0) '())
	  ((> month 12)
	   (cons (template-transaction (+ year 1) 1 day interest redemption)
		 (mortgage-transactions description bank-account
					asset-account expense-account
					(- amount redemption) percentage
					(+ year 1) 1 day
					(- recurrence 1))))
	  (else
	   (cons (template-transaction year month day interest redemption)
		 (mortgage-transactions description bank-account
					asset-account expense-account
					(- amount redemption) percentage
					year (+ month 1) day
					(- recurrence 1)))))))

