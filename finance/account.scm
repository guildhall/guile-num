(define-module (finance account)
  :use-module (ice-9 slib)
  :use-module (ice-9 format))

(export make-account
	account?
	account->description
	account->type
	account->currency
	make-transaction
	transaction?
	transaction->date
	transaction->splits
	make-split
	split?
	split->description
	split->account
	split->amount
	split->price
	make-stock-split
	split->balance
	balanced-transaction?
	splits-with-account
	cleanup-empty-splits
	balance-account-splits
	amount-account-splits
	transactions-to-date
	transactions-with-account
	balance-to-date
	amount-to-date
	print-splits
	print-transactions)

(require 'record)
(require 'posix-time)

(define account-type
  (make-record-type "account" '(description type currency)))
(define make-account
  (record-constructor account-type '(description type currency)))
(define account? (record-predicate account-type))
(define account->description (record-accessor account-type 'description))
(define account->type (record-accessor account-type 'type))
(define account->currency (record-accessor account-type 'currency))

(define transaction-type
  (make-record-type "transaction" '(date splits)))
(define make-transaction
  (record-constructor transaction-type '(date splits)))
(define transaction? (record-predicate transaction-type))
(define transaction->date (record-accessor transaction-type 'date))
(define transaction->splits (record-accessor transaction-type 'splits))

(define split-type
  (make-record-type "split" '(description account amount price)))
(define make-split
  (record-constructor split-type '(description account amount price)))
(define split? (record-predicate split-type))
(define split->description (record-accessor split-type 'description))
(define split->account (record-accessor split-type 'account))
(define split->amount (record-accessor split-type 'amount))
(define split->price (record-accessor split-type 'price))

(define make-stock-split make-split)
(define (make-split description account price)
  (make-stock-split description account 1 price))

(define (split->balance split)
  (* (split->amount split) (split->price split)))

(define (balanced-transaction? ta)
  (< (abs (apply + (map split->balance (transaction->splits ta))))
     1e-4))

(define (filter func lst)
  (cond ((null? lst) lst)
	((func (car lst))
	 (cons (car lst) (filter func (cdr lst))))
	(else (filter func (cdr lst)))))

(define (flatten lst)
  (cond ((null? lst) lst)
	((pair? (car lst))
	 (append (flatten (car lst)) (flatten (cdr lst))))
	(else
	 (cons (car lst)
	       (flatten (cdr lst))))))

;;; (begin
;;;   (assert (equal? (flatten '(1 2 3 4 5)) '(1 2 3 4 5))
;;; 	  (equal? (flatten '((1 2 3) (4 5))) '(1 2 3 4 5))
;;; 	  (equal? (flatten '((1) (2) (3) (4) (5))) '(1 2 3 4 5))
;;; 	  (equal? (flatten '(1 (2 (3 (4 (5)))))) '(1 2 3 4 5))))

(define (splits-with-account account splits)
  (filter (lambda (split)
	    (equal? (split->account split) account))
	  splits))

(define (cleanup-empty-splits transactions)
  (filter (lambda (transaction)
	    (not (null? (transaction->splits transaction))))
	  transactions))

(define (balance-account-splits account splits)
  (apply + (map split->balance (splits-with-account account splits))))

(define (amount-account-splits account splits)
  (apply + (map split->amount (splits-with-account account splits))))

(define (transactions-to-date timestamp transactions)
  (filter (lambda (transaction)
	    (<= (transaction->date transaction) timestamp))
	  transactions))

(define (transactions-with-account account transactions)
  (cleanup-empty-splits
   (map (lambda (transaction)
	  (make-transaction
	   (transaction->date transaction)
	   (splits-with-account account
				(transaction->splits transaction))))
	transactions)))

(define (balance-to-date timestamp account transactions)
  (apply + (map (lambda (splits) (balance-account-splits account splits))
		(map transaction->splits
		     (transactions-to-date timestamp transactions)))))

(define (amount-to-date timestamp account transactions)
  (apply + (map (lambda (splits) (amount-account-splits account splits))
		(map transaction->splits
		     (transactions-to-date timestamp transactions)))))


(define (print-splits splits)
    (if (not (null? splits))
	(let* ((split (car splits))
	       (description (split->description split))
	       (account (split->account split))
	       (price   (split->price split))
	       (amount  (split->amount split))
	       (balance (split->balance split)))
	  (format "\t~-20A\t~-10A\t~10A" description account balance)
	  (if (not (= amount 1))
	      (format " (~G @ ~G)" amount price))
	  (newline)
	  (print-splits (cdr splits)))))

(define (print-transactions transactions)
  (if (not (null? transactions))
      (let ((transaction (car transactions)))
	(display (ctime (transaction->date transaction)))
	(print-splits (transaction->splits transaction))
	(print-transactions (cdr transactions)))))
