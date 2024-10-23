(define-module (pso)
  #:use-module (ice-9 match)
  #:use-module (ice-9 threads)
  #:use-module ((srfi srfi-1) #:select (fold unfold circular-list take drop take-right drop-right))
  #:export (make-particle particle-pos particle-vel particle-best))
(define fill (make-parameter 0))
(define type (make-parameter 'f64))
;;dims=4
;;0 0 0 0... pos
;;0 0 0 0... vel
;;0 0 0 0... best

(define (make-particle dims)
  (make-typed-array (type) (fill) 3 dims))

(define-inlinable (particle-pos prtcl)
  (define dims (cadr (array-dimensions prtcl)))
  (make-shared-array prtcl (lambda (i) (list 0 i)) `(0 ,(1- dims))))
(define-inlinable (particle-vel prtcl)
  (define dims (cadr (array-dimensions prtcl)))
  (make-shared-array prtcl (lambda (i) (list 1 i)) `(0 ,(1- dims))))
(define-inlinable (particle-best prtcl)
  (define dims (cadr (array-dimensions prtcl)))
  (make-shared-array prtcl (lambda (i) (list 2 i)) `(0 ,(1- dims))))

(define (print-particle p)
  (display (particle-best p)) (newline)
  (display (particle-pos p)) (newline)
  (display (particle-vel p)) (newline))

(define-inlinable (1r0)
  (/ (random 4294967296)
     4294967296.))
(define-inlinable (random-real lower upper)
  (+ lower (* (1r0) (- upper lower))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;(limits dim)
(define (make-swarm count dims limit)
  (define (randomize-particle p)
    (let ([pos  (particle-pos  p)]
	  [vel  (particle-vel  p)]
	  [best (particle-best p)])
      (random:solid-sphere! vel)
      (do ((i 0 (1+ i)))
	  ((>= i dims))
	(let ([a (limit i)])
	  (array-set! pos (random-real (car a) (cdr a)) i)))
      (array-copy! pos best))
    p)
  (unfold zero?
	  (lambda a (randomize-particle (make-particle dims)))
	  1- count))

(define (step-particle p better)
  (let ([pos  (particle-pos  p)]
	[vel  (particle-vel  p)]
	[best (particle-best p)])
    (array-map! pos (lambda (p v) (+ p v))
		vel pos)
    (when (better pos best)
      (array-copy! pos best))))

(define (step-swarm swarm better best)
  (for-each (lambda (p) (step-particle p better)) swarm)
  (apply better swarm))

(define (div swarm)
  (define l  (length swarm))
  (define th (current-processor-count))
  (define step (inexact->exact (ceiling (/ l th 1.))))
  (display step) (newline)
  (let loop ([sw swarm] [res '()] [l l])
    (cond
     [(null? sw) res]
     [(negative? (- l step)) (display "hi\n") (append res `(,sw))]
     [else ;;(display res) (newline)
	   (loop (drop sw step)
		 (append res `(,(take sw step)))
		 (- l step))])))

(define (para-step-swarm swarm better best)
  (for-each join-thread
	    (map (lambda (sub-swarm)
		   (begin-thread (step-swarm sub-swarm better best)))
		 (div swarm))))
