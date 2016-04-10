; Herbie expressions used to optimise floating-point computations.

;
; Warden.Numeric.combineVariance
;

(herbie-test (mu1 var1 k1)
  "Combine variance (one term)"
  (* k1 (+ var1 (sqr mu1))))

(herbie-test (mu var1 k1 var2 k2 t1 t2)
  "Combine variance (simplified)"
  (- (/ (+ t1 t2) (+ k1 k2)) (sqr mu)))

(herbie-test (mu mu1 var1 k1 mu2 var2 k2)
  "Combine variance (full)"
  (- (/ (+ (* k1 (+ var1 (sqr mu1))) (* k2 (+ var2 (sqr mu2)))) (+ k1 k2)) (sqr mu)))
