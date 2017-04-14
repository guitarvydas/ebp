(defun send-from-outside (sched c pin data)
  (setf (inqueue c) (append (inqueue c)
                            (list (make-instance 'event :pin pin :data data))))
  (setf (ready-queue sched ) (append (ready-queue sched) (list c))))

(defclass probe (leaf)
  ())

(defmethod execute ((self probe))
  (with-slots (inqueue) self
    (unless inqueue (error "assert"))
    (let ((ev (pop inqueue)))
      (unless (string= "in" (pin ev)))
      (format t "probe --> ~a~%" (data ev))
      (send self "out" (data ev)))))

;
; ctop contains c which contains p and p2
; ctop "in" goes to c "in"
; c "in" goes to p "in"
; p "out" goes to p2 "in"
; p2 "out" goes to c "out"
; c "out" goes to ctop "out"
;
(defun top1 ()
  (let ((c (make-container "c"))
        (ctop (make-container "ctop"))
        (p (make-instance 'probe))
        (p2 (make-instance 'probe)))
    (add-instance c p "probe1")
    (add-instance c p2 "probe2")
    (add-net c c "in" (list (list p "in")))
    (add-net c p "out" (list (list p2 "in")))
    (add-net c p2 "out" (list (list c "out")))
    (add-instance ctop c "top container")
    (add-net ctop ctop "in" (list (list c "in")))
    (add-net ctop c "out" (list (list ctop "out")))
    ctop))

;
; ctop contains c and p, c contains p2
; ctop "in" goes to p "in"
; p "out" goes to c "in"
; c "in" goes to p2 "in"
; p2 "out" goes to c "out"
; c "out" goes to ctop "out"
;
(defun top2 ()
  (let ((c (make-container "c"))
        (ctop (make-container "ctop"))
        (p (make-instance 'probe))
        (p2 (make-instance 'probe)))
    (add-instance ctop p "probe1")
    (add-instance ctop c "top container")
    (add-net ctop ctop "in" (list (list p "in")))
    (add-net ctop p "out" (list (list c "in")))
    (add-instance c p2 "probe2")
    (add-net c c "in" (list (list p2 "in")))
    (add-net c p2 "out" (list (list c "out")))
    (add-net ctop c "out" (list (list ctop "out")))
    ctop))

;
; ctop contains p, p2
; ctop "in" goes to p "in"
; p "out" goes to p2 "in"
; p2 "out" goes to p "in" (infinite loop)
;
(defun top3 ()
  (let ((ctop (make-container "ctop"))
        (p (make-instance 'probe))
        (p2 (make-instance 'probe)))
    (add-instance ctop p "probe1")
    (add-instance ctop p2 "probe2")
    (add-net ctop ctop "in" (list (list p "in")))
    (add-net ctop p "out" (list (list p2 "in")))
    (add-net ctop p2 "out" (list (list p "in")))
    ctop))

;
; ctop contains p, c
; ctop "in" goes to p "in"
; p "out" goes to c "in"
; c "in" goes to p2 "in"
; p2 "out" goes to c "out"
; c "out" goes to p "in"
;
(defun top4 ()
  (let ((ctop (make-container "ctop"))
        (c (make-container "c"))
        (p (make-instance 'probe))
        (p2 (make-instance 'probe)))
    (add-instance ctop p "probe1")
    (add-instance ctop c "c")
    (add-instance c p2 "probe2")
    (add-net ctop ctop "in" (list (list p "in")))
    (add-net ctop p "out" (list (list c "in")))
    (add-net c c "in" (list (list p2 "in")))
    (add-net c p2 "out" (list (list c "out")))
    (add-net ctop c "out" (list (list p "in")))
    ctop))
;
; ctop contains p, c
; ctop "in" goes to p "in"
; p "out" goes to c "in"
; c "in" goes to c "out"
; c "out" goes to p "in"
;
(defun top5 ()
  (let ((ctop (make-container "ctop"))
        (c (make-container "c"))
        (p (make-instance 'probe)))
    (add-instance ctop p "probe1")
    (add-instance ctop c "c")
    (add-net ctop ctop "in" (list (list p "in")))
    (add-net ctop p "out" (list (list c "in")))
    (add-net c c "in" (list (list c "out")))
    (add-net ctop c "out" (list (list p "in")))
    ctop))

;
; ctop contains p, c
; ctop "in" goes to p "in" and c "in"
; c contains p2
;
(defun top6 ()
  (let ((ctop (make-container "ctop"))
        (c (make-container "c"))
        (p (make-instance 'probe))
        (p2 (make-instance 'probe)))
    (add-instance ctop p "probe1")
    (add-instance ctop c "c")
    (add-net ctop ctop "in" (list (list p "in") (list c "in")))
    (add-instance c p2 "probe2")
    (add-net c c "in" (list (list p2 "in")))
    ctop))

;
; ctop contains p, c, p3
; ctop "in" goes to p "in" and c "in"
; c contains p2
; p2 "out" goes to c "out"
; c "out" goes to p3 "in"
;
(defun top7 ()
  (let ((ctop (make-container "ctop"))
        (c (make-container "c"))
        (p (make-instance 'probe))
        (p2 (make-instance 'probe))
        (p3 (make-instance 'probe)))
    (add-instance ctop p "probe1")
    (add-instance ctop c "c")
    (add-instance ctop p3 "probe3")
    (add-net ctop ctop "in" (list (list p "in") (list c "in")))
    (add-instance c p2 "probe2")
    (add-net c c "in" (list (list p2 "in")))
    (add-net c p2 "out" (list (list c "out")))
    (add-net ctop c "out" (list (list p3 "in")))
    ctop))

(defun run ()
  (let ((top (top7))
        (scheduler (make-container "scheduler")))
    (send-from-outside scheduler top "in" "hello")
    (send-from-outside scheduler top "in" "middle")
    (send-from-outside scheduler top "in" "goodbye")
    (loop while (ready-queue scheduler)
          do (let ((c (pop (ready-queue scheduler))))
               (execute c)
               (setf (output-queue c) nil)
               (loop while (ready-queue c)
                     do (progn
                          (execute c)
                          (setf (output-queue c) nil)))))))
    