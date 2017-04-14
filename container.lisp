(defmethod make-container (&optional (name ""))
  (let ((cont (make-instance 'container)))
    (with-slots (ready-queue instances incoming-nets internal-nets output-queue id) cont
      (setf ready-queue nil
            instances (make-hash-table)
            incoming-nets (make-hash-table :test 'string-equal) ; incoming ports from outside 
            internal-nets (make-hash-table) ; internal nets between children
            output-queue nil
            id name))
    cont))

(defmethod add-instance ((self container) inst name)
  (setf (parent inst) self)
  (setf (gethash name (instances self)) inst))

(defmethod i-am-finished ((self container) (part component))
  (setf (ready-queue self) (delete part (ready-queue self)))
  (when (null (ready-queue self))
    (i-am-finished (parent self) self)))

(defmethod i-am-finished ((self (eql nil)) part)
  )

(defmethod i-can-run ((self container) (part component))
  (setf (ready-queue self) (append (ready-queue self) (list part))))

(defmethod i-can-run ((self (eql nil)) part)
  )

(defmethod add-net ((self container) instance port-name target-list)
  (with-slots (incoming-nets internal-nets) self
    (if (eq instance self)
        (multiple-value-bind (m success) (gethash port-name incoming-nets)
          (declare (ignore m))
          (when success (error "multiple nets for same pin"))
          (setf (gethash port-name incoming-nets) target-list))
      (multiple-value-bind (inst-outs success) (gethash instance internal-nets)
        (unless success
          (setf inst-outs (make-hash-table :test 'string-equal))
          (setf (gethash instance internal-nets) inst-outs))
        (multiple-value-bind (port-map success) (gethash port-name inst-outs)
          (declare (ignore port-map))
          (when success (error "multiple nets for same pin"))
          (setf (gethash port-name inst-outs) target-list))))))

(defmethod send ((self component) pin data)
  (when (parent self) 
    (setf (output-queue (parent self))
          (append (output-queue (parent self)) (list (make-instance 'event :pin pin :data data))))))
  
(defmethod execute ((self container))
  (with-slots (inqueue in-pin-list out-pin-list parent ready-queue output-queue
                       incoming-nets internal-nets busy) self
    (when (and (null inqueue) (null ready-queue)) (error "inqueue and ready queue empty"))
    (when (and inqueue (null busy))
      (setf busy t)
      (let ((event (pop inqueue)))
        (let ((pin (pin event))
              (data (data event)))
          (let ((net (gethash pin incoming-nets)))
            (unless net (error "no net"))
            (dolist (pair net)
              (let ((inst (first pair))
                    (pin (second pair)))
                (if (eq inst self)
                    (send self pin data)
                  (let ((new-ev (make-instance 'event :pin pin :data data)))
                    (setf (inqueue inst) (append (inqueue inst) (list new-ev)))
                    (setf ready-queue (append ready-queue (list inst)))))))))))
    (unless (null ready-queue)
      (let ((component (pop ready-queue)))
        (when output-queue (error "assert oq not empty"))
        (execute component)
        (loop while output-queue
              do (let ((ev (pop output-queue)))
                   (let ((pin (pin ev))
                         (data (data ev)))
                     (let ((inst-hash (gethash component internal-nets)))
                       (when inst-hash ;; ok to dump the event if pin not wired up
                         (let ((net (gethash pin inst-hash)))
                           (when net  ;; it is OK to dump the event if pin not wired up
                             (dolist (pair net)
                               (let ((inst (first pair))
                                     (pin (second pair)))
                                 (if (eq inst self)
                                     (send self pin data)
                                   (let ((new-ev (make-instance 'event :pin pin :data data)))
                                     (setf (inqueue inst)
                                           (append (inqueue inst) (list new-ev)))
                                     (setf ready-queue
                                           (append ready-queue (list inst))))))))))))))))
    (if (null ready-queue)
        (progn
          (setf busy nil)
          (i-am-finished (parent self) self))
      (i-can-run (parent self) self))))

                       
