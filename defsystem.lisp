(defsystem ebp (:optimize ((speed 0) (space 0) (safety 3) (debug 3) (float 1)))
  :members ("ebp.lisp"
            "container.lisp"
            "test1.lisp"
            )
  :rules ((:compile :all (:requires (:load :previous)))))
