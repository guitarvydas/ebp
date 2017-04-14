(defclass event ()
  ((pin :accessor pin :initarg :pin)
   (data :accessor data :initarg :data)))

(defclass component ()
  ((inqueue :initform nil :accessor inqueue)
   (busy :initform nil :accessor busy)
   (parent :initform nil :accessor parent)
   (id :initform "" :accessor id)))

(defclass container (component)
  ((ready-queue :initform nil :accessor ready-queue)
   (instances :initform nil :accessor instances)
   (incoming-nets :initform nil :accessor incoming-nets)
   (internal-nets :initform nil :accessor internal-nets)
   (output-queue :initform nil :accessor output-queue)))

(defclass leaf (component)
  ())



