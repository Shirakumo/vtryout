(in-package #:org.shirakumo.fraf.vtryout)

(define-shader-entity actor (basic-animation-controller)
  ())

(defmethod shared-initialize :after ((actor actor) slots &key layers)
  (dolist (layer layers)
    (destructuring-bind (name &optional (strength 1.0)) (enlist layer)
      (add-animation-layer name actor :strength strength :if-exists :supersede))))

;; KLUDGE
(defmethod enter :after ((entity animated-entity) (controller animation-controller))
  (setf (animation-controller entity) controller))

(defmethod update :before ((actor actor) tt dt fc)
  (let* ((layer (animation-layer :mouth-open actor :if-does-not-exist :create))
         (seg (harmony:segment 'speech-detection T))
         (new (if (speech-p seg)
                  (/ (- (clamp 300.0 (vy (main-frequency seg)) 800.0) 300.0)
                     500.0)
                  0.0)))
    (setf (strength layer) (lpf 0.99 (strength layer) new))))

(defun actor (&optional name)
  (do-scene-graph (node (scene +main+))
    (when (and (typep node 'actor)
               (or (not name) (eql name (name node))))
      (return node))))

(defun toggle-layer (name &key (actor (actor)) (strength 1.0))
  (if (animation-layer name actor)
      (remove-animation-layer name actor)
      (add-animation-layer name actor :strength strength)))

(defun activate-camera (name &optional (scene T))
  (with-simple-restart (continue "Don't activate the camera")
    (do-scene-graph (node (node name scene) (error "No camera named ~s" name))
      (when (typep node 'camera)
        (activate node)
        (return)))))

