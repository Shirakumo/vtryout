(in-package #:org.shirakumo.fraf.vtryout)

(define-shader-entity actor (basic-animation-controller)
  ())

(defmethod shared-initialize :after ((actor actor) slots &key layers)
  (dolist (layer layers)
    (destructuring-bind (name &optional (strength 1.0)) (enlist layer)
      (add-animation-layer name actor :strength strength :if-exists :supersede))))

(defmethod fk-update ((actor actor) pose tt dt fc)
  #++
  (let* ((bone (node :ORG-SPINE.006 (skeleton actor)))
         (joint (elt pose bone)))
    (!q* (trotation joint) (qfrom-angle +vx+ (sin (* 10 tt))) (trotation joint))))

;; KLUDGE
(defmethod enter :after ((entity animated-entity) (controller animation-controller))
  (setf (animation-controller entity) controller))

(defmethod update :before ((actor actor) tt dt fc)
  (let* ((layer (animation-layer :mouth-open actor :if-does-not-exist :create))
         (seg (harmony:segment 'speech-detection T))
         (new (if (speech-p seg)
                  (* 1.5 (vz (main-frequency seg)))
                  0.0)))
    (setf (strength layer) (lpf 0.8 (strength layer) new))))

(defun actor (&optional name)
  (do-scene-graph (node (scene +main+))
    (when (and (typep node 'actor)
               (or (not name) (eql name (name node))))
      (return node))))

(defun toggle-layer (name &key (actor (actor)) (strength 1.0))
  (if (animation-layer name actor)
      (remove-animation-layer name actor)
      (add-animation-layer name actor :strength strength)))

(defun activate-camera (name &optional (scene (scene +main+)))
  (with-simple-restart (continue "Don't activate the camera")
    (do-scene-graph (node (node name scene) (error "No camera named ~s" name))
      (when (typep node 'camera)
        (activate node)
        (return)))))

(defun activate-next-camera (&optional (scene (scene +main+)))
  (let ((found NIL))
    (or (do-scene-graph (node scene)
          (when (typep node 'camera)
            (cond ((eq node (camera scene))
                   (setf found T))
                  (found
                   (return (activate node))))))
        (do-scene-graph (node scene)
          (when (typep node 'camera)
            (return (activate node)))))))

(defun activate-prev-camera (&optional (scene (scene +main+)))
  (let ((prev NIL))
    (or (do-scene-graph (node scene)
          (when (typep node 'camera)
            (cond ((and prev (eq node (camera scene)))
                   (return (activate prev)))
                  (T
                   (setf prev node)))))
        (when prev
          (activate prev)))))
