(in-package #:org.shirakumo.fraf.vtryout)

(define-shader-entity actor (basic-animation-controller)
  ())

;; KLUDGE
(defmethod enter :after ((entity animated-entity) (controller animation-controller))
  (setf (animation-controller entity) controller))

(defmethod fk-update ((actor actor) pose tt dt fc)
  (let* ((bone (node :DEF-JAW (skeleton actor)))
         (joint (elt pose bone)))
    #++(!q* (trotation joint) (qfrom-angle +vx+ (deg->rad 45)) (trotation joint))))

(defun actor ()
  (do-scene-graph (node (scene +main+))
    (when (typep node 'actor)
      (return node))))

(defun toggle-layer (name &key (actor (actor)) (strength 1.0))
  (unless (clip actor)
    (play :idle actor))
  (if (animation-layer name actor)
      (remove-animation-layer name actor)
      (add-animation-layer name actor :strength strength)))

(defun activate-camera (name &optional (scene T))
  (with-simple-restart (continue "Don't activate the camera")
    (do-scene-graph (node (node name scene) (error "No camera named ~s" name))
      (when (typep node 'camera)
        (activate node)
        (return)))))

