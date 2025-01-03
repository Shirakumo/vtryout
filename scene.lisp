(in-package #:org.shirakumo.fraf.vtryout)

(define-event change-scene () file name)

(defclass scene (pipelined-scene)
  ((camera :initform (make-instance 'editor-camera :name :editor :move-speed 0.1))))

(defmethod setup-scene ((main trial:main) (scene scene))
  ;; Units are in metres, so adjust accordingly.
  (setf (mixed:min-distance :effect) 10.0)
  (setf (mixed:max-distance :effect) 500.0)
  (setf (mixed:soundspeed :effect) 343.3)
  (enter (camera scene) scene)
  (setup-pipeline scene))

(defmethod setup-pipeline ((scene scene))
  (construct-pipeline scene
      (z-prepass
       (ssao-pbr-render-pass
        :name 'render
        :ssao-radius 0.3
        :ssao-bias 0.1
        :allow-other-keys T)
       (bloom-cutoff-pass :name 'bloom-cutoff-pass
                          :threshold 1.0)
       (bloom-merge-pass :name 'bloom-merge-pass
                         :intensity 2.0)
       (uchimura :name 'tone-map
                 :max-brightness 1.0
                 :contrast 1.0
                 :linear-start 0.22
                 :linear-length 0.4
                 :black-tightness-shape 1.33
                 :black-tightness-offset 0.0)
       fxaa-pass
       (ui :name 'trial-alloy:ui)
       (post-effects-pass :name 'post))
    ((z-prepass NIL depth) (render depth-map))
    ((z-prepass NIL depth) (post depth-map))
    (render bloom-cutoff-pass (bloom-merge-pass bloom-cutoff color) tone-map)
    (render bloom-merge-pass)
    (tone-map fxaa-pass post)
    (trial-alloy:ui (post ui-map))))

(define-handler (scene change-scene) (file name)
  (generate-resources (make-instance 'model-file :input file :pool (find-pool 'vtryout)) T
                      :load-scene name)
  (ensure-entity 'ambient-light scene 'ambient-light :color (vec3 0.01))
  (loop for pass across (passes scene)
        do (dolist (thing (to-preload scene))
             (when (typep thing '(or class entity))
               (enter thing pass))))
  (commit scene (loader +main+))
  (ignore-errors (reset-render-loop)))

(define-shader-pass post-effects-pass (post-effect-pass)
  ((ui-map :port-type input :accessor ui-map)
   (depth-map :port-type input :accessor depth-map)
   (previous-pass :port-type input :accessor previous-pass)
   (color :port-type output :accessor color)
   (midpoint :uniform T :initform (vec3 0.5) :accessor midpoint)
   (color-filter :uniform T :initform (vec3 1) :accessor color-filter)
   (exposure :uniform T :initform (vec3 1) :accessor exposure)
   (contrast :uniform T :initform (vec3 1) :accessor contrast)
   (brightness :uniform T :initform (vec3 0) :accessor brightness)
   (saturation :uniform T :initform (vec3 1) :accessor saturation)
   (temperature :uniform T :initform 0.0 :accessor temperature)
   (tint :uniform T :initform 0.0 :accessor tint)
   (hue :uniform T :initform 0.0 :accessor hue))
  (:shader-file (vtryout "shaders/post.glsl"))
  (:buffers (trial standard-environment-information)))

(defun actor ()
  (do-scene-graph (node (scene +main+))
    (when (typep node 'animation-controller)
      (return node))))

(defun activate-camera (name)
  (do-scene-graph (node (print (node name T)))
    (when (typep (print node) 'camera)
      (activate node)
      (return))))

(defun toggle-layer (name &key (actor (actor)) (strength 1.0))
  (unless (clip actor)
    (play :idle actor))
  (if (animation-layer name actor)
      (remove-animation-layer name actor)
      (add-animation-layer name actor :strength strength)))
