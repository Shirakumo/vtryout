(in-package #:org.shirakumo.fraf.vtryout)

(define-event change-scene () file name camera)

(defclass scene (pipelined-scene)
  ((camera :initform (make-instance 'editor-camera :name :editor :move-speed 0.1))
   (transparent-p :initform T :initarg :transparent :accessor transparent-p)))

(defmethod setup-scene ((main trial:main) (scene scene))
  (enter (make-instance 'display-controller) scene)
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
       (hable :name 'tone-map)
       fxaa-pass
       (ui :name 'trial-alloy:ui)
       (post-effects-pass :name 'post))
    ((z-prepass NIL depth) (render depth-map))
    ((z-prepass NIL depth) (post depth-map))
    (render bloom-merge-pass)
    (render bloom-cutoff-pass (bloom-merge-pass bloom-cutoff color) tone-map)
    (tone-map fxaa-pass post)
    (trial-alloy:ui (post ui-map))))

(defmethod object-renderable-p ((_ fps-counter) (pass standard-render-pass)) NIL)
(defmethod object-renderable-p ((_ system-stats) (pass standard-render-pass)) NIL)
(defmethod object-renderable-p ((_ debug-draw) (pass standard-render-pass)) NIL)
(defmethod object-renderable-p ((_ debug-text) (pass standard-render-pass)) NIL)

(defmethod render :after ((scene scene) (target null))
  (let ((passes (passes scene)))
    (bind (framebuffer (aref passes (1- (length passes)))) :framebuffer))
  (flet ((maybe-render (name)
           (let ((node (node name scene)))
             (when (typep node 'renderable) (render node target)))))
    (maybe-render 'debug-draw)
    (maybe-render 'system-stats)
    (maybe-render 'fps-counter)
    (maybe-render :controller)))

(defmethod in-view-p ((skybox skybox) camera)
  (not (transparent-p (trial:scene skybox))))

(define-handler (scene change-scene) (file name camera)
  (setf (camera scene) (node :editor scene))
  (generate-resources (make-instance 'model-file :input file :pool (find-pool 'vtryout)) T
                      :load-scene name)
  (ensure-entity 'ambient-light scene 'ambient-light :color (vec3 0.01))
  (loop for pass across (passes scene)
        do (dolist (thing (to-preload scene))
             (when (typep thing '(or class entity))
               (enter thing pass))))
  (when camera
    (activate-camera camera scene))
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

(defun activate-camera (name &optional (scene T))
  (do-scene-graph (node (node name scene))
    (when (typep node 'camera)
      (activate node)
      (return))))

(defun toggle-layer (name &key (actor (actor)) (strength 1.0))
  (unless (clip actor)
    (play :idle actor))
  (if (animation-layer name actor)
      (remove-animation-layer name actor)
      (add-animation-layer name actor :strength strength)))

