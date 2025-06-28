(in-package #:org.shirakumo.fraf.vtryout)

(define-global +scene+ NIL)
(define-global +app-system+ "vtryout")
(define-global +settings+
    (copy-tree '(:audio (:latency 0.01
                         :backend :default
                         :device NIL
                         :volume (:master 0.5
                                  :effect 1.0
                                  :speech 1.0
                                  :music 1.0)
                         :volume-range (10.0 . 400.0)
                         :frequency-range (100.0 . 1500.0))
                 :display (:resolution (1280 720)
                           :fullscreen NIL
                           :monitor T
                           :vsync T
                           :gamma 2.2
                           :ui-scale 1.0
                           :frame-scale 1.0
                           :target-framerate NIL
                           :shadow-map-resolution 2048
                           :shadows T
                           :bloom T
                           :fxaa T
                           :ssao T
                           :texture (:filter :trilinear
                                     :anisotropy 2))
                 :language :system
                 :openseeface (:launch T
                               :binary ("openseeface.sh")
                               :host "localhost"
                               :port 11573
                               :width 1280
                               :height 720
                               :framerate 60)
                 :debugging (:show-debug-settings #+release NIL #-release T
                             :send-diagnostics T
                             :remote-debug (:active NIL :port 4005)
                             :fps-counter NIL
                             :system-stats NIL))))

(define-pool vtryout)

(defun n (name)
  (node name T))

(defun leave* (thing scene)
  (let ((thing (node thing scene)))
    (when thing (leave thing scene))))

(defmacro ! (&body body)
  `(when (and +main+ (scene +main+))
     (with-eval-in-render-loop ()
       ,@body)))
