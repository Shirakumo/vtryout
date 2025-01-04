(in-package #:org.shirakumo.fraf.vtryout)

(defclass main (org.shirakumo.fraf.trial.harmony:settings-main
                org.shirakumo.fraf.trial.notify:main)
  ((trial:scene :initform (make-instance 'scene) :accessor scene)))

(defmethod initialize-instance :after ((main main) &key scene-file (scene T) camera)
  (org.shirakumo.fraf.trial.notify:watch (find-pool 'vtryout))
  (when scene-file (issue (scene main) 'change-scene :file scene-file :name scene :camera camera)))

(defmethod trial-harmony:setup-server progn ((main main) server)
  ;; Units are in metres, so adjust accordingly.
  (setf (mixed:min-distance :effect) 10.0)
  (setf (mixed:max-distance :effect) 500.0)
  (setf (mixed:soundspeed :effect) 343.3)
  ;; Connect the source up.
  (let ((seg (make-instance 'speech-detection)))
    (harmony:add-to :sources seg)
    (harmony:connect :source 0 seg 0)))

(defmethod trial-harmony:server-initargs append ((main main))
  (list :mixers '((:music mixed:basic-mixer :effects ((mixed:biquad-filter :filter :lowpass :name :music-lowpass)))
                  (:effect mixed:space-mixer))
        :effects '((mixed:biquad-filter :filter :lowpass :name :lowpass)
                   (mixed:speed-change :name :speed))
        :source T))

(defun launch (&rest initargs)
  (let ((*package* #.*package*))
    (load-keymap)
    (ignore-errors
     (load-settings))
    (save-settings)
    (apply #'trial:launch 'main
           (append initargs
                   (setting :debugging :initargs)
                   (list :context (list :width (first (setting :display :resolution))
                                        :height (second (setting :display :resolution))
                                        :vsync (setting :display :vsync)
                                        :fullscreen (setting :display :fullscreen)
                                        :transparent-framebuffer T
                                        :decorated NIL
                                        :title "VTryOut"
                                        :version '(4 1)
                                        :profile :core)
                         :audio-backend (setting :audio :backend))))))

(defun main ()
  (command-line-toplevel)
  (load-mods)
  (launch))
