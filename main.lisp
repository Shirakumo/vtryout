(in-package #:org.shirakumo.fraf.vtryout)

(defclass main (org.shirakumo.fraf.trial.harmony:settings-main
                org.shirakumo.fraf.trial.notify:main)
  ((trial:scene :initform (make-instance 'scene))))

(defmethod initialize-instance :after ((main main) &key)
  (org.shirakumo.fraf.trial.notify:watch (find-pool 'vtryout)))

(defmethod trial-harmony:server-initargs append ((main main))
  (list :mixers '((:music mixed:basic-mixer :effects ((mixed:biquad-filter :filter :lowpass :name :music-lowpass)))
                  (:effect mixed:space-mixer))
        :effects '((mixed:biquad-filter :filter :lowpass :name :lowpass)
                   (mixed:speed-change :name :speed))))

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
                                        :title "VTryOut"
                                        :version '(4 1)
                                        :profile :core)
                         :audio-backend (setting :audio :backend))))))

(defun main ()
  (command-line-toplevel)
  (load-mods)
  (launch))
