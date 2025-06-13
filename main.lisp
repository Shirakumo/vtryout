(in-package #:org.shirakumo.fraf.vtryout)

(defun load-scene-description (desc-file)
  (let ((desc-file (pool-path 'vtryout (pathname-utils:parse-native-namestring desc-file))))
    (destructuring-bind (&rest args &key file &allow-other-keys) (trial::parse-sexps desc-file)
      (setf (getf args :file) (merge-pathnames (pathname-utils:parse-native-namestring file) desc-file))
      args)))

(defclass main (org.shirakumo.fraf.trial.harmony:settings-main
                org.shirakumo.fraf.trial.notify:main
                openseeface-main)
  ((trial:scene :initform (make-instance 'scene) :accessor scene)))

(defmethod initialize-instance :after ((main main) &key scene)
  (org.shirakumo.fraf.trial.notify:watch (find-pool 'vtryout))
  (when scene
    (issue (scene main) (apply #'make-instance 'change-scene (load-scene-description scene)))))

(defmethod trial-harmony:setup-server progn ((main main) server)
  ;; Units are in metres, so adjust accordingly.
  (setf (mixed:min-distance :effect) 10.0)
  (setf (mixed:max-distance :effect) 500.0)
  (setf (mixed:soundspeed :effect) 343.3)
  ;; Connect the source up.
  (let ((input (harmony:segment :input server))
        (fft (make-instance 'mixed:fwd-fft :samplerate (harmony:samplerate server)))
        (seg (make-instance 'speech-detection :name 'speech-detection)))
    (harmony:connect input T fft T)
    (mixed:connect fft 0 seg 0 (mixed:make-buffer (mixed:framesize fft)))
    (harmony:add-to input fft seg)
    (setf (harmony:segment (harmony:name seg) server) seg)))

(defmethod trial-harmony:server-initargs append ((main main))
  (list :mixers '((:effect mixed:space-mixer))
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
