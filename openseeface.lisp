(in-package #:org.shirakumo.fraf.vtryout)

(bs:define-io-structure packet
  (timestamp :unix-time-u64)
  (id :uint32)
  (width :float32)
  (height :float32)
  (blink-left :float32)
  (blink-right :float32)
  (success-p boolean)
  (pnp-error :float32)
  (face-orientation (vector :float32 4))
  (face-euler (vector :float32 3))
  (face-location (vector :float32 3))
  (landmarks-confidence (vector :float32 68))
  (landmarks-2d (vector :float32 #.(* 68 2)))
  (landmarks-3d (vector :float32 #.(* 70 3)))
  (eye-left :float32)
  (eye-right :float32)
  ;; Steepness, updown, quirk
  (eyebrow-left (vector :float32 3))
  (eyebrow-right (vector :float32 3))
  ;; updown, inout
  (mouth-left (vector :float32 2))
  (mouth-right (vector :float32 2))
  (mouth-open :float32)
  (mouth-wide :float32))

(defun receive-packets (callback socket)
  (let* ((size (bs:octet-size (bs:io-type 'packet)))
         (buffer (make-array size :element-type '(unsigned-byte 8))))
    (declare (dynamic-extent buffer))
    (loop do (when (usocket:wait-for-input socket :timeout 0.2 :ready-only T)
               (usocket:socket-receive socket buffer size)
               (unless (funcall callback (read-packet buffer))
                 (return))))))

(defun packet-transform (packet)
  (let ((tf (transform)))
    (vsetf (tlocation tf)
          (aref (packet-face-location packet) 1)
          (aref (packet-face-location packet) 0)
          (aref (packet-face-location packet) 2))
    (let ((euler (packet-face-euler packet)))
      (!q* (trotation tf)
           (qfrom-angle +vx+ (deg->rad (+ 180 10 (aref euler 0))))
           (qfrom-angle +vy+ (deg->rad (- -10 (aref euler 1))))
           (qfrom-angle +vz+ (deg->rad (- 90 (aref euler 2))))))
    tf))

(define-event face-update ()
  local-transform
  packet)

(defun handle-packet (main packet)
  (when (and (context main) (openseeface-socket main))
    (handle (make-event 'face-update
                        :packet packet
                        :local-transform (packet-transform packet))
            main)
    T))
960 720

(defclass openseeface-main (trial:main)
  ((openseeface-thread :initform NIL :accessor openseeface-thread)
   (openseeface-process :initform NIL :accessor openseeface-process)
   (openseeface-socket :initform NIL :accessor openseeface-socket)))

(defmethod initialize-instance :after ((main openseeface-main) &key)
  (when (setting :openseeface :launch)
    (v:info :vtryout.openseeface "Launching OpenSeeFace")
    (uiop:chdir (data-root))
    (setf (openseeface-process main)
          (uiop:launch-program (print (append (setting :openseeface :binary)
                                              (list
                                               "-s" "1"
                                               "-i" (setting :openseeface :host)
                                               "-p" (princ-to-string (setting :openseeface :port))
                                               "-W" (princ-to-string (setting :openseeface :width))
                                               "-H" (princ-to-string (setting :openseeface :height))
                                               "-F" (princ-to-string (setting :openseeface :framerate)))))
                               :output *error-output* :error-output *error-output*)))
  (v:info :vtryout.openseeface "Listening for OpenSeeFace packets on ~a:~a"
          (setting :openseeface :host) (setting :openseeface :port))
  (let ((socket (usocket:socket-connect NIL NIL
                                        :local-host (setting :openseeface :host)
                                        :local-port (setting :openseeface :port)
                                        :protocol :datagram
                                        :element-type '(unsigned-byte 8))))
    (setf (openseeface-socket main) socket)
    (flet ((thunk ()
             (unwind-protect
                  (ignore-errors
                   (with-error-logging (:vtryout.openseeface)
                     (receive-packets (lambda (packet) (handle-packet main packet)) socket)))
               (v:info :vtryout.openseeface "Exiting OpenSeeFace packet receiver"))))
      (setf (openseeface-thread main) (make-thread "openseeface-receiver" #'thunk)))))

(defmethod finalize :after ((main openseeface-main))
  (let ((socket (openseeface-socket main)))
    (when socket
      (setf (openseeface-socket main) NIL)
      (usocket:socket-close socket)
      (wait-for-thread-exit (openseeface-thread main))))
  (let ((process (openseeface-process main)))
    (when (and process (uiop:process-alive-p process))
      (v:info :vtryout.openseeface "Killing OpenSeeFace")
      (uiop:terminate-process process)
      (setf (openseeface-process main) NIL))))
