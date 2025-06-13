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
  (landmarks-2d (vector :float32 (* 68 2)))
  (landmarks-3d (vector :float32 (* 70 3)))
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
    (loop do (usocket:socket-receive socket buffer size)
          while (funcall callback (read-packet buffer)))))

(defun packet-transform (packet)
  (let ((tf (transform)))
    (replace (varr (tlocation tf)) (packet-face-location packet))
    (let ((euler (packet-face-euler packet)))
      (!q* (trotation tf)
           (qfrom-angle +vx+ (deg->rad (- 180 (aref euler 0))))
           (qfrom-angle +vz+ (deg->rad (aref euler 1)))
           (qfrom-angle +vy+ (deg->rad (- (aref euler 2) 90)))))
    tf))

(define-event face-update ()
  local-transform
  packet)

(defun handle-packet (main packet)
  (when (and (context main) (openseeface-socket main))
    (issue main 'face-update
           :packet packet
           :local-transform (packet-transform packet))
    T))

(defclass openseeface-main (main)
  ((receive-thread :initform NIL :accessor receive-thread)
   (openseeface-socket :initform NIL :accessor openseeface-socket)))

(defmethod initialize-instance :after ((main openseeface-main) &key (openseeface-host "localhost")
                                                                    (openseeface-port 11573))
  (let ((socket (usocket:socket-connect openseeface-host openseeface-port
                                        :protocol :datagram
                                        :element-type '(unsigned-byte 8))))
    (setf (openseeface-socket main) socket)
    (flet ((thunk ()
             (receive-packets (lambda (packet) (handle-packet main packet)) socket)))
      (setf (receive-thread main) (make-thread "openseeface-receiver" #'thunk)))))

(defmethod finalize :after ((main openseeface-main))
  (let ((socket (openseeface-socket main)))
    (when socket
      (setf (openseeface-socket main) NIL)
      (usocket:socket-close socket)
      (wait-for-thread-exit (receive-thread main)))))
