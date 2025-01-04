(in-package #:org.shirakumo.fraf.vtryout)

(defclass speech-detection (mixed:virtual)
  ((main-frequency :initform (vec2) :accessor main-frequency)
   (speech-p :initform NIL :accessor speech-p)))

(defmethod mixed:info ((segment speech-detection))
  (list :name "speech-detection"
        :description "Detects human speech respons in the input."
        :flags 0 :min-inputs 1 :max-inputs 1 :outputs 0 :fields ()))

(defmethod mixed:mix ((segment speech-detection))
  (declare (optimize speed (safety 1)))
  (mixed:with-buffer-tx (data start size (aref (mixed:inputs segment) 0))
    (declare (type (simple-array single-float (*)) data))
    (when (< 0 size)
      (let ((mag 0f0) (freq 0f0))
        (loop for i of-type (unsigned-byte 32) from start below (1- (+ start size)) by 2
              do (when (< mag (aref data (+ i 1)))
                   (setf mag (aref data (+ i 1)))
                   (setf freq (aref data (+ i 0)))))
        (vsetf (the vec2 (main-frequency segment)) freq mag)
        (setf (speech-p segment) (and (<= 300 mag) (<= 100 freq 800))))
      (mixed:finish))))
