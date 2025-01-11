(in-package #:org.shirakumo.fraf.vtryout)

(defclass speech-detection (mixed:virtual)
  ((main-frequency :initform (vec3) :accessor main-frequency)
   (volume-range :initform (setting :audio :volume-range) :accessor volume-range)
   (frequency-range :initform (setting :audio :frequency-range) :accessor frequency-range)
   (speech-p :initform NIL :accessor speech-p)))

(defmethod mixed:info ((segment speech-detection))
  (list :name "speech-detection"
        :description "Detects human speech respons in the input."
        :flags 0 :min-inputs 1 :max-inputs 1 :outputs 0 :fields ()))

(defun normalize (x lo hi)
  (/ (- (clamp lo x hi) lo) (- hi lo)))

(defmethod mixed:mix ((segment speech-detection))
  (declare (optimize speed (safety 1)))
  (mixed:with-buffer-tx (data start size (aref (mixed:inputs segment) 0))
    (declare (type (simple-array single-float (*)) data))
    (when (< 0 size)
      (let ((mag 0f0) (freq 0f0))
        (destructuring-bind (lo . hi) (frequency-range segment)
          (declare (type single-float lo hi))
          (loop for i of-type (unsigned-byte 32) from start below (1- (+ start size)) by 2
                do (when (and (<= lo (aref data (+ i 0)) hi) (< mag (aref data (+ i 1))))
                     (setf mag (aref data (+ i 1)))
                     (setf freq (aref data (+ i 0))))))
        (let ((volume (destructuring-bind (lo . hi) (volume-range segment) (normalize mag lo hi))))
          (vsetf (the vec3 (main-frequency segment)) freq mag volume)
          (setf (speech-p segment) (< 0.0 volume))))
      (mixed:finish))))
