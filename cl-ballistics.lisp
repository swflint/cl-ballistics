;;;; cl-ballistics.lisp

(in-package #:cl-ballistics)

;;; "cl-ballistics" goes here. Hacks and glory await!

;;; Constant definitions
(defconstant +gravity+ -32.194)

(defconstant +standard-pressure+ 29.92)

(defconstant +to-radians+ (/ pi 180))

(defconstant +to-degrees+ (/ 180 pi))

;;; Atmospheric Functions

(defun calc-fp (pressure)
  (declare (rational pressure))
  (/ (- pressure +standard-pressure+) +standard-pressure+))

(defun calc-fr (temperature pressure relative-humidity)
  (declare (rational temperature pressure relative-humidity))
  (let ((vpw (- (* 4e-6 (expt temperature 3))
                (+ (* 4e-4 (expt temperature 2))
                   (* 0.0234 temperature))
                0.2517)))
    (* 0.995
       (/ pressure (* (- pressure 0.3783)
                      relative-humidity
                      vpw)))))

(defun calc-ft (temperature altitude)
  (declare (rational temperature altitude))
  (let ((tstd (+ (* -0.0036 altitude) 59)))
    (/ (- temperature tstd)
       (- 459.6 tstd))))

(defun calc-fa (altitude)
  (declare (rational altitude))
  (/ 1 (+ (* -4e-15 (expt altitude 3))
          (* 4e-10 (expt altitude 2))
          (* -3e-5 altitude)
          1)))

(defun atmospheric-correction (drag-coefficient altitude pressure temperature relative-humidity)
  (declare (rational drag-coefficient altitude pressure temperature relative-humidity))
  (let* ((altitude-correction (calc-fa altitude))
         (temperature-correction (calc-ft temperature altitude))
         (humidity-correction (calc-fr temperatuer pressure relative-humidity))
         (pressure-correction (calc-fp pressure))
         (correction-factor (* altitude-correction
                               (- (+ 1 temperature-correction)
                                  pressure-correction)
                               humidity-correction)))
    (* drag-coefficient correction-factor)))

(export 'atmospheric-correction)

;;; G-function retardation

;;; Angular conversion functions

(defun degrees-to-moa (degrees)
  (declare (real degrees))
  (* degrees 60))

(defun degrees-to-radians (degrees)
  (declare (real degrees))
  (* degrees +to-radians+))

(defun moa-to-degrees (moa)
  (declare (real moa))
  (/ moa 60))

(defun moa-to-radians (moa)
  (declare (real moa))
  (degrees-to-radians (moa-to-degrees moa)))

(defun radians-to-degrees (radians)
  (declare (real radians))
  (* radians +to-degrees+))

(defun radians-to-moa (radians)
  (declare (real radians))
  (degrees-to-moa (radians-to-degrees radians)))

(export '(degrees-to-moa
          degrees-to-radians
          moa-to-degrees
          moa-to-radians
          radians-to-degrees
          radians-to-moa))
