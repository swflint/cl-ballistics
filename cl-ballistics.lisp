;;;; cl-ballistics.lisp

(in-package #:cl-ballistics)

;;; "cl-ballistics" goes here. Hacks and glory await!

;;; Constant definitions
(defconstant +gravity+ -32.194)

(defconstant +standard-pressure+ 29.92)

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

;;; Angular conversion functions
;; degree-to-moa : *60
;; degree-to-radian :  * (π / 180)
;; moa-to-degree : / 60
;; moa-to-radian : / 60 * (π / 180)
;; radian-to-degree : * (180 / π)
;; radian-to-moa : * 60 * (180 / π)
