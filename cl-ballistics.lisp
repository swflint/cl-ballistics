;;;; cl-ballistics.lisp

(in-package #:cl-ballistics)

;;; Constant definitions
(defconstant +gravity+ -32.194)

(defconstant +standard-pressure+ 29.92)

(defconstant +to-radians+ (/ pi 180))

(defconstant +to-degrees+ (/ 180 pi))

(defconstant +standard-altitude+ 0)

(defconstant +standard-temperature+ 59)

(defconstant +standard-humidity+ 1)

;;; Alist of Drag Functions
(defconstant +drag-functions+
  '((:g1 . "Ingalls, flatbase")
    (:g2 . "Aberdeen \"J\" Projectile")
    (:g3 . "")
    (:g4 . "")
    (:g5 . "Short, 7.5° boat-tail")
    (:g6 . "Flatbase, 6 Calibers long")
    (:g7 . "Long 7.5° boat-tail")
    (:g8 . "Flatbase, 10 Calibers long")))

;;; Solution class

(defclass ballistic-solution ()
  ())

;;; Atmospheric Functions

(defun calc-fp (pressure)
  (declare (real pressure))
  (/ (- pressure +standard-pressure+) +standard-pressure+))

(defun calc-fr (temperature pressure relative-humidity)
  (declare (real temperature pressure relative-humidity))
  (let ((vpw (- (* 4e-6 (expt temperature 3))
                (+ (* 4e-4 (expt temperature 2))
                   (* 0.0234 temperature))
                0.2517)))
    (* 0.995
       (/ pressure (* (- pressure 0.3783)
                      relative-humidity
                      vpw)))))

(defun calc-ft (temperature altitude)
  (declare (real temperature altitude))
  (let ((tstd (+ (* -0.0036 altitude) 59)))
    (/ (- temperature tstd)
       (- 459.6 tstd))))

(defun calc-fa (altitude)
  (declare (real altitude))
  (/ 1 (+ (* -4e-15 (expt altitude 3))
          (* 4e-10 (expt altitude 2))
          (* -3e-5 altitude)
          1)))

(defun atmospheric-correction (drag-coefficient &optional (altitude +standard-altitude+) (pressure +standard-pressure+) (temperature +standard-temperature+) (relative-humidity +standard-humidity+))
  (declare (real drag-coefficient altitude pressure temperature relative-humidity))
  (let* ((altitude-correction (calc-fa altitude))
         (temperature-correction (calc-ft temperature altitude))
         (humidity-correction (calc-fr temperature pressure relative-humidity))
         (pressure-correction (calc-fp pressure))
         (correction-factor (* altitude-correction
                               (- (+ 1 temperature-correction)
                                  pressure-correction)
                               humidity-correction)))
    (* drag-coefficient correction-factor)))

(export 'atmospheric-correction)

;;; G-function retardation

;; (defun retard (drag-function drag-coefficient velocity)
;;   (declare (integer drag-function)
;;            (real drag-coefficient velocity))
;;   (let (())
;;     ))

;;; Angular conversion functions

(defun degrees-to-moa (degrees)
  "Converts degrees to Minutes of Angle."
  (declare (real degrees))
  (* degrees 60))

(defun degrees-to-radians (degrees)
  "Converts Degrees to Radians"
  (declare (real degrees))
  (* degrees +to-radians+))

(defun moa-to-degrees (moa)
  "Converts Minutes of Angle to Degrees"
  (declare (real moa))
  (/ moa 60))

(defun moa-to-radians (moa)
  "Converts Minutes of Angle to Radians"
  (declare (real moa))
  (degrees-to-radians (moa-to-degrees moa)))

(defun radians-to-degrees (radians)
  "Converts Radians to Degrees"
  (declare (real radians))
  (* radians +to-degrees+))

(defun radians-to-moa (radians)
  "Converts Radians to Minutes of Angle"
  (declare (real radians))
  (degrees-to-moa (radians-to-degrees radians)))

(export '(degrees-to-moa
          degrees-to-radians
          moa-to-degrees
          moa-to-radians
          radians-to-degrees
          radians-to-moa))

;;; Windage functions

(defun windage (wind-speed initial-velocity range time)
  "Calculates the windage correction, in inches, to achive zero on
   target at given range."
  (declare (real wind-speed initial-velocity range time))
  (let ((wind-speed-inches-per-second (* wind-speed 17.60)))
    (* wind-speed-inches-per-second (/ (- time range) initial-velocity))))

(defun head-wind (wind-speed wind-angle)
  "Calculates the Headwind component in miles per hour"
  (declare (real wind-speed wind-angle))
  (* (cos (degrees-to-radians wind-angle))
     wind-speed))

(defun cross-wind (wind-speed wind-angle)
  "Calculates the crosswind components in miles per hour"
  (* (sin (degrees-to-radians wind-angle))
     wind-speed))

(export '(windage
          head-wind
          cross-wind))

;;; Bore angle functions

(defun zero-angle (drag-function ballistic-coefficient initial-velocity sight-height zero-range y-intercept)
  "Calculates the angle of the bore, relative to the sighting system in degrees."
  (declare (keyword drag-function)
           (real ballistic-coefficient
                 initial-velocity
                 sight-height
                 zero-range
                 y-intercept))
  (let ((nit 0)
        (dt (/ 1 initial-velocity))
        (y (/ sight-height 12))
        (x 0)
        (da 0)
        (v 0) (vx 0) (vy 0)
        (vx1 0) (vy1 0)
        (dv 0) (dvx 0) (dvy 0)
        (gx 0) (gy 0)
        (angle 0)
        (quit 0))
    (declare (real nit
                   dt
                   y
                   x
                   da
                   v vx vy
                   vx1 vy1
                   dv dvx dvy
                   gx gy
                   angle)
             (integer quit))
    (loop for angle = (+ angle da)
       if (= quit 1)
       return (radians-to-degrees angle)
       else
         )))

;;; Solving

(defun solve-all (drag-function drag-coefficient initial-velocity sight-height shooting-angle zero-angle wind-speed wind-angle)
  "Generates a ballistic solutions table."
  (declare (keyword drag-function)
           (real drag-coefficient
                 initial-velocity
                 sight-height
                 shooting-angle
                 zero-angle
                 wind-speed
                 wind-angle)))

;;; Retrieving Data

(defun get-range (solution yardage)
  "Retrieve range in yards.")

(defun get-path (solution yardage)
  "Retrieves projectile pathin inches, relative to the line of sight at yardage.")

(defun get-moa (solution yardage)
  "Retrieves the estimated elevation correction for zero at specified range.
Very useful for \"click charts\" and similar.")

(defun get-time (solution yardage)
  "Retrieves the time of flight to this range.")

(defun get-windage (solution yardage)
  "Retrives the windage correction, in inches, required to achieve
zero at this range.")

(defun get-windage-moa (solution yardage)
  "Retrieves windage correction in MOA to achieve zero at this range.")

(defun get-velocity (solution yardage)
  "Retrieves velocity of projectile at specified yardage.")

(defun get-velocity-bore (solution yardage)
  "Retrives projectile's velocity in the direction of the bore.")

(defun get-velocity-perpendicular (solution yardage)
  "Retireves the velocity of the projectile perpendicular to the
direction of the bore.")
