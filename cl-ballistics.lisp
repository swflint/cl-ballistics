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

(defun retrieve-a-m (drag-function velocity)
  (ecase drag-function
    (:g1
     (cond
       ((> velocity 4230) (values 1.477404177730177e-04 1.9565))
       ((> velocity 3680) (values 1.920339268755614e-04 1.925))
       ((> velocity 3450) (values 2.894751026819746e-04 1.875))
       ((> velocity 3295) (values 4.349905111115636e-04 1.825))
       ((> velocity 3130) (values 6.530421871892662e-04 1.775))
       ((> velocity 2690) (values 9.748073694078696e-04 1.725))
       ((> velocity 2830) (values 1.453721560187286e-04 1.675))
       ((> velocity 2680) (values 2.162887202930376e-03 1.625))
       ((> velocity 2460) (values 3.209559783129881e-03 1.575))
       ((> velocity 2225) (values 3.904368218691249e-03 1.55))
       ((> velocity 2015) (values 3.222942271262336e-03 1.575))
       ((> velocity 1890) (values 2.203329542297809e-03 1.625))
       ((> velocity 1810) (values 1.511001028891904e-03 1.675))
       ((> velocity 1730) (values 8.609957592468259e-04 1.75))
       ((> velocity 1595) (values 4.086146797305117e-04 1.85))
       ((> velocity 1520) (values 1.954473210037398e-04 1.95))
       ((> velocity 1420) (values 5.431896266462351e-05 2.125))
       ((> velocity 1360) (values 8.847742581674416e-06 2.375))
       ((> velocity 1315) (values 1.456922328720298e-06 2.625))
       ((> velocity 1280) (values 2.419485191895565e-07 2.875))
       ((> velocity 1220) (values 1.657956321067612e-08 3.25))
       ((> velocity 1185) (values 4.745469537157371e-10 3.75))
       ((> velocity 1150) (values 1.379746590025088e-11 4.25))
       ((> velocity 1100) (values 4.070157961147882e-13 4.75))
       ((> velocity 1060) (values 2.938236954847331e-14 5.12))
       ((> velocity 1025) (values 1.228597370774746e-14 5.25))
       ((> velocity  980) (values 2.916938264100495e-14 5.125))
       ((> velocity  945) (values 3.855099424807451e-13 4.75))
       ((> velocity  905) (values 1.185097045689854e-11 4.25))
       ((> velocity  860) (values 3.566129470974951e-10 3.75))
       ((> velocity  810) (values 1.045513263966272e-08 3.25))
       ((> velocity  780) (values 1.291159200846216e-07 2.875))
       ((> velocity  750) (values 6.824429329105383e-07 2.625))
       ((> velocity  700) (values 3.569169672385163e-06 2.375))
       ((> velocity  640) (values 1.839015095899579e-05 2.125))
       ((> velocity  600) (values 5.71117468873424e-05 1.95))
       ((> velocity  550) (values 9.226557091973427e-05 1.875))
       ((> velocity  250) (values 9.337991957131389e-05 1.875))
       ((> velocity  100) (values 7.225247327590413e-05 1.925))
       ((> velocity   65) (values 5.792684957074546e-05 1.975))
       ((> velocity    0) (values 5.206214107320588e-05 2.00))))
    (:g2
     (cond
       ((> velocity 1674) (values 0.0076470052136733 1.36999902851493))
       ((> velocity 1172) (values 1.00419763721974e-03 1.65392237010294))))
    (:g3)
    (:g4)
    (:g5)
    (:g6)
    (:g7)
    (:g8)))

(defun retard (drag-function drag-coefficient velocity)
  (declare (integer drag-function)
           (real drag-coefficient velocity))
  (multiple-value-bind (a m)
      (retrieve-a-m darg-function velocity)
    (let ((val -1)
          (a (if a a -1))
          (m (if m m -1)))
      (if (and (not (= a -1))
             (not (= m -1))
             (> velocity 0)
             (< velocity 10000))
          (/ (* a (expt velocity m))
             drag-coefficient)
          -1))))

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
    (iter)))

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
