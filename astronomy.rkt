#lang racket
(require racket/date)
(require rnrs/base-6)
(require rackunit)

;; Redefine julian day calculation. It was giving incorrect results for
;; 1800-1899. Uses method described here:
;; http://quasar.as.utexas.edu/BillInfo/JulianDatesG.html
(define (date->julian/scalinger date)
  (define day (date-day date))
  (define month (+ 3 (remainder (+ 9 (date-month date)) 12)))
  (define year (- (date-year date) (floor (/ (- month 1) 12))))
  (define century (floor (/ year 100)))

  (+ (+ 2 (- (floor (/ century 4)) century))
     day
     (floor (* 365.25 (+ year 4716)))
     (floor (* 30.6001 (+ month 1)))
     -1524.5))
(define date->julian/scaliger date->julian/scalinger)

;;Inverse of above, converts julian day numbers into dates
;;Does not calculate times, so all date numbers must = N.5
(define (julian/scalinger->date date)
  (define Z (ceiling date))
  (define W (floor (/ (- Z 1867216.25) 36524.25)))
  (define A (- (+ Z 1 W) (floor (/ W 4))))
  (define B (+ A 1524))
  (define C (floor (/ (- B 122.1) 365.25)))
  (define D (floor (* C 365.25)))
  (define E (floor (/ (- B D) 30.6001)))
  (define day (- B
                 (floor (* 30.6001 E))
                 D))
  (define month (+ 1 (remainder (- E 2) 12)))
  (define year (+ (- C 4716) (floor (/ (- E 2) 12))))
  (date* 0 0 0 (inexact->exact day) (inexact->exact month) (inexact->exact year) 0 0 #f 0 0 ""))
(define julian/scaliger->date julian/scalinger->date)

(check-equal? 2453495.5
              (date->julian/scaliger (date* 0 0 0 5 5 2005 0 0 #f 0 0 "")))
(check-equal? 2378854.5
              (date->julian/scaliger (date* 0 0 0 25 12 1800 0 0 #f 0 0 "")))
(check-equal? 2448783.5
              (date->julian/scaliger (date* 0 0 0 10 6 1992 0 0 #f 0 0 "")))
(check-equal? 2387278.5
              (date->julian/scaliger (date* 0 0 0 18 1 1824 0 0 #f 0 0 "")))
(check-equal? 2451625.5
              (date->julian/scaliger (date* 0 0 0 22 3 2000 0 0 #f 0 0 "")))

(check-equal? (julian/scaliger->date 2453495.5)
              (date* 0 0 0 5 5 2005 0 0 #f 0 0 ""))
(check-equal? (julian/scaliger->date 2378854.5)
              (date* 0 0 0 25 12 1800 0 0 #f 0 0 ""))
(check-equal? (julian/scaliger->date 2448783.5)
              (date* 0 0 0 10 6 1992 0 0 #f 0 0 ""))
(check-equal? (julian/scaliger->date 2387278.5)
              (date* 0 0 0 18 1 1824 0 0 #f 0 0 ""))
(check-equal? (julian/scaliger->date 2451625.5)
              (date* 0 0 0 22 3 2000 0 0 #f 0 0 ""))

;;-------------------------------------------------------------
;; Determine position of the sun on a given date
;; Refer to
;; http://farside.ph.utexas.edu/Books/Syntaxis/Almagest/node33.html
;; for explanations of the formulas used.
;;-------------------------------------------------------------

;; epoch J2000 (2000-01-01 12:00:00 *in the Julian calendar*
;; used as t[0]
(define EPOCH 2451545.0)

;; mean solar longitude at epoch (see above)
;; mean solar longitude lets you ignore longitude when
;; calculating equinox (also time zone, DST, etc.)
(define MEAN_SOLAR_LONGITUDE 280.458)

;; anomaly at epoch
;; TODO: what is anomaly exactly?
(define ANOMALY_0 357.588)

;; rate of motion in mean longitude
;; degrees per day the sun moves from the perspective of earth
(define RATE_OF_PROGRESSION 0.98564735)
(define MEAN_RATE_OF_PROGRESSION 0.98560025)

;; How "oval" the orbit is
(define ORBITAL_ECCENTRICITY 0.016711)

;; number of days since epoch (see above) of a given date
(define (get-delta-time date)
  (- date
     EPOCH))

(check-equal? 1950.5 (get-delta-time (date->julian/scaliger (date* 0 0 0 5 5 2005 0 0 #f 0 0 ""))))
(check-equal? -72690.5 (get-delta-time (date->julian/scaliger (date* 0 0 0 25 12 1800 0 0 #f 0 0 ""))))

;; get better estimate of solar longitude, given a date
;; to consider (see get-delta-time)
(define (estimated-solar-longitude date)
  (mod
   (+ MEAN_SOLAR_LONGITUDE
      (* RATE_OF_PROGRESSION
         (get-delta-time date)))
   360))

(test-= "" 42.961
        (estimated-solar-longitude (date->julian/scaliger (date* 0 0 0 5 5 2005 0 0 #f 0 0 "")))
        0.003)
(test-= "" 273.259
        (estimated-solar-longitude (date->julian/scaliger (date* 0 0 0 25 12 1800 0 0 #f 0 0 "")))
        0.003)

;; get estimated anomaly given the date to consider
(define (get-estimated-anomaly date)
  (degrees->radians
   (mod
    (+ ANOMALY_0
       (* MEAN_RATE_OF_PROGRESSION
          (get-delta-time date)))
    360)))

(test-= "" 2.09441
        (get-estimated-anomaly (date->julian/scaliger (date* 0 0 0 5 5 2005 0 0 #f 0 0 "")))
        0.00003)
(test-= "" 6.17521
        (get-estimated-anomaly (date->julian/scaliger (date* 0 0 0 25 12 1800 0 0 #f 0 0 "")))
        0.00003)

;; Calculate equation of center using estimated anomaly
(define (get-offset date)
  (radians->degrees
   (+ (* 2 ORBITAL_ECCENTRICITY (sin (get-estimated-anomaly date)))
      (* 5/4 (sqr ORBITAL_ECCENTRICITY) (sin (* 2 (get-estimated-anomaly date)))))))

(test-= "" 1.641
        (get-offset (date->julian/scaliger (date* 0 0 0 5 5 2005 0 0 #f 0 0 "")))
        0.009)
(test-= "" -0.204
        (get-offset (date->julian/scaliger (date* 0 0 0 25 12 1800 0 0 #f 0 0 "")))
        0.009)

;; use offset and estimated longitude to get actual ecliptic longitude
(define (get-ecliptic-longitude date)
  (mod (+ (get-offset date) (estimated-solar-longitude date)) 360))

(test-= "" 44.602
        (get-ecliptic-longitude (date->julian/scaliger(date* 0 0 0 5 5 2005 0 0 #f 0 0 "")))
        0.01)
(test-= "" 273.055
        (get-ecliptic-longitude (date->julian/scaliger(date* 0 0 0 25 12 1800 0 0 #f 0 0 "")))
        0.01)


;;-------------------------------------------------------------
;; Determine the dates of astrological events (solstices and
;; equinoxes). Uses reverse process of above.
;; Refer to
;; http://farside.ph.utexas.edu/Books/Syntaxis/Almagest/node33.html
;; for explanations of the formulas used.
;;-------------------------------------------------------------

;; get estimated date of an astrological event assuming every
;; year matches epoch
;; given a degree representation of the event we want:
;; 0 = vernal equinox
;; 90 = summer solstice
;; 180 = autumnal equinox
;; 270 = winter solstice
;; returns julian date of that event
(define (estimated-date-of-event event)
  (round (+ EPOCH
     (/ (- (+ 360 event)
           MEAN_SOLAR_LONGITUDE)
        RATE_OF_PROGRESSION))))

;;Returns better estimate of the date of event,
;;taking into account the solar longitude *at
;;around the time of the event*
(define (date-of-event event)
  (- (estimated-date-of-event event)
     (/ (-
         (get-ecliptic-longitude (estimated-date-of-event event))
         event)
        RATE_OF_PROGRESSION)))

(test-= ""
        (date->julian/scaliger (date* 0 0 0 20 3 2000 0 0 #f 0 0 ""))
        (date-of-event 0)
        1)

(test-= ""
        (date->julian/scaliger (date* 0 0 0 21 6 2000 0 0 #f 0 0 ""))
        (date-of-event 90) 1)

(test-= ""
        (date->julian/scaliger (date* 0 0 17 22 9 2000 0 0 #f 0 0 ""))
        (date-of-event 180) 1)

(test-= ""
        (date->julian/scaliger (date* 0 0 14 21 12 2000 0 0 #f 0 0 ""))
        (date-of-event 270) 1)