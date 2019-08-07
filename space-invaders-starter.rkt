;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname space-invaders-starter) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/universe)
(require 2htdp/image)

;; Space Invaders

;; Constants:

(define WIDTH  300)
(define HEIGHT 500)

(define INVADER-X-SPEED 1.5)  ;speeds (not velocities) in pixels per tick
(define INVADER-Y-SPEED 1.5)
(define TANK-SPEED 2)
(define MISSILE-SPEED 10)

(define HIT-RANGE 10)

(define INVADE-RATE 100)

(define BACKGROUND (empty-scene WIDTH HEIGHT))

(define INVADER
  (overlay/xy (ellipse 10 15 "outline" "blue")              ;cockpit cover
              -5 6
              (ellipse 20 10 "solid"   "blue")))            ;saucer

(define TANK
  (overlay/xy (overlay (ellipse 28 8 "solid" "black")       ;tread center
                       (ellipse 30 10 "solid" "green"))     ;tread outline
              5 -14
              (above (rectangle 5 10 "solid" "black")       ;gun
                     (rectangle 20 10 "solid" "black"))))   ;main body

(define TANK-HEIGHT/2 (/ (image-height TANK) 2))

(define MISSILE (ellipse 5 15 "solid" "red"))

(define BLANK (rectangle WIDTH HEIGHT "outline" "WHITE"))

;; Data Definitions:

(define-struct game (invaders missiles tank))
;; Game is (make-game  (listof Invader) (listof Missile) Tank)
;; interp. the current state of a space invaders game
;;         with the current invaders, missiles and tank position

;; Game constants defined below Missile data definition

#;
(define (fn-for-game s)
  (... (fn-for-loinvader (game-invaders s))
       (fn-for-lom (game-missiles s))
       (fn-for-tank (game-tank s))))



(define-struct tank (x dir))
;; Tank is (make-tank Number Integer[-1, 1])
;; interp. the tank location is x, HEIGHT - TANK-HEIGHT/2 in screen coordinates
;;         the tank moves TANK-SPEED pixels per clock tick left if dir -1, right if dir 1

(define T0 (make-tank (/ WIDTH 2) 1))   ;center going right
(define T1 (make-tank 50 1))            ;going right
(define T2 (make-tank 50 -1))           ;going left

#;
(define (fn-for-tank t)
  (... (tank-x t) (tank-dir t)))



(define-struct invader (x y dx))
;; Invader is (make-invader Number Number Number)
;; interp. the invader is at (x, y) in screen coordinates
;;         the invader along x by dx pixels per clock tick

(define I1 (make-invader 150 100 12))           ;not landed, moving right
(define I2 (make-invader 150 HEIGHT -10))       ;exactly landed, moving left
(define I3 (make-invader 150 (+ HEIGHT 10) 10)) ;> landed, moving right


#;
(define (fn-for-invader invader)
  (... (invader-x invader) (invader-y invader) (invader-dx invader)))


(define-struct missile (x y))
;; Missile is (make-missile Number Number)
;; interp. the missile's location is x y in screen coordinates

(define M1 (make-missile 150 300))                       ;not hit U1
(define M2 (make-missile (invader-x I1) (+ (invader-y I1) 10)))  ;exactly hit U1
(define M3 (make-missile (invader-x I1) (+ (invader-y I1)  5)))  ;> hit U1

#;
(define (fn-for-missile m)
  (... (missile-x m) (missile-y m)))



(define G0 (make-game empty empty T0))
(define G1 (make-game empty empty T1))
(define G2 (make-game (list I1) (list M1) T1))
(define G3 (make-game (list I1 I2) (list M1 M2) T1))



;; =================
;; Functions:



;; game -> game
;; start program by evaluating (main (make-game  empty empty (make-tank (/ WIDTH 2) 1)))
(define (main g)
  (big-bang g
            (on-key handle-key)   ; game Integer Integer MouseEvent -> game
            (on-tick  next)       ; game -> game
            (to-draw  render)     ; game -> Image
            (stop-when didInvaderLand?)))   


;  game -> game
(define (handle-key g a-key)
  (cond ;w - is the previous worldState, V here we change it
    [(key=? a-key "left")  (make-game (game-invaders g) (game-missiles g)(make-tank (- (tank-x (game-tank g)) TANK-SPEED) (- HEIGHT 12)))] 
    [(key=? a-key "right") (make-game (game-invaders g) (game-missiles g) (make-tank (+ (tank-x (game-tank g)) TANK-SPEED) (- HEIGHT 12)))] ;; EKRAN DIŞINI CIKMAMASINI SAGLA
    [(key=? a-key " ")  (make-game (game-invaders g) (cons (make-missile (tank-x (game-tank g))  HEIGHT) (game-missiles g)) (game-tank  g))]
    [else g]))

; game -> Image
(define (render g)
  (place-image (render-invaders (game-invaders g)) (/ WIDTH 2) (/ HEIGHT 2)
                                (place-image (render-missiles (game-missiles g)) (/ WIDTH 2) (/ HEIGHT 2)
                                                              (place-image TANK (tank-x (game-tank g)) (- HEIGHT 12) BACKGROUND))))
(define (render-invaders d)
  (cond [(empty?  d ) BLANK]
        [else
        (place-image INVADER (invader-x (first d)) (invader-y (first  d)) (render-invaders (rest  d)))])) ;;;


(define (render-missiles d)
  (cond [(empty?  d ) BLANK]
        [else
         (place-image MISSILE (missile-x (first d)) (missile-y (first d)) (render-missiles (rest d)))]))
     

; game -> game
(define (next g)
 (removeCollisions (missiles-screen (invade (make-game (next-invaders (game-invaders g)) ;;
             (next-missiles (game-missiles g))
             (game-tank g))))))
            

(define (next-invaders g)
  (cond [(empty? g) empty]
        [else
         (cons (next-invader (first g)) (next-invaders (rest g)))]))

(define (next-invader g )
  (cond [(> (+(invader-x g) (* (invader-dx g) INVADER-X-SPEED)) WIDTH) (make-invader (+ (invader-x g)(* (invader-dx g) INVADER-X-SPEED)) (+ (invader-y g) INVADER-Y-SPEED)
                                                                                  (* -1 (invader-dx g)))]
        [(< (+(invader-x g) (* (invader-dx g) INVADER-X-SPEED)) 0) (make-invader (+ (invader-x g)(* (invader-dx g) INVADER-X-SPEED)) (+ (invader-y g) INVADER-Y-SPEED)
                                                                                  (* -1 (invader-dx g)))]
        [else
         (make-invader (+ (invader-x g) (* (invader-dx g) INVADER-X-SPEED)) (+ (invader-y g) INVADER-Y-SPEED) (invader-dx g))]))


(define (next-missiles g)
  (cond [(empty? g) empty]
        [else
         (cons (next-missile (first g)) (next-missiles (rest g)))]))
(define (next-missile g )
  (make-missile (missile-x g) (- (missile-y g) MISSILE-SPEED )))

;game -> game
(define (invade g)
  (if (integer? (/ (/ (random 51)  21) 3))
      (make-game (cons (make-invader (random WIDTH) 0 1) (game-invaders g)) (game-missiles g) (game-tank g) )
      g ))


(define (missiles-screen g)
  (make-game (game-invaders g) (clearMissiles (game-missiles g)) (game-tank g)))

(define (clearMissiles lom)
  (cond [(empty? lom) empty]
        [else
         (cond [(< (missile-y (first lom)) 2) (clearMissiles (rest lom))]
               [else
                (cons (first lom) (clearMissiles (rest lom)))])]))

(define (removeCollisions g)
  (make-game
   (filterInvaders (game-invaders g) (game-missiles g))
   (filterMissiles (game-missiles g) (game-invaders g))
   (game-tank g)))
; list of invaders and missiles -> list of invaders
(define (filterInvaders loi lom)
  (cond [(empty? loi) empty]
        [else
         (cond [(invCollides? (first loi) lom) (filterInvaders (rest loi) lom)]
               [else
                (cons (first loi) (filterInvaders (rest loi) lom))])]))
; invader, list of missiles -> boolean
(define (invCollides? i lom)
  (cond [(empty? lom) false]
        [else
         (if (and (<= (abs (- (missile-x (first lom)) (invader-x i))) HIT-RANGE)  (<= (abs (- (missile-y (first lom)) (invader-y i))) HIT-RANGE) )
             true
             (invCollides? i (rest lom)))]))
; list of invaders and missiles -> list of missiles
(define (filterMissiles lom loi)
  (cond [(empty? lom) empty]
        [else
         (cond [(mslCollides? (first lom) loi) (filterMissiles (rest lom) loi)]
               [else
                (cons (first lom) (filterMissiles (rest lom) loi))])]))
; invader, list of missiles -> boolean
 (define (mslCollides? m loi)
  (cond [(empty? loi) false]
        [else
         (if (and (<= (abs (- (invader-x (first loi)) (missile-x m)))HIT-RANGE) (<= (abs (- (missile-y m) (invader-y (first loi)))) HIT-RANGE))
             true
             (mslCollides? m (rest loi)))]))
(define (didInvaderLand? g)
  (checkInvader(game-invaders g)))


;; checkInvader
;; listOfInvaders -> Boolean
;; checks if any of the invaders in the list have reached the bottom edge
;; tests from didInvaderLand? apply

(define (checkInvader loi)
  (cond [(empty? loi) false]
        [else
         (if (>= (invader-y (first loi)) HEIGHT)
             true
             (checkInvader (rest loi)) )]))
  
(main (make-game (cons (make-invader (random WIDTH) 0 1) empty) empty  (make-tank 14 1)))

