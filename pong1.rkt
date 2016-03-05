;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname pong1) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))
(require 2htdp/image)
(require 2htdp/universe)


;;=====================================================================
;; CONSTANTS
;;=====================================================================

;; numeric constants refer to pixels and must be natural

(define BG-COLOR "black")
(define DETAIL "white")

(define WIDTH 300)
(define HEIGHT 200)
(define MID-W (/ WIDTH 2))
(define MID-H (/ HEIGHT 2))

(define MTS (add-line
             (add-line
              (add-line 
               (rectangle WIDTH HEIGHT "solid" BG-COLOR)      ; background 
               MID-W 0 MID-W HEIGHT DETAIL)                   ; center line
              0 0 WIDTH 0 DETAIL)                             ; top border
             0 HEIGHT WIDTH HEIGHT DETAIL))                   ; bottom border

(define FONT 10)

(define RADIUS 2)

(define BALL (circle RADIUS "solid" DETAIL))
(define BALL-SPEED 2)

(define PADDLE-WIDTH 2)
(define PADDLE-HEIGHT 30)
(define PADDLE (rectangle PADDLE-WIDTH PADDLE-HEIGHT "solid" DETAIL))

(define PADDLE-SPEED 10)

(define PADDLE-OFFSET (* 2 PADDLE-WIDTH))
(define PADDLE1-X PADDLE-OFFSET)
(define PADDLE2-X (- WIDTH PADDLE-OFFSET))
(define PADDLE-MID (/ PADDLE-HEIGHT 2))

(define MIN-PADDLE-Y (/ 2 PADDLE-HEIGHT))
(define MAX-PADDLE-Y (- HEIGHT (/ 2 PADDLE-HEIGHT)))

;;=====================================================================
;; DATA DEFINITIONS
;;=====================================================================

;; Paddle is Natural[(- HEIGHT (/ 2 PADDLE-HEIGHT)), (/ 2 PADDLE-HEIGHT)]
;; interp. the y position of the paddle, in screen coordinates
(define P1 MIN-PADDLE-Y)                   ;top
(define P2 MAX-PADDLE-Y)                   ;bottom
(define P3 MID-H)                          ;center

#;
(define (fn-for-paddle p)
  (... p))


(define-struct points (p1 p2))
;; Points is (make-points Natural Natural)
;; interp. the number of points each player has
(define PNT1 (make-points 0 0))
(define PNT2 (make-points 1 0))
(define PNT3 (make-points 0 1))
(define PNT4 (make-points 10 0))
(define PNT5 (make-points 100 0))

#;
(define (fn-for-points p)
  (... (points-p1 p)
       (points-p2 p)))


(define-struct ball (x y xv yv))
;; Ball is (make-ball Integer Natural[(- HEIGHT RADIUS), RADIUS] Number Number)
;;interp x,y pos of the ball, in screen coordinates, with trajectory xv, yv
(define B1 (make-ball MID-W MID-H 1 1))  ;center of screen, moving right/up
(define B2 (make-ball MID-W MID-H -1 -1)) ;center of screen, moving left/down
(define B3 (make-ball (- RADIUS) MID-H -1 -1)) ;off screen, moving left/down

#;
(define (fn-for-ball b)
  (... (ball-x b)
       (ball-y b)
       (ball-xv b)
       (ball-yv b)))


(define-struct game (p1 p2 pnts ball))
;; Game is (make-game Paddle Points Paddle Points Ball)
(define G1 (make-game P3 P3 PNT1 B1)) ;start of game
(define G2 (make-game P3 P3 PNT1 B2)) ;alt start of game
(define G3 (make-game P3 P3 PNT1 B3)) ;p2 about to get point
(define G4 (make-game P1 P2 PNT4 B1))
(define G5 (make-game P1 P2 PNT5 B2))

#;
(define (fn-for-game g)
  (... (fn-for-paddle (game-p1 g))
       (fn-for-paddle (game-p2 g))
       (fn-for-points (game-pnts g))
       (fn-for-ball (game-ball g))))



;;=====================================================================
;; FUNCTIONS
;;=====================================================================

;; Game -> Game
;; start the world with a game
;; 
(define (main g)
  (big-bang g                           ; Game
            (on-tick   tock)            ; Game -> Game
            (to-draw   render)          ; Game -> Image
            (on-key    handle-key)      ; Game KeyEvent -> Game
            (name "Pong")))

;; Game -> Game
;; produce the next game
(check-expect (tock (make-game P3 P3 PNT1 (make-ball MID-W MID-H 1 1))) 
              (make-game P3 P3 PNT1 (make-ball (+ MID-W 1) (+ MID-H 1) 1 1)))
;(check-expect (tock G3) (make-game P3 P3 PNT3 B1))
;(check-expect (tock (make-game P3 P3 PNT1 (make-ball (+ WIDTH RADIUS) MID-H 1 1))) 
;              (make-game P3 P3 PNT2 B1))
(check-expect (tock (make-game P3 P3 PNT1 (make-ball (- PADDLE2-X RADIUS) MID-H 1 1)))
              (make-game P3 P3 PNT1 (make-ball (- PADDLE2-X RADIUS 1) (+ MID-H 1) -1 1)))

;(define (tock g) g)

(define (tock g)
  (make-game (game-p1 g)
       (game-p2 g)
       (check-points (game-pnts g) (game-ball g))
       (tock-ball (game-ball g) (game-p1 g) (game-p2 g))))



;; Points Ball -> Points
;; add points if ball is out of bounds
(check-expect (check-points PNT1 B1) PNT1)
(check-expect (check-points PNT1 B3) PNT3)
(check-expect (check-points PNT1 (make-ball (+ WIDTH RADIUS) MID-H 1 1))
              PNT2)
;(define (check-points p b) p)

; template from Points and Ball

(define (check-points p b)
  (make-points (if (>= (ball-x b) (+ WIDTH RADIUS))
                   (+ 1 (points-p1 p))
                   (points-p1 p))
               (if (<= (ball-x b) (- RADIUS))
                       (+ 1 (points-p2 p))
                       (points-p2 p))))



;; Ball Paddle Paddle -> Ball
;; move the ball based on its current velocity. change velocity if collision
(check-expect (tock-ball (make-ball MID-W MID-H 1 1) P3 P3)
              (make-ball (+ MID-W 1) (+ MID-H 1) 1 1))
(check-expect (tock-ball (make-ball MID-W MID-H -1 -1) P3 P3)
              (make-ball (- MID-W 1) (- MID-H 1) -1 -1))
(check-expect (tock-ball (make-ball (- PADDLE2-X RADIUS) MID-H 1 1) P3 P3)
              (make-ball (- PADDLE2-X RADIUS 1) (+ MID-H 1) -1 1))
;(check-expect (tock-ball B3 P3 P3) B1)
              
;(define (tock-ball b p1 p2) b) 

; template from ball

(define (tock-ball b p1 p2)
  (cond [(out-of-bounds? b) (make-ball MID-W MID-H (sub1 (random BALL-SPEED)) (sub1 (random BALL-SPEED)))]
        [(collision-paddle? b p1 p2) (make-ball (+ (ball-x b) (- (ball-xv b)))
                                                (+ (ball-y b) (ball-yv b))
                                                (- (ball-xv b))
                                                (ball-yv b))]
        [(collision-wall? b p1 p2) (make-ball (+ (ball-x b) (ball-xv b))
                                              (+ (ball-y b) (- (ball-yv b)))
                                              (ball-xv b)
                                              (- (ball-yv b)))]
        [else
         (make-ball (+ (ball-x b) (ball-xv b))
                    (+ (ball-y b) (ball-yv b))
                    (ball-xv b)
                    (ball-yv b))]))
;; Ball -> Boolean
;; produce true if ball is outside of the board
(check-expect (out-of-bounds? B3) true)
(check-expect (out-of-bounds? B1) false)

;(define (out-of-bounds? b) false)

(define (out-of-bounds? b)
  (or (>= (ball-x b) (+ WIDTH RADIUS))
      (<= (ball-x b) (- 0 RADIUS))))


;; Ball Paddle Paddle -> Boolean
;; check if ball collided with a paddle
(check-expect (collision-paddle? (make-ball (- PADDLE2-X RADIUS) MID-H 1 1) P3 P3)
              true)
(check-expect (collision-paddle? B1 P3 P3) false)
;(define (collision-paddle? b p1 p2) false)

(define (collision-paddle? b p1 p2)
  (or (and (>= (ball-x b) PADDLE1-X)
           (<= (ball-x b) (+ RADIUS PADDLE1-X))
           (>= (ball-y b) (- p1 (/ PADDLE-HEIGHT 2)))
           (<= (ball-y b) (+ p1 (/ PADDLE-HEIGHT 2))))
      (and (<= (ball-x b) PADDLE2-X)
           (>= (ball-x b) (- PADDLE2-X RADIUS))
           (>= (ball-y b) (- p2 (/ PADDLE-HEIGHT 2)))
           (<= (ball-y b) (+ p2 (/ PADDLE-HEIGHT 2))))))



;; Ball Paddle Paddle -> Boolean
;; check if ball collided with a wall

;(define (collision-wall? b p1 p2) false)

(define (collision-wall? b p1 p2)
  (or (>= (ball-y b) (- HEIGHT RADIUS))
      (<= (ball-y b) RADIUS)))
           
           
           
;; Game -> Image
;; render current state of the game 
(check-expect (render G1) (place-image 
                           PADDLE PADDLE1-X P3
                           (place-image
                            PADDLE PADDLE2-X P3
                            (place-image 
                             (text (number->string 0) FONT "white")
                             (- MID-W (image-width (text (number->string 0) FONT "white")))
                             FONT
                             (place-image 
                              (text (number->string 0) FONT "white")
                              (+ MID-W (image-width (text (number->string 0) FONT "white")))
                              FONT
                              (place-image 
                               BALL MID-W MID-H MTS))))))

;(define (render g) empty-image)

(define (render g)
  (place-image PADDLE PADDLE1-X (game-p1 g)
               (place-image PADDLE PADDLE2-X (game-p2 g)
                            (place-points (game-pnts g)
                                          (place-image BALL (ball-x (game-ball g)) (ball-y (game-ball g)) MTS)))))

;; Points Image -> Image
;; convert points to images and place on another image

(define (place-points p i)
  (local [(define (number->text n)
            (text (number->string n) FONT DETAIL))
          (define i1 (number->text (points-p1 p)))
          (define i2 (number->text (points-p2 p)))
          (define (place-points p i)
            (place-image i1 (- MID-W (image-width i1)) FONT
                       (place-image i2 (+ MID-W (image-width i2)) FONT i)))]
    (place-points p i)))


;; Game KeyEvent -> Game
;; move paddle 1 if W or S pressed
;; move paddle 2 if "up" or "down" pressed

;(define (handle-key g ke) g)

(define (handle-key g ke)
  (cond [(key=? ke "w")    (paddle1-up g)]
        [(key=? ke "s")    (paddle1-down g)]
        [(key=? ke "up")   (paddle2-up g)]
        [(key=? ke "down") (paddle2-down g)]
        [else 
          g]))

;; Game -> Game
;; move paddle 1 up

(define (paddle1-up g)
  (make-game (move-paddle (- PADDLE-SPEED) (game-p1 g))
             (game-p2 g)
             (game-pnts g)
             (game-ball g)))


;; Game -> Game
;; move paddle 1 down

(define (paddle1-down g)
  (make-game (move-paddle PADDLE-SPEED (game-p1 g))
             (game-p2 g)
             (game-pnts g)
             (game-ball g)))


;; Game -> Game
;; move paddle 2 up

(define (paddle2-up g)
  (make-game (game-p1 g)
             (move-paddle (- PADDLE-SPEED) (game-p2 g))
             (game-pnts g)
             (game-ball g)))


;; Game -> Game
;; move paddle 2 down

(define (paddle2-down g)
  (make-game (game-p1 g)
             (move-paddle PADDLE-SPEED (game-p2 g))
             (game-pnts g)
             (game-ball g)))

;; Natural Paddle -> Paddle
;; move a paddle
(define (move-paddle n p)
  (+ p n))


(main (make-game P3 P3 PNT1 (make-ball MID-W MID-H BALL-SPEED BALL-SPEED)))