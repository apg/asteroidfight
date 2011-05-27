(ns asteroidfight.core
  (:use clojure.contrib.import-static
        [clojure.contrib.seq-utils :only (includes?)]
        [clojure.contrib.def :only (defn-memo)]
        [clojure.pprint :only (pprint)])
  (:import (java.awt Color Dimension Polygon)
           (javax.swing JPanel JFrame Timer JOptionPane)
           (java.awt.event ActionListener KeyListener MouseListener)))

(import-static java.awt.event.KeyEvent VK_LEFT VK_RIGHT VK_UP VK_DOWN VK_SPACE)

(def TWOPI (* Math/PI 2))
(def HALFPI (* Math/PI 0.5))
(def ONEANDHALFPI (* Math/PI 1.5))

(def game-frame-rate 30)
(def game-tick-time (Math/floor (/ 1000 game-frame-rate)))

(def asteroid-speed 0.5)
(def asteroid-min-size 50)

(def bullet-lifetime 20) ; ticks
(def bullet-speed 20) ; pixels/tick

(def ship-max-energy 100)
(def ship-max-speed 5)
(def ship-speed-decay 0.01)

(defn-memo color
  "Creates and memoizes a Color for fast allocation"
  ([r g b] (Color. r g b))
  ([r g b a] (Color. r g b a)))

;; benefits to using a struct here?
(defn point 
  [x y]
  {:x x :y y})

(defn radians-of 
  "When pt is a unit-vector, compute radians"
  [pt]
  (let [x (:x pt)
        y (:y pt)]
    (cond
      (> x 0) (if (>= y 0)
                (Math/atan (/ y x))
                (+ TWOPI (Math/atan (/ y x))))
      (== x 0) (cond 
                (> y 0) HALFPI
                (== y 0) 0
                :else ONEANDHALFPI)
      :else (+ (Math/atan (/ y x)) Math/PI))))

(defn vec2d-of [rads]
  (point (Math/cos rads) (Math/sin rads)))

(defn rotate
  [pt rads]
  (let [x (:x pt)
        y (:y pt)
        sr (Math/sin rads)
        cr (Math/cos rads)]
    {:x (- (* x cr) (* y sr))
     :y (+ (* y cr) (* x sr))}))

(defn average
  "Take the average of the points"
  [pts]
  (let [c (count pts)]
    (point (/ (apply + (map :x pts)) c)
           (/ (apply + (map :y pts)) c))))

(defn shift
  [pt delta]
  {:x (+ (:x pt) (:x delta))
   :y (+ (:y pt) (:y delta))})

(defn scale
  "Computes the next position given heading `h` and speed `v`"
  [{:keys [x y]} s]
  (point (* s x) (* s y)))

(defn scale2d
  "Piecewise multiply 2 vectors"
  [pt1 pt2]
  (point (* (:x pt1) (:x pt2)) 
         (* (:y pt1) (:y pt2))))

(defn normalize
  [pt]
  (let [{:keys [x y]} pt
        d (Math/sqrt (+ (* x x) (* y y)))]
    (point (/ x d) (/ y d))))

(defn constrain
  "Keeps pt within boundary of world width, world height"
  [{:keys [x y]} w h]
  {:x (mod x w) :y (mod y h)})

(defn jitter-int
  "Produces a random number between the mi mx"
  [mi mx]
  (let [mag (rand-int (- mx mi))]
    (+ mi mag)))

(defn jitter-float
  "Produces a random number between the mi mx"
  [mi mx]
  (let [mag (* (rand 1) (- mx mi))]
    (+ mi mag)))

(defn jitter-point
  "Jitters a point"
  [{:keys [x y]} amt]
  (point (jitter-int (- x amt) (+ x amt))
         (jitter-int (- y amt) (+ y amt))))

(defn new-direction
  "Computes a new direction based on prior speed / direction and new heading"
  [od hd speed]
  (let [{:keys [x y]} (scale od speed)
        nx (:x hd) ny (:y hd)]
    (normalize (point (+ x nx) (+ y ny)))))

(defn random-convex-polygon
  [x y]
  (let [xs [-0.50 -0.25 0.25 0.50 0.25 -0.25]
        ys [0      0.50 0.50 0   -0.50 -0.50]
        jmin -0.1
        jmax 0.1
        rads (rand 3)]
    (map #(rotate (point (+ %1 (jitter-float jmin jmax))
                         (+ %2 (jitter-float jmin jmax)))
                  rads)
         xs ys)))

(defn asteroid
  "Creates an asteroid"
  [pos heading width height]
  {:Type ::Asteroid
   :origin pos
   :pos pos
   :heading (normalize heading)
   :width width
   :height height
   :points (random-convex-polygon (:x pos) (:y pos))
   :hits 0})

(defn ship
  "Creates a ship at {:x :y}

Optionally, the arguments:

   :heading => unit-vector of direction => {:x 0 :y 1}
   :exploding => bool of exploding => false
   :width => int of width => 12
   :height => int of height => 20
"
  [pt & {:keys [heading exploding width height] :or 
         {heading (point 1 0) exploding false width 12 height 20}}]
  (let [nheading (normalize heading)]
    {:Type ::Ship 
     :pos pt
     :heading nheading ; which way we're pointing
     :direction nheading ; which way we're moving
     :speed 0 
     :exploding exploding
     :exploding-bits []
     :width width 
     :height height 
     :bullets [] 
     :energy ship-max-energy 
     :lives 3 
     :thruster false}))

(defn game
  "Creates a game, with a ship, and other things"
  [w h]
  (let [w2 (/ w 2)
        h2 (/ h 2)]
    {:p1 (ship (point w2 h2) :heading (point 0 1)) ; player 1 ship
     :width w :height h
     :time 0
     :asteroids [(asteroid (point (rand-int w) (rand-int h)) 
                          (point (rand 1) (rand 1))
                          100 100)
                 (asteroid (point (rand-int w) (rand-int h)) 
                           (point (rand 1) (rand 1))
                           100 100)
                 (asteroid (point (rand-int w) (rand-int h)) 
                           (point (rand 1) (rand 1))
                           75 75)]
     :explosions []
     :Type ::Game}))

(defn bullet
  "Creates a bullet"
  [pos heading time]
  {:Type ::Bullet 
   :pos pos 
   :heading heading 
   :created time 
   :origin pos
   :height 1
   :width 1})

(defn explosion
  "Creates an explosion"
  [from pos pieces time]
  {:Type ::Explosion
   :FromType from
   :pos pos
   :pieces pieces
   :created time})

(defn piece
  "Creates an explosion piece"
  [pos dir rot len time]
  {:Type ::Piece
   :pos pos
   :heading dir
   :rotation rot
   :created time
   :width len})

(defn random-piece
  [pos mxl t]
  (piece 
   (jitter-point pos 5)
   (vec2d-of (* (rand) TWOPI))
   (jitter-float -0.01 0.01)
   (jitter-int 5 mxl)
   t))

(defn butexpired
  "Removes the expired items from `coll` given the game-time t"
  [coll t lifetime]
  (let [c (count coll)
        n (filter #(< (- t (:created %)) lifetime) coll)]
    n))

(defn shoot [s t]
  "Shoots a bullet towards heading from pos"
  (let [{:keys [heading pos bullets]} s
        bs (butexpired bullets t bullet-lifetime)]
    (if (< (count bs) 5)
      (assoc s :bullets (cons (bullet pos heading t) bs))
      s)))



;  1 2           1  ->  1, n1-2, p 0
;                2  ->  n1-2 2 3 p
; 0 . 3          3  ->  p 3 4 n4-5
;                5  ->  0 p n4-5 5 
;  5 4
(defn split-asteroid 
  "Splits asteroid into 4 asteroids"
  [a]
  (let [points (vec (:points a)) 
        p0 (points 0)
        p1 (points 1)
        p2 (points 2)
        p3 (points 3)
        p4 (points 4)
        p5 (points 5)
        p1-2 (average [p1 p2])
        p4-5 (average [p4 p5])
        p (:pos a)
        heading (:heading a)
        asteroids [[p1 p1-2 p p0]
                   [p1-2 p2 p3 p]
                   [p p3 p4 p4-5]
                   [p0 p p4-5 p5]]]
    (map #(let [w (/ (:width a) 4)
                h (/ (:height a) 4)]
            (asteroid
             (case %2
               1 (shift p (point (- w) (- h)))
               2 (shift p (point w (- h)))
               3 (shift p (point w h))
               4 (shift p (point (- w) h)))
             (rotate heading (* HALFPI %2))
             (/ (:width a) 2.6)
             (/ (:height a) 2.6)))
         asteroids (range 1 (inc (count asteroids))))))

(defn update-asteroid
  "Updates a single asteroid's position"
  [a t ww wh]
  (let [{:keys [pos heading]} a
        newpos (shift pos (scale heading asteroid-speed))]
    (assoc a :pos (constrain newpos ww wh))))

(defn update-asteroids
  [coll t ww wh]
  (map #(update-asteroid % t ww wh) coll))

(defn update-bullets [coll t ww wh]
  (let [bullets (butexpired coll t bullet-lifetime)] 
    (map (fn [b]
           (let [{:keys [pos heading]} b]
             (assoc b :pos (constrain 
                            (shift pos (scale heading bullet-speed)) 
                            ww wh))))
         bullets)))

(defn update-ship-pos
  "Updates the ship position given heading and speed"
  [pos heading speed]
  (shift pos (scale heading speed)))

(defn update-ship-speed
  "Updates the ship's speed given thruster and previous speed"
  [speed thruster]
  (if thruster
    (if (< speed ship-max-speed) (inc speed) ship-max-speed)
    (if (> speed 0) (- speed ship-speed-decay) 0)))

(defn update-ship-energy
  [energy thruster]
  (if (and thruster (> energy 0))
    (- energy 0.5)
    energy))

(defn update-ship 
  "Update the ship's state"
  [s t ww wh]
  (let [{:keys [bullets pos heading speed direction
                thruster energy exploding exploding-bits]} s
        nenergy (update-ship-energy energy thruster)
        canthrust (and thruster (> nenergy 0))
        nspeed (update-ship-speed speed canthrust)
        ndirection (if canthrust
                     (new-direction direction heading nspeed)
                     direction)]
    (assoc s 
      :bullets (update-bullets bullets t ww wh)
      :direction ndirection
      :pos (constrain (update-ship-pos pos ndirection nspeed) ww wh)
      :energy nenergy
      :speed nspeed
      :thruster false)))


(defn react-keypressed-ship 
  "Reacts to keyboard events for the ship `s` for keyCode `k` at 
game time `t`"
  [s k t]
  (let [{:keys [heading pos exploding direction speed bullets]} s]
    (cond ;; case didnt' work here?
      (== k VK_RIGHT) (assoc s :heading (rotate heading (Math/toRadians 10.0))) 
      (== k VK_LEFT) (assoc s :heading (rotate heading (Math/toRadians -10.0)))
      (== k VK_UP) (assoc s :thruster true)
      :else s)))

(defn react-keyreleased-ship 
  "Reacts to keyboard events for the ship `s` for keyCode `k` at 
game time `t`"
  [s k t]
  (let [{:keys [heading pos exploding bullets]} s]
    (cond ;; case didnt' work here?
      (== k VK_SPACE) (shoot s t)
      :else s)))

(defn paint-decorations 
  "Paints the scoreboards and things of that nature"
  [g {:keys [p1 width height]}]
  (let [{:keys [energy lives]} p1
        barw (* (/ energy ship-max-energy) 200)]
    (.setColor g (color 255 255 255 50))
    (.fillRect g 71 21 barw 8)
    (.setColor g (color 100 100 100))
    (.drawRect g 70 20 200 10)
    (.setColor g (color 150 150 150))
    (.drawString  g "Player 1" 20 30)))

(defn paint-heading
  [g pos heading mag]
  (let [hea (shift pos (scale heading mag))]
    (.setColor g (color 200 0 0 100))
    (.drawLine g (:x pos) (:y pos) (:x hea) (:y hea))))

;; returns bool
(defmulti collision-of (fn [a b t & _] [(:Type a) (:Type b)]))

;; returns an explosion for object
(defmulti explosion-of (fn [o t & _] (:Type o)))

(defmulti explosion-of :Type)

(defmulti explodable? :Type)

;; returns a polygon representation of object
(defmulti polygon-of "Computes a Polygon from type" :Type)

;; paints object on graphics context g
(defmulti paint (fn [g object & _] (:Type object)))

(defmethod explosion-of ::Ship [s t]
  ;; return an explosion for s
  (explosion ::Ship (:pos s) [] t))

(defmethod explosion-of ::Asteroid [a t]
  ;; return an explosion for asteroid a if and only if asteroid size is
  ;; explodable.
  (explosion ::Asteroid 
             (:pos a)
             (take (inc (rand-int 5)) 
                   (repeatedly #(random-piece (:pos a) (:width a) t)))
  ;   [pos dir rot len time]
             t))

(defmethod explosion-of :default [a t]
  (prn "not sure how this happened..."))

(defmethod explodable? ::Asteroid [a]
  (or (< (:width a) asteroid-min-size)
      (< (:height a) asteroid-min-size)))

(defmethod polygon-of ::Ship [s]
  (let [hw (/ (:width s) 2)
        hh  (/ (:height s) 2)
        heading (:heading s)
        points [(point hh 0) 
                (point (- hh) (- hw)) 
                (point (- hh) hw)]
        rheading (radians-of (:heading s))
        spoints (map #(shift (rotate % rheading) (:pos s)) points)]
    (Polygon. (int-array (map :x spoints))
              (int-array (map :y spoints))
              3)))

(defmethod polygon-of ::Asteroid [a]
  (let [sz (point (:width a) (:height a))
        points (map #(shift (scale2d % sz) (:pos a)) (:points a))]
    (Polygon. (int-array (map :x points))
              (int-array (map :y points))
              (count points))))

(defmethod collision-of [::Bullet ::Asteroid] [bull aster t]
  (let [poly (polygon-of aster)
        pos (:pos bull)]
    (.contains poly (:x pos) (:y pos))))

(defmethod collision-of [::Bullet ::Ship] [bull ship t]
  (let [poly (polygon-of ship)
        pos (:pos bull)]
    (.contains poly (:x pos) (:y pos))))

(defmethod collision-of [::Ship ::Asteroid] [ship aster t]
  (let [spoly (polygon-of ship)
        apoly (polygon-of aster)]
    (.intersects spoly (.getBounds2d apoly))))

(defmethod collision-of [::Ship ::Ship] [ship1 ship2 t]
  (let [apoly (polygon-of ship1)
        bpoly (polygon-of ship2)]
    (.intersects apoly (.getBounds2d bpoly))))

(defmethod collision-of [::Asteroid ::Asteroid] [aster1 aster2 t]
  (let [apoly (polygon-of aster1)
        bpoly (polygon-of aster2)]
    (.intersects apoly (.getBounds2d bpoly))))

(defmethod paint ::Bullet [g b]
  (let [pos (:pos b)
        x (:x pos)
        y (:y pos)]
    (color 255 255 255)
    (.drawRect g x y 1 1)))

(defmethod paint ::Ship [g s]
  (let [poly (polygon-of s)
        bullets (:bullets s)]
    (.setColor g (color 100 100 100))
    (paint-heading g (:pos s) (:heading s) 20)

    ; ship's color
    (.setColor g (color 255 255 0))
    (.drawPolygon g poly)

    ; draw bullets
    (when (coll? bullets)
      (doseq [b bullets]
        (paint g b)))))

(defmethod paint ::Asteroid [g a]
  (let [poly (polygon-of a)]
    (paint-heading g (:pos a) (:heading a) (:width a))
    (.setColor g (color 255 255 255))
    (.drawPolygon g poly)))

(defmethod paint ::Game [g gs]
  (let [{:keys [p1 width height asteroids explosions]} gs]
    (paint-decorations g gs)
    (doseq [a asteroids]
      (paint g a))
    (doseq [a explosions]
      (paint g a))
    (paint g p1)))

(defmethod paint ::Explosion [g e]
  (doseq [p (:pieces e)]
    (paint g p)))

(defmethod paint ::Piece [g p]
  (let [{:keys [pos width heading]} p
        start pos
        end (shift pos (scale width heading))]
    (.setColor g (color 255 255 255 100))
    (.drawLine g (:x start) (:y start) (:x end) (:y end))))

;; rewrite!
(defn update-collisions
  [t bullets asteroids explosions]
  (let [ba-collisions (for [b bullets a asteroids :when (collision-of b a t)] [b a])
        ast-in-collision (map second ba-collisions)
        bull-in-collision (map first ba-collisions)
        dead-asteroids (filter explodable? ast-in-collision)
        spliting-asteroids (filter #(not (explodable? %)) ast-in-collision)
        new-explosions (map (fn [a]
                              (explosion-of a t)) 
                            dead-asteroids)]
    {:bullets (remove (apply hash-set bull-in-collision) bullets)
     :asteroids (flatten (replace 
                          (apply hash-map
                                 (reduce concat 
                                         (map
                                          (fn [a] [a (split-asteroid a)])
                                          spliting-asteroids)))
                          (remove (apply hash-set dead-asteroids) asteroids)))
     :explosions (concat new-explosions explosions)}))

(defn update-game
  [gm]
  (let [{:keys [time p1 width height asteroids]} @gm
        t (inc time)
        nasteroids (update-asteroids asteroids t width height)
        sp1 (update-ship p1 t width height)
        collisions (update-collisions 
                    (:time gm)
                    (:bullets sp1) 
                    nasteroids 
                    (:explosions gm))]
    (dosync
     (ref-set gm (assoc @gm 
                   :p1 (assoc sp1 :bullets (:bullets collisions))
                   :time t
                   :explosions (:explosions collisions)
                   :asteroids (:asteroids collisions))))))

(defn game-panel [frame gm]
  (proxy [JPanel ActionListener KeyListener MouseListener] []
    (paintComponent [g]
      (proxy-super paintComponent g)
      (paint g @gm))
    (getPreferredSize []
      (Dimension. (:width @gm)
                  (:height @gm)))
    (actionPerformed [e]
                     (do 
                       (update-game gm)
                       (.repaint this)))
    (keyReleased [e]
                (let [s (react-keyreleased-ship (:p1 @gm) 
                                                (.getKeyCode e) 
                                                (:time @gm))]
                  (dosync
                   (ref-set gm (assoc @gm :p1 s)))))
    (keyTyped [e])
    (keyPressed [e]
                (let [s (react-keypressed-ship (:p1 @gm) 
                                               (.getKeyCode e) 
                                               (:time @gm))]
                  (dosync
                   (ref-set gm (assoc @gm :p1 s)))))
    (mouseClicked [e])
    (mousePressed [e])
    (mouseReleased [e])
    (mouseEntered [e])
    (mouseExited [e])))

(defn start-game [w h]
  (let [frame (JFrame. "Asteroid Fight")
        gm (ref (game w h))
        panel (game-panel frame gm)
        timer (Timer. game-tick-time panel)]
    (doto panel
      (.setBackground (color 0 0 0))
      (.setFocusable true)
      (.addKeyListener panel)
      (.addMouseListener panel))
    (doto frame
      (.add panel)
      (.pack)
      (.setVisible true))
    (.start timer)
    [gm timer]))

