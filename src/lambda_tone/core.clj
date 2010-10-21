(ns lambda-tone.core
  (:use overtone.live)
  (:require [polynome.core :as poly]))

(def ipad (osc-server 9800))

(def metro (metronome 114))

(defn mix [channels]
  (let [scale-factor (/ 1.0 (count channels))]
    (with-ugens
      (apply + (map #(* scale-factor %) channels)))))

(def snap (sample "/home/rosejn/studio/samples/snap.wav"))

;(snap)

(definst nanana [note 60 dur 2 R 0.5 amp 0.3 beat 20]
  (let [freq (midicps note)
        a (* 0.3 dur)
        b (* 3 dur)
        src (* amp (env-gen (perc a b) 1 1 0 1 :free)
               (rlpf (saw [freq (* 1.5 freq)])
                     (* (env-gen (perc (+ a 0.01) (- b 0.1))) 3 freq) R))]
    (comb-n (compander src src (* 0.1 (+ 1 (sin-osc:kr beat)) 1 0.5 0.01 0.01)))))

(nanana (- 71 24) 35 0.9 0.15 )

(definst kick [amp 0.6 freq 100 dur 0.3 width 0.5]
  (let [freq-env (* freq (env-gen (perc 0 (* 0.9 dur))))
        env (env-gen (perc 0.01 dur) 1 1 0 1 :free)
        sqr (* 0.8 (env-gen (perc 0 0.01)) (pulse (* 2 freq) width))
        src (sin-osc freq-env)
        drum (+ sqr (* env src))]
    (* amp drum))) ;(compander drum drum 0.99 1 0.6 0.01 0.01))))

(definst c-hat [amp 0.8 t 0.04]
  (let [env (env-gen (perc 0.001 t) 1 1 0 1 :free)
        noise (white-noise)
        sqr (* (env-gen (perc 0.01 0.04)) (pulse 880 0.2))
        filt (bpf (+ sqr noise) 9000 0.5)]
    (* 0.5 (abs amp) env filt)))

(defsynth snare [out 0 freq 100 rq 0.5 attack 0 release 0.5 amp 0.1 pan 0]
  (let [snd (white-noise)
        filt (hpf snd freq)
        filt (+ filt (hpf filt (* freq 1.345)))
        filt (+ filt (hpf filt (* freq 2.456)))
        filt (+ filt (hpf filt (* freq 4.345)))
        snd (lpf filt 2000)
        snd (* 0.2 (rhpf snd (* 2 freq) rq))
        snd (* snd (env-gen (perc attack release 1 -10) :action :free))
        snd (free-verb snd 0.5 0.5 0.5)]
    (offset-out out (pan2 (* snd amp) pan))))

(def kicks (atom [0 0 0 0 0 0 0 0]))
(def hats  (atom [0 0 0 0 0 0 0 0]))
(def snaps (atom [0 0 0 0 0 0 0 0]))

(defn monome-ctl [path args]
  (let [val (first args)
        [_ x y] (first (re-seq #"/1/monome/([0-9])/([0-9])"  path))
        x (Integer. x)
        y (Integer. y)]
    ;(println x y val (type x) (type y) (type val))
    (cond 
      (= 1 x) (swap! snaps #(assoc % (dec y) val))
      (= 2 x) (swap! hats #(assoc % (dec y) val))
      (= 3 x) (swap! kicks #(assoc % (dec y) val)))))

(def ipad-controllers
  {#"/./monome/.*" monome-ctl
   #"/./rotary6"   nanana-R-ctl
   #"/./rotary7"   nanana-beat-ctl})

(defn beat-listener [{:keys [path args]}]
  (try 
    (doseq [ctl-key (filter #(re-matches % path) (keys ipad-controllers))]
      ((get ipad-controllers ctl-key) path args))
    (catch Exception e
      (println "Got exception in beat-listener: " (.printStackTrace e) e))))

(defn beat-listener-proxy [& args]
  (apply beat-listener args))

(osc-listen ipad beat-listener-proxy)

(defn drummer [beat]
  (doseq [i (range (count @hats))]
    (if (not= 0 (nth @hats i))
      (at (metro (+ (* 0.125 i) beat))
          (c-hat))))
;  (at (metro beat)
;      (c-hat 0.12))
;  (at (metro (+ 0.25 beat))
;      (c-hat 0.05))
;  (at (metro (+ 0.75 beat))
;      (c-hat 0.06))
;
;  (at (metro (- beat 0.02))
;      (if (zero? (mod beat 4))
;        (kick 0.97)
;        (kick)))
;
;  (at (metro (+ 0.5 beat))
;      (c-hat 0.08))

  (when (not= 0 (nth @kicks (mod beat (count @kicks))))
    (at (metro beat)
        (kick)))

  (when (not= 0 (nth @snaps (mod beat (count @snaps))))
    (let [offset (* 0.02 (rand))]
      (at (metro (- beat offset))
          (snap))
      (at (metro (- beat offset 0.02))
          (snare 0 200 0.01 0.01))))

    (apply-at #'drummer (metro (inc beat)) (inc beat)))

(drummer (metro))

;
;
;  (if (> (rand) 0.9)
;    (at (metro (+ 0.27 beat))
;        (c-hat 0.3 0.1)))
;
;  (when (zero? (mod beat 8))
;    (at (metro (+ 0.5 beat))
;    (at (metro (+ 1 beat))
;        (snap))))
;
  

(defsynth bass [note 55 dur 0.2 vel 0.8 filt 3 resonance 0.2]
  (let [freq (midicps (- note 12))
        env (env-gen (asr 0.3 dur 0.3) 1 :action :free)
        src (mix (saw [freq (* 0.99 freq)]))
        sub (sin-osc (/ freq 2))
        filt (rlpf (+ src sub) (line (* filt freq) freq 0.2) resonance)
        sound (* 0.8 vel
                 env
                 filt)
        snd (free-verb sound 0.6 0.3 0.5)]
    (out 0 (pan2 snd))))
;(bass)

(definst bass [note 55 dur 0.2 vel 0.4]
  (let [freq (midicps note)
        env (env-gen (perc 0.3 dur) 1 1 0 1 :free)
        src (saw [(* 0.995 freq) freq (* 1.02 freq)])
        sub (sin-osc [(* freq 0.5) (* 0.25 freq)])
        filt (lpf src (* (* 3 (rand 1)) freq))]
    (* (+ filt sub) env vel)))

(def ROOT 59)

(defn repeater 
  ([col] (repeater col 4 0.8))
  ([col n] (repeater col n 0.8))
  ([col n factor]
   (let [base (take n (repeatedly #(choose col)))]
     (loop [base base
            result []]
       (if (empty? base)
         result
         (recur (next base)
                (if (> (rand) factor)
                  (concat result [(first base) (first base)])
                  (conj result (first base)))))))))

(defn nanana-R-ctl [path args]
  (nanana :ctl :R (first args)))

(def nanana-beat (atom 20))

(defn nanana-beat-ctl [path args]
  (nanana :ctl :beat (* 30 (first args)))
  (reset! nanana-beat (* 30 (first args)))
  ;(println @nanana-beat)
  )

(defn bass-player [beat notes durs]
  (let [[notes durs]
        (if (empty? notes)
;          [[0   0   0   0   0   0   0   3  -4   -4  -4  -4  -4 -4 -4  -2]
;           [1.5 0.5 1.5 0.5 1.5 0.5 1.0 1.0 1.5 0.5 1.5 0.5 1.5 0.5 1.0 1.0]]
          [(repeater [0 3 -4 -2] 16) (take 16 (cycle [1.5 0.5]))]
          [notes durs])
        note (+ ROOT (first notes))
        dur (first durs)
        next-beat (+ dur beat)]
      (at (metro (+ beat 0.01 (* 0.03 (rand))))
          (nanana (- note 12) dur 0.5 0.6 @nanana-beat))
    (apply-at #'bass-player (metro next-beat) next-beat (next notes) (next durs))))

(definst high-drone [note 71 amp 0.0001 dur 18 amt 0.8]
  (let [freq (+ (midicps note) (lag:kr (* (line:kr 1 10 (* 0.6 dur))
                                          (lf-noise0:kr (line:kr 40 0.1 dur))) 0.5))
        src (lf-tri [freq (* 0.99 freq)])
        saws (saw [freq (* 1.01 freq) (* freq 0.99)])
        signal (apply + (concat src saws))
        k          (/ (* 2 amt) (- 1 amt))
        distort    (/ (* (+ 1 k) signal) (+ 1 (* k (abs signal))))
        ;f-env (env-gen (perc (* 0.3 dur) (* 0.7 dur)))
        ;filt (rlpf distort (+ freq (* f-env 200)) 0.8)
        filt (rlpf distort (line:kr (* 4 freq) (* 0.5 freq) dur) 0.4)
        env (env-gen (perc (* 0.5 dur) (* 0.5 dur)) :action :free)]
    (out 0 (pan2 (* 0.3 amp env filt) (* 0.5 (sin-osc:kr 0.2))))))

(defn droner [beat]
  (at (metro beat)
      (high-drone (- 71 24) 0.186)
      (high-drone (- 71 12) 0.086)
      (high-drone (- 71 36) 0.486)
      )
  (apply-at #'droner (metro (+ 16 beat)) (+ 16 beat)))

(defn azure []
  (droner (metro))
  (drummer (metro))
  (bass-player (metro) [] []))

;(azure)
;(stop)

(def BASS-FREQ (midi->hz 43))

;(defonce m (poly/init "/dev/ttyUSB0"))
;(defn monome []
;  (poly/remove-all-callbacks m)
;  (poly/on-press m (fn [x y s]
;                     (bass (diotonic-freq BASS-FREQ (poly/button-id m x y)))))
;  (poly/light-led-on-sustain m))
;(monome)

;(kick)

(definst rise-fall-pad [note 60 t 4 amt 0.3 amp 0.8]
  (let [freq       (midicps note)
        f-env      (env-gen (perc t t) 1 1 0 1 :free)
        src        (saw [freq (* freq 1.01)])
        signal     (rlpf (* 0.3 src)
                         (+ (* 0.6 freq) (* f-env 2 freq)) 0.2)
        k          (/ (* 2 amt) (- 1 amt))
        distort    (/ (* (+ 1 k) signal) (+ 1 (* k (abs signal))))
        gate       (pulse (* 2 (+ 1 (sin-osc:kr 0.05))))
        compressor (compander distort gate 0.01 1 0.5 0.01 0.01)
        dampener   (+ 1 (* 0.5 (sin-osc:kr 0.5)))
        reverb     (free-verb compressor 0.5 0.5 dampener)
        echo       (comb-n reverb 0.4 0.3 0.5)]
    (* amp echo)))

(rise-fall-pad 67 20 0.5 0.2)
;(rise-fall-pad 440 2 0.9)

(def bass-notes (doall (concat
                         (take 8 (cycle [40 40 47 43]))
                         [40 35 43 40])))

(defn note-seq []
  (lazy-seq
    (cons
      (choose bass-notes)
      (note-seq))))

(def C-MAJ (take 24 (drop 24 (scale :c :major))))

(defn ballin [beat bass-line]
  (when bass-line
    (at (metro beat)
        (do
          (kick)
          (bass (midi->hz (first bass-line)))
          (if (= 0 (rem beat 4))
            (doseq [note (map #(+ % 12 (octave-note 3 (:c NOTE))) (:major7 CHORD))]
              (bass note 0.4)))))
    (at (metro (+ 1.5 beat)) (c-hat))
    (apply-at #'ballin (metro (inc beat)) (inc beat) (next bass-line))))

;(ballin (metro) (take 40 (cycle bass-notes)))

(defn whoop [beat notes]
  (let [n (* 1 (inc (rand-int 3)))
        shift (* 12 (rand-int 3))
        notes (if (empty? notes)
                (apply interleave
                       (repeat n (map #(- % shift) [60 68 65 63 68 72 65])))
                notes)
        note (first notes)]
    (at (metro beat)
        (do
          (if (zero? (mod beat 4))
            (kick 0.9)
            (kick (+ 0.2 (/ (rand) 2))))
          (rise-fall-pad (midi->hz (+ 0 note)) 2.2
                         (* 0.3 (Math/abs (Math/sin (* 0.4 beat)))))))
;    (at (metro (+ 0.25 beat))
;               (rise-fall-pad (midi->hz (- note 12)) 2.8
;                              (* 0.8 (Math/abs (Math/sin (* 0.1 beat))))))
    (if (= 0 (rem beat 4))
      (at (metro (+ 0.745 beat)) (kick 0.6 140)))
;    (if (= 0 (rem beat 9))
;      (flute-noise))
    (at (metro (+ beat 0.5))
        (c-hat 0.2))

    (apply-at #'whoop (metro (inc beat)) (inc beat) (next notes))))

;(whoop (metro) [60 63 65 60 68])

; Sound FX and more...

(definst whoah [note 40 amp 0.3]
  (let [freq (midicps note)
        sound (resonz (saw (map #(+ % (* (sin-osc 100) 1000)) 
                                [freq (+ freq 3) (+ freq 7)]))
                      (x-line 10000 10 25 :free)
                      (line 1 0.05 25))]
  (* amp (lf-saw:kr (line:kr 13 17 3)) (line:kr 1 0 10) sound)))

(whoah (- 71 36))

(definst robot [note 40 dur 0.5 rate 5]
  (let [gate (line 1 0 dur)
        env (env-gen (adsr 0.05 0.8 0.01 0.3)
                     :gate gate
                     :action :free)
        noiz (white-noise)
        modulator (* 24 (latch:kr noiz (lf-pulse:kr rate)))
        src (sin-osc (midicps (+ modulator note)) 0 0.45)]
    src))

;(robot 67)


