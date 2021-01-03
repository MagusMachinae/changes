(ns changes.core
  (:require [clojure.pprint :as pp]
            [clojure.string :as str]))


(def values {
             [7,7,7,7,7,7] "#1, Ch'ien, The Creative",
             [8,8,8,8,8,8] "#2, K'un, The Receptive",
             [7 8 8 8 7 8] "#3, Chun, Difficulty at the Beginning",
             [8,7,8,8,8,7] "#4, Meng, Youthful Folly",
             [7,7,7,8,7,8] "#5, Hsu, Waiting (Nourishment)",
             [8,7,8,7,7,7] "#6, Sung, Conflict",
             [8,7,8,8,8,8] "#7, Shih, The Army",
             [8,8,8,8,7,8] "#8, Pi, Holding Together (Union)",
             [7,7,7,8,7,7] "#9, Hsiao Ch'u, The Taming Power of the Small",
             [7,7,8,7,7,7] "#10, Lu, Treading (Conflict)",
             [7,7,7,8,8,8] "#11, T'ai, Peace",
             [8,8,8,7,7,7] "#12, P'i, Standstill (Stagnation)",
             [7,8,7,7,7,7] "#13, T'ung Jen, Fellowship with Men",
             [7,7,7,7,8,7] "#14, Ta Yu, Possession in Great Measure",
             [8,8,7,8,8,8] "#15, Ch'ien, Modesty",
             [8,8,8,7,8,8] "#16, Yu, Enthusiasm",
             [7,8,8,7,7,8] "#17, Sui, Following",
             [8,7,7,8,8,7] "#18, Ku, Work on What Has Been Spoiled (Decay)",
             [7,7,8,8,8,8] "#19, Lin, Approach",
             [8,8,8,8,7,7] "#20, Kuan, Contemplation (View)",
             [7,8,8,7,8,7] "#21, Shih Ho, Biting Through",
             [7,8,7,8,8,7] "#22, Pi, Grace",
             [8,8,8,8,8,7] "#23, Po, Splitting Apart",
             [7,8,8,8,8,8] "#24, Fu, Return (The Turning Point)",
             [7,8,8,7,7,7] "#25, Wu Wang, Innocence (The Unexpected)",
             [7,7,7,8,8,7] "#26, Ta Ch'u, The Taming Power of the Great",
             [7,8,8,8,8,7] "#27, I, The Corners of the Mouth",
             [8,7,7,7,7,8] "#28, Ta Kuo, Preponderance of the Great",
             [8,7,8,8,7,8] "#29, K'an, The Abysmal (Water)",
             [7,8,7,7,8,7] "#30, Li, The Clinging (Fire)",
             [8,8,7,7,7,8] "#31, Hsien, Influence (Wooing)",
             [8,7,7,7,8,8] "#32, Heng, Duration",
             [8,8,7,7,7,7] "#33, Tun, Retreat",
             [7,7,7,7,8,8] "#34, Ta Chuang, The Power of the Great",
             [8,8,8,7,8,7] "#35, Chin, Progress",
             [7,8,7,8,8,8] "#36, Ming I, Darkening of the Light",
             [7,8,7,8,7,7] "#37, Chia Jen, The Family (The Clan)",
             [7,7,8,7,8,7] "#38, K'uei, Opposition",
             [8,8,7,8,7,8] "#39, Chien, Obstruction",
             [8,7,8,7,8,8] "#40, Hsieh, Deliverance",
             [7,7,8,8,8,7] "#41, Sun, Decrease",
             [7,8,8,8,7,7] "#42, I,Increase",
             [7,7,7,7,7,8] "#43, Kuai, Break-through (Resoluteness)",
             [8,7,7,7,7,7] "#44, Kou, Coming to Meet",
             [8,8,8,7,7,8] "#45, Ts'ui, Gathering Together (Massing)",
             [8,7,7,8,8,8] "#46, Sheng, Pushing Upward",
             [8,7,8,7,7,8] "#47, K'un, Oppression (Exhaustion)",
             [8,7,7,8,7,8] "#48, Ching, The Well",
             [7,8,7,7,7,8] "#49, Ko, Revolution (Molting)",
             [8,7,7,7,8,7] "#50, Ting, The Cauldron",
             [7,8,8,7,8,8] "#51, Chen, The Arousing (Shock, Thunder)",
             [8,8,7,8,8,7] "#52, Ken, Keeping Still(Mountain)",
             [8,8,7,8,7,7] "#53, Chien, Development (Gradual Progress)",
             [7,7,8,7,8,8] "#54, Kuei Mei, The Marrying Maiden",
             [7,8,7,7,8,8] "#55, Feng, Abundance (Fullness)",
             [8,8,7,7,8,7] "#56, Lu, The Wanderer",
             [8,7,7,8,7,7] "#57, Sun, The Gentle (Penetrating, Wind)",
             [7,7,8,7,7,8] "#58, Tui, The Joyous (Lake)",
             [8,7,8,8,7,7] "#59, Huan, Dispersion (Dissolution)",
             [7,7,8,8,7,8] "#60, Chieh, Limitation",
             [7,7,8,8,7,7] "#61, Chung Fu, Inner Truth",
             [8,8,7,7,8,8] "#62, Hsiao Kuo, Preponderance of the Small",
             [7,8,7,8,7,8] "#63, Chi Chi, After Completion",
             [8,7,8,7,8,7] "#64, Wei Chi, Before Completion"})

(defn normalise-hands [hand]
  (if (zero? (mod hand 4))
    4
    (mod hand 4)))

(def m-normalise-hands
  (memoize normalise-hands))

(defn pick-hands [z]
  (let [left  (rand-int z)
        right (- z left 1)]
    (- z
       (+ (normalise-hands left)
          (normalise-hands right)
          1) ))

  )

(map parse-hands (map #(/ % 4) (repeatedly 6 #(last (take 4 (iterate pick-hands 49))))))
(int (/ 36 4))
(repeatedly 10 pick-hands)
(let [x (rand-int 49)]
  (int (/ (- 47 (+ (normalise-hands x) (normalise-hands (- 48 x)))) 4)))
(map normalise-hands (repeatedly 5 #(rand-int 49)))
(defn parse-hands [val]
  (case val
    7 "_______"
    8 "___ ___"
    6 "___x___"
    9 "___Ø___"))

(defn gua1 [num]
  (case num
    9 7
    6 8
    num))

(defn gua2 [num]
  (case num
    9 7
    6 8
    num))

(def m-parse-hands
  (memoize parse-hands))

(defn parse-trigrams [gua]
  )

(defn changes
  "I don't do a whole lot."
  []
  ())

;(defn parse-6-9 [coll]
;  (let [coll-length (length coll)]
;    ()))

(def ba [
         "_______"
         "___ ___"
         "___x___"])
(print (str/join "\n" ba))

;(def joiner (partial str/join /n ))

;(apply)

(clojure.pprint/pprint {:a 1 :b 3 :c {:d 4 :e 5} :g 1234567 :h 23049 :j 329847234 :ff 2})

;(pprint ba)
