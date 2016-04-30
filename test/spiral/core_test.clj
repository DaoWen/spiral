(ns spiral.core-test
  (:require [clojure.test :refer :all]
            [spiral.core :refer :all]))

(deftest random-test
  (testing "A bunch of random sequences"
    (time
      (let [t (atom 0), f (atom 0)]
        (dotimes [i 100000]
          (let [dists (take (inc (rand-int 100))
                            (repeatedly #(inc (rand-int 100))))
                res1 (any-segs-intersect-naive? dists)
                res2 (any-segs-intersect? dists)]
            (is (= (boolean res1) (boolean res2)))
            (swap! (if res1 t f) inc)
            (when-not (= (boolean res1) (boolean res2))
              (doall (map println [dists res1 res2])))))
        (printf "Intersected %d times, didn't %d times.%n" @t @f)))))
