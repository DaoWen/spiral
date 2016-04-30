(ns spiral.core)

(defrecord Point [x y])

(defrecord HLineSeg [y x1 x2])

(defrecord VLineSeg [x y1 y2])

; I'm taking advantage of the fact that we only have
; horizontal and vertical lines, making intersection easy.
; I'm also assuming the values are given in sorted order.
(defn intersects? [h v]
  (and (<= (:x1 h) (:x v) (:x2 h))
       (<= (:y1 v) (:y h) (:y2 v))))

;;;;;;;;;;;;;;;;;;;;;;;
;; naive version

(defn advance-point [prev-point direction distance]
  (case direction
    :n (->Point (:x prev-point) (+ (:y prev-point) distance))
    :s (->Point (:x prev-point) (- (:y prev-point) distance))
    :e (->Point (+ (:x prev-point) distance) (:y prev-point))
    :w (->Point (- (:x prev-point) distance) (:y prev-point))))

; This function ensures that line segment values correctly sorted.
(defn make-segment [direction p1 p2]
  (if (contains? #{:n :s} direction)
    (apply ->VLineSeg (:x p1) (sort (mapv :y [p1 p2])))
    (apply ->HLineSeg (:y p1) (sort (mapv :x [p1 p2])))))

; Computes all of the line segments for the input.
; Adds two dummy (zero-length) segments on the ends so that every
; input line intersects its two neighbors.
(defn all-segments [distances]
  (loop [segs []
         prev-point (->Point 0 0)
         [dir & directions] (cycle [:e :n :w :s])
         [dist & distances] (concat [0] distances [0])]
    (if-not dist segs
      (let [next-point (advance-point prev-point dir dist)
            new-seg (make-segment dir prev-point next-point)]
        (recur (conj segs new-seg) next-point directions distances)))))

; Naive solution checks all line segments to see if the intersect
; with more than just their two neighboring segments.
(defn any-segs-intersect-naive? [distances]
  (let [segs (all-segments distances)
        hsegs (take-nth 2 segs)
        vsegs (take-nth 2 (rest segs))]
    (seq (filter #(< 2 (count %))
                 (for [h hsegs]
                   (keep #(if (intersects? h %) [h %]) vsegs))))))

;;;;;;;;;;;;;;;;;;;;;;;;;
;; optimized version

(defn check-intersect [seg1 seg2]
  (when (not-any? nil? [seg1 seg2])
    (if (instance? HLineSeg seg1)
      (intersects? seg1 seg2)
      (intersects? seg2 seg1))))

; The key insight here is that the new line segment has an intersection
; iff it intersects with the 3rd-to-last or 5th-to-last segment.
; This means you only have to check those two segments for intersections,
; and you only need to record the last 5 segments, giving you a constant
; space requirement, and a constant time per input element.
(defn any-segs-intersect? [distances]
  (loop [prev-segs nil
         prev-point (->Point 0 0)
         [dir & directions] (cycle [:n :w :s :e])
         [dist & distances] distances]
    (when dist
      (let [next-point (advance-point prev-point dir dist)
            new-seg (make-segment dir prev-point next-point)
            seg-3 (nth prev-segs 2 nil)
            seg-5 (nth prev-segs 4 nil)]
        (cond
          (check-intersect new-seg seg-3) [new-seg seg-3] ; these crossed
          (check-intersect new-seg seg-5) [new-seg seg-5] ; these crossed
          :else (recur (cons new-seg (take 4 prev-segs))
                       next-point directions distances))))))

