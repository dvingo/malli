(ns malli.instrument.fn-schemas)

(def VecOfInts [:sequential :int])

(defn sum-nums
  {:malli/schema [:=> [:cat VecOfInts] :int]}
  [args]
  (apply + args))
