(defn run_test_level10_0 []
(ohua
(let [local-28 (write-data  "service-name" 1) local-29 (get-data  "service-name" 1)]
(let [local-26 (compute  2) local-27 (compute local-28 local-29 1)]
(let [local-23 (compute local-28 1) local-24 (get-data  "service-name" 1) local-25 (compute local-28 2)]
(let [local-21 (compute local-23 local-26 7) local-22 (compute local-24 local-25 local-26 local-27 local-29 4)]
(let [local-18 (compute local-21 local-22 local-26 local-28 4) local-19 (get-data local-22 local-23 "service-name" 5) local-20 (get-data local-21 local-24 local-28 "service-name" 2)]
(let [local-12 (get-data local-19 local-20 local-25 "service-name" 7) local-13 (get-data local-18 local-20 local-29 "service-name" 2) local-14 (get-data  "service-name" 3) local-15 (compute local-19 local-20 3) local-16 (write-data local-20 local-21 "service-name" 6) local-17 (compute local-18 local-19 local-20 6)]
(let [local-7 (get-data local-15 local-16 local-17 local-18 local-25 "service-name" 6) local-8 (compute local-12 local-13 local-15 local-16 local-18 local-20 7) local-9 (get-data local-15 local-16 local-18 local-20 "service-name" 7) local-10 (get-data local-15 local-20 local-25 "service-name" 1) local-11 (get-data local-15 local-17 local-18 local-22 "service-name" 2)]
(let [local-4 (get-data local-8 local-10 local-17 local-18 local-22 local-23 local-26 "service-name" 2) local-5 (compute local-8 local-10 local-11 local-13 local-15 4) local-6 (compute local-11 local-12 local-16 local-17 local-19 local-20 4)]
(let [local-2 (get-data local-5 local-6 local-9 local-12 local-16 "service-name" 2) local-3 (get-data local-4 local-16 "service-name" 6)]
(compute local-2 local-3 local-7 local-14 6)
)))))))))
))
(defn run_test_level9_0 []
(ohua
(let [local-25 (get-data  "service-name" 4) local-26 (compute  5) local-27 (get-data  "service-name" 2) local-28 (compute  4) local-29 (compute  4)]
(let [local-22 (compute local-29 6) local-23 (compute local-27 local-28 1) local-24 (compute local-25 local-27 local-29 2)]
(let [local-20 (compute local-23 local-27 local-29 2) local-21 (compute local-22 local-25 local-26 3)]
(let [local-18 (compute local-25 local-27 local-29 4) local-19 (compute local-21 local-28 4)]
(let [local-14 (compute local-18 local-19 6) local-15 (get-data local-19 local-21 local-29 "service-name" 7) local-16 (get-data local-19 local-27 "service-name" 7) local-17 (get-data local-25 local-26 local-28 "service-name" 7)]
(let [local-8 (get-data local-15 local-16 local-18 local-20 "service-name" 4) local-9 (compute local-15 local-17 local-19 local-21 5) local-10 (compute local-14 local-16 local-17 1) local-11 (get-data local-15 local-16 local-18 "service-name" 7) local-12 (get-data local-17 local-19 local-27 local-28 "service-name" 1) local-13 (compute local-14 local-15 local-17 local-20 6)]
(let [local-5 (get-data local-8 local-10 local-11 local-12 "service-name" 5) local-6 (compute local-11 local-12 local-14 local-15 local-16 local-18 2) local-7 (get-data local-8 local-9 local-10 local-11 local-12 local-16 "service-name" 4)]
(let [local-2 (compute local-6 local-8 local-9 7) local-3 (compute local-5 local-6 local-12 local-13 2) local-4 (get-data local-8 local-10 local-12 "service-name" 3)]
(compute local-2 local-3 local-4 local-7 local-24 5)
))))))))
))
(defn run_test_level8_0 []
(ohua
(let [local-19 (compute  6) local-20 (get-data  "service-name" 3) local-21 (compute  6) local-22 (compute  7)]
(let [local-17 (compute local-20 local-21 local-22 5) local-18 (compute  1)]
(let [local-15 (write-data local-18 local-20 "service-name" 3) local-16 (compute local-17 local-19 6)]
(let [local-12 (get-data local-15 local-18 "service-name" 2) local-13 (compute local-15 local-16 local-17 local-18 local-20 5) local-14 (get-data local-15 local-17 "service-name" 5)]
(let [local-7 (compute local-12 local-13 local-15 local-20 local-21 2) local-8 (compute local-12 local-14 local-15 local-18 4) local-9 (get-data local-12 local-14 "service-name" 5) local-10 (get-data local-21 "service-name" 7) local-11 (get-data local-12 local-13 local-14 local-16 local-19 "service-name" 5)]
(let [local-5 (write-data local-7 local-8 local-10 local-14 "service-name" 2) local-6 (compute local-11 local-16 5)]
(let [local-2 (get-data local-10 "service-name" 3) local-3 (get-data local-6 local-8 local-10 local-13 "service-name" 5) local-4 (compute local-11 local-12 3)]
(get-data local-2 local-3 local-4 local-5 local-9 "service-name" 1)
)))))))
))
(defn run_test_level7_0 []
(ohua
(let [local-15 (compute  6) local-16 (compute  5) local-17 (get-data  "service-name" 4)]
(let [local-13 (compute local-15 local-17 1) local-14 (get-data local-15 local-16 local-17 "service-name" 3)]
(let [local-9 (write-data local-13 local-14 local-15 "service-name" 1) local-10 (get-data local-13 local-14 local-15 local-16 "service-name" 7) local-11 (compute local-13 4) local-12 (get-data local-16 "service-name" 2)]
(let [local-5 (compute local-9 local-11 local-14 7) local-6 (compute local-9 local-12 local-15 local-16 1) local-7 (get-data local-9 local-11 local-12 local-13 local-14 local-17 "service-name" 6) local-8 (get-data local-10 local-11 local-12 "service-name" 6)]
(let [local-3 (get-data local-6 local-7 local-8 local-10 local-12 "service-name" 6) local-4 (compute local-6 local-12 7)]
(let [local-2 (compute  2)]
(get-data local-2 local-3 local-4 local-5 "service-name" 3)
))))))
))
(defn run_test_level6_0 []
(ohua
(let [local-10 (compute  6) local-11 (get-data  "service-name" 4) local-12 (write-data  "service-name" 2)]
(let [local-9 (get-data local-10 local-11 "service-name" 6)]
(let [local-7 (compute local-11 1) local-8 (compute local-11 4)]
(let [local-5 (compute local-7 local-10 1) local-6 (compute  7)]
(let [local-2 (get-data local-6 local-8 "service-name" 5) local-3 (get-data local-5 local-6 local-7 local-8 "service-name" 1) local-4 (compute local-6 7)]
(get-data local-2 local-3 local-4 local-9 local-12 "service-name" 5)
)))))
))
(defn run_test_level5_0 []
(ohua
(let [local-9 (compute  7) local-10 (compute  3) local-11 (compute  3)]
(let [local-7 (compute local-9 local-10 local-11 2) local-8 (get-data local-9 local-10 local-11 "service-name" 6)]
(let [local-3 (compute local-7 local-8 5) local-4 (compute local-9 local-10 4) local-5 (get-data local-8 "service-name" 7) local-6 (get-data local-7 local-11 "service-name" 7)]
(let [local-2 (compute local-4 local-6 local-7 1)]
(get-data local-2 local-3 local-5 "service-name" 2)
))))
))
(defn run_test_level4_0 []
(ohua
(let [local-6 (get-data  "service-name" 4)]
(let [local-4 (write-data local-6 "service-name" 2) local-5 (compute  4)]
(let [local-2 (get-data local-4 local-5 "service-name" 2) local-3 (compute local-4 local-5 local-6 6)]
(write-data local-2 local-3 "service-name" 2)
)))
))
(defn run_test_level3_0 []
(ohua
(let [local-5 (compute  5) local-6 (compute  2) local-7 (get-data  "service-name" 7)]
(let [local-2 (compute local-7 5) local-3 (get-data local-5 local-7 "service-name" 3) local-4 (compute local-5 local-6 1)]
(compute local-2 local-3 local-4 2)
))
))
(defn run_test_level2_0 []
(ohua
(let [local-2 (get-data  "service-name" 7) local-3 (get-data  "service-name" 2)]
(compute local-2 local-3 5)
)
))
(defn run_test_level1_0 []
(ohua
(get-data  "service-name" 6)

))
(defn run_test_level10_1 []
(ohua
(let [local-24 (compute  3) local-25 (compute  3) local-26 (compute  4) local-27 (compute  2)]
(let [local-22 (write-data local-27 "service-name" 7) local-23 (compute local-26 local-27 2)]
(let [local-19 (get-data local-22 "service-name" 3) local-20 (compute local-23 local-25 local-27 5) local-21 (get-data local-22 local-23 local-27 "service-name" 4)]
(let [local-16 (get-data local-19 local-20 local-21 local-23 "service-name" 3) local-17 (write-data local-20 local-22 local-27 "service-name" 6) local-18 (write-data local-19 local-20 local-21 local-25 "service-name" 7)]
(let [local-12 (compute local-16 local-17 local-18 3) local-13 (get-data local-16 local-19 local-24 "service-name" 7) local-14 (get-data local-18 local-22 "service-name" 3) local-15 (get-data local-21 local-23 "service-name" 3)]
(let [local-10 (get-data local-12 local-13 local-14 local-15 local-17 "service-name" 7) local-11 (get-data local-14 local-21 "service-name" 7)]
(let [local-7 (get-data local-10 local-13 local-20 "service-name" 2) local-8 (get-data local-11 "service-name" 5) local-9 (get-data local-11 local-13 local-14 local-20 local-21 "service-name" 4)]
(let [local-4 (get-data local-7 local-8 local-15 local-16 "service-name" 7) local-5 (get-data local-8 local-16 "service-name" 7) local-6 (get-data local-8 local-14 local-26 "service-name" 5)]
(let [local-2 (get-data local-6 local-19 local-25 "service-name" 4) local-3 (write-data local-5 local-6 local-14 "service-name" 5)]
(get-data local-2 local-3 local-4 local-9 "service-name" 2)
)))))))))
))
(defn run_test_level9_1 []
(ohua
(let [local-24 (compute  1) local-25 (get-data  "service-name" 4)]
(let [local-21 (compute local-24 6) local-22 (compute local-24 2) local-23 (compute local-24 local-25 4)]
(let [local-16 (compute local-23 5) local-17 (write-data local-21 "service-name" 1) local-18 (get-data local-21 local-22 local-23 local-25 "service-name" 3) local-19 (compute  7) local-20 (compute local-22 local-23 3)]
(let [local-11 (compute local-16 local-17 local-18 local-19 local-20 7) local-12 (compute local-19 local-25 5) local-13 (get-data local-19 local-21 local-22 local-23 "service-name" 2) local-14 (compute local-19 local-20 local-24 2) local-15 (write-data local-16 local-18 local-19 local-20 local-23 "service-name" 5)]
(let [local-10 (compute local-11 local-13 local-17 local-20 1)]
(let [local-8 (get-data local-10 local-14 "service-name" 6) local-9 (get-data local-10 local-12 "service-name" 7)]
(let [local-5 (compute local-17 7) local-6 (write-data local-8 local-10 local-13 local-20 "service-name" 3) local-7 (compute local-8 local-16 local-20 5)]
(let [local-2 (get-data local-5 local-6 local-8 local-11 local-12 "service-name" 7) local-3 (get-data local-16 "service-name" 2) local-4 (get-data local-5 local-8 local-9 "service-name" 4)]
(compute local-2 local-3 local-4 local-7 local-15 6)
))))))))
))
(defn run_test_level8_1 []
(ohua
(let [local-15 (write-data  "service-name" 3) local-16 (compute  1)]
(let [local-13 (get-data local-16 "service-name" 7) local-14 (get-data local-16 "service-name" 5)]
(let [local-9 (get-data local-13 local-15 "service-name" 1) local-10 (compute local-14 local-16 6) local-11 (compute local-13 1) local-12 (get-data local-15 "service-name" 1)]
(let [local-7 (get-data local-10 local-11 "service-name" 7) local-8 (write-data local-11 local-13 "service-name" 4)]
(let [local-6 (compute local-7 1)]
(let [local-4 (write-data local-9 local-12 "service-name" 6) local-5 (get-data local-6 local-7 "service-name" 6)]
(let [local-2 (compute local-4 local-8 1) local-3 (write-data local-4 local-6 "service-name" 1)]
(get-data local-2 local-3 local-5 "service-name" 3)
)))))))
))
(defn run_test_level7_1 []
(ohua
(let [local-20 (compute  4) local-21 (get-data  "service-name" 5) local-22 (get-data  "service-name" 2) local-23 (compute  6) local-24 (compute  5)]
(let [local-16 (get-data local-22 local-23 "service-name" 2) local-17 (get-data local-20 local-22 local-24 "service-name" 5) local-18 (compute local-21 local-22 local-23 local-24 6) local-19 (compute local-21 local-22 local-23 local-24 1)]
(let [local-14 (get-data local-16 local-17 local-19 local-21 local-22 "service-name" 6) local-15 (write-data local-18 local-19 local-24 "service-name" 7)]
(let [local-11 (get-data local-15 local-16 "service-name" 5) local-12 (compute local-17 local-18 local-19 6) local-13 (compute local-14 local-15 local-17 local-24 4)]
(let [local-6 (compute local-13 5) local-7 (compute local-12 local-13 local-14 local-16 local-18 7) local-8 (get-data local-12 local-14 local-19 local-20 "service-name" 5) local-9 (get-data local-11 local-14 "service-name" 4) local-10 (get-data local-12 local-14 local-24 "service-name" 7)]
(let [local-2 (compute local-8 local-11 local-13 local-15 5) local-3 (compute local-6 local-7 local-9 local-10 local-11 local-12 local-13 local-15 local-16 local-20 7) local-4 (compute  3) local-5 (get-data local-7 local-9 local-11 local-17 "service-name" 3)]
(write-data local-2 local-3 local-4 local-5 "service-name" 1)
))))))
))
(defn run_test_level6_1 []
(ohua
(let [local-11 (get-data  "service-name" 1) local-12 (get-data  "service-name" 7)]
(let [local-9 (get-data local-11 local-12 "service-name" 5) local-10 (compute local-11 local-12 7)]
(let [local-7 (compute local-9 6) local-8 (compute local-9 local-10 local-12 7)]
(let [local-4 (compute local-7 local-8 local-12 1) local-5 (compute local-9 local-11 1) local-6 (compute local-7 local-8 1)]
(let [local-2 (compute local-4 local-5 local-6 local-8 2) local-3 (get-data local-6 "service-name" 6)]
(get-data local-2 local-3 "service-name" 3)
)))))
))
(defn run_test_level5_1 []
(ohua
(let [local-12 (compute  2) local-13 (get-data  "service-name" 6) local-14 (get-data  "service-name" 5)]
(let [local-10 (compute local-12 local-13 3) local-11 (compute local-12 local-13 local-14 5)]
(let [local-5 (compute local-11 3) local-6 (get-data  "service-name" 4) local-7 (get-data local-10 local-13 "service-name" 2) local-8 (get-data local-11 "service-name" 4) local-9 (get-data local-14 "service-name" 5)]
(let [local-2 (get-data local-7 local-9 "service-name" 5) local-3 (get-data local-5 local-7 local-8 local-11 "service-name" 4) local-4 (get-data local-5 local-7 local-8 local-9 local-10 "service-name" 7)]
(write-data local-2 local-3 local-4 local-6 "service-name" 3)
))))
))
(defn run_test_level4_1 []
(ohua
(let [local-8 (write-data  "service-name" 2) local-9 (compute  3)]
(let [local-5 (write-data local-8 local-9 "service-name" 1) local-6 (compute local-8 3) local-7 (write-data local-8 local-9 "service-name" 7)]
(let [local-2 (compute local-7 local-9 5) local-3 (write-data local-6 local-7 "service-name" 7) local-4 (write-data local-5 local-6 "service-name" 2)]
(get-data local-2 local-3 local-4 "service-name" 4)
)))
))
(defn run_test_level3_1 []
(ohua
(let [local-5 (compute  4) local-6 (compute  2) local-7 (get-data  "service-name" 3) local-8 (get-data  "service-name" 4)]
(let [local-2 (write-data local-6 local-7 local-8 "service-name" 3) local-3 (compute local-6 local-7 local-8 3) local-4 (get-data local-5 local-7 local-8 "service-name" 7)]
(compute local-2 local-3 local-4 3)
))
))
(defn run_test_level2_1 []
(ohua
(let [local-2 (get-data  "service-name" 7)]
(get-data local-2 "service-name" 6)
)
))
(defn run_test_level1_1 []
(ohua
(compute  6)

))