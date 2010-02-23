(ns clj-validate
  "A library to build validation functions that validate clojure data structures
   against certain conditions."
  (:use clojure.contrib.seq-utils)
  (:use clojure.test))

(def +default-level-order+ [:info :warning :error :critical])

(defn level-pos
  ([l] "Returns the position of the supplied keyword in the default level order"
     (level-pos l +default-level-order+))
  ([l lo]"Returns the position of the supplied keyword in the given level order"
   (or ((zipmap lo (iterate inc 0)) l) -1)))

(with-test
  (defn max-level 
    ([levels level-order]
       (apply max-key #(level-pos % level-order) levels))
    ([levels]
       (max-level levels +default-level-order+)))

  (testing "single"
    (is (= :foo (max-level [:foo]))))
  (testing "maxing" 
    (is (= :critical (max-level [:warning :critical :foo])))
    (is (= :warning  (max-level [:warning :warning :info]))))
  (testing "custom levelmap"
    (is (= :foo (max-level [:foo :bar :baz] [:baz :bar :foo])))))


(with-test 
  (defn combine [& rs]    
    "Combines a list of results to a result with a level being the maximum level
     of the individual results. The reason if the result will be the result list.

     Example: =((combine { :level :warning :info foo} { :level :error :info :boo})
                { :level :error :reason [{:level :warning :info :foo}
                                         {:level :error :info :boo}]})"

    (let [rs (filter (comp not nil?) rs)]
      (if (empty? rs) nil 
	   (let [level (max-level (map :level rs))]
	     {:level level :reason (vec rs)}))))

  (let [e {:level :error :reason :e}
	w {:level :warning :reason :w}]
    (testing "empty and nils"
      (is (nil? (combine)) "empty")
      (is (nil? (combine nil)) "nil")
      (is (nil? (combine nil nil)) "nils"))
    (testing "single"
      (is (= {:level :error :reason [e]}
	     (combine e)) "error")
      (is (= {:level :warning :reason [w]}
	     (combine w)) "reason"))
    (testing "multiple"
      (is (= {:level :error :reason [e w]} (combine e w)) "error warning")
      (is (= {:level :error :reason [w e]} (combine w e)) "warning error"))
    (testing "some nil"
      (is (= {:level :error :reason [e w]} (combine e nil w)) "error nil warning"))))

(defn validate 
  ([f name x]
     (validate f name nil nil x))
  ([f name info x]
     (validate f name info nil x))
  ([f name info reason x]
     (when-not (f x) 
       (-> {:level :error}
	   (#(if name (assoc % :validator name) %))
	   (#(if reason (assoc % :reason reason) %))
	   (#(if info (assoc % :info info) %))))))

(with-test
  (defn numeric [x]
    (validate number? :numeric x))
  (testing "numeric"
    (are (nil? (numeric 0))
	(nil? (numeric 1/3))
	(={:level :error :reason :numeric} (numeric "a")))))

(defn string [x]
  (validate string? :string x))

(defn odd [x]
  (validate odd? :odd x))

(defn is-ifn [x]
  (validate ifn? :ifn x))

(with-test
  (defn less-than [l] 
    (fn [x] (validate #(< % l) :less-than l x)))
  (testing "simple"
    (is (nil? ((less-than 5) 0)))
    (is (= { :level :error :validator :less-than :info 5 } ((less-than 5) 10)))))

(with-test
  (defn greater-than [l] 
    (fn [x] (validate #(> % l) :greater-than l x)))
  (testing "simple"
    (is (nil? ((greater-than 0) 5)))
    (is (= { :level :error :validator :greater-than :info 10 } ((greater-than 10) 5)))))
(with-test
  (defn applied [f validator name]
    (fn [x] (if-let [rs (combine (validator (f x)))]
	      (assoc rs :validator name))))
  (testing "count"
    (is (nil? ((applied count (less-than 5) :count) [1 2 3 4])))
    (is (= {:level :error :validator :count 
	    :reason [{:level :error :validator :less-than :info 5}] } 
	   ((applied count (less-than 5) :count) [1 2 3 4 5])))))

(with-test
  (defn all [& validators]
    (fn [x] (if-let [rs (apply combine (map #(% x) validators))]
	      (assoc rs :validator :all))))
  (testing "single"
    (is (nil? ((all numeric) 5)))
    (is (= {:level :error :validator :all :reason [{:level :error :validator :numeric}]}
	   ((all numeric) "a"))))
  (testing "multiple"
    (is (nil? ((all numeric odd) 5)))
    (is (= {:level :error :validator :all :reason [{:level :error :validator :odd}
						   {:level :error :validator :less-than :info 5}]}
	   ((all odd (less-than 5)) 20)))))

(defn is-not [name validator]
  (fn [x] (when-not (validator x)
	    { :level :error :validator not :reason name })))

(defn coll-empty [coll]
  (fn [x] (if (coll? x)
	    (validate empty? :empty x)
	    { :level :error :validator :empty :reason :not-coll })))

(with-test
  (defn in [key validator]
    (fn [x] (if (coll? x)
	      (if (contains? x key)
		(if-let [rs (combine (validator (x key)))]
		  (assoc rs :validator :in :info key))
		{:level :error :validator :in :reason :not-contains :info key})
	      
	      {:level :error :validator :in :reason :not-coll :info x})))

  (testing "map"
    (is (nil? ((in :a numeric) { :a 1 })))
    (is (nil? ((in :a numeric) { :a 1 :b 2})))
    (is (= { :level :error :validator :in :reason [{:level :error :validator :numeric}] :info :a}
	   ((in :a numeric) { :a "a"})))
    (is (= { :level :error :validator :in :reason :not-contains :info :a}
	   ((in :a numeric) {})))))

(with-test
  (defn all-in-coll [validator]
    (fn [coll] (if (coll? coll)
		 (if-let [rs (apply combine (map #(if-let [r (validator (coll %))]
						    (assoc r :key %)) 
						 (range (count coll))))]
		   (assoc rs :validator :coll))
		 { :level :error :validator :all-in-coll :reason :not-coll :info coll})))
  (testing "valid"
    (is (nil? ((all-in-coll numeric) [])) "empty")
    (is (nil? ((all-in-coll numeric) [1])) "single")
    (is (nil? ((all-in-coll numeric) [1 2])) "two entries"))
  (testing "invalid"
    (is (= {:level :error :validator :coll 
	    :reason [{:level :error :key 0 :validator :numeric}]} 
	   ((all-in-coll numeric) ["a"])) "single invalid")
    (is (= {:level :error :validator :coll 
	    :reason [{:level :error :key 0 :validator :numeric}
		     {:level :error :key 2 :validator :numeric}]} 
	   ((all-in-coll numeric) ["a" 1 "b"])) "two invalid")
    (is (= {:level :error :validator :coll 
	    :reason [{:level :error :key 1 :validator :numeric}]} 
	   ((all-in-coll numeric) [1 "a" 2])) "invalid and valid")))

(with-test
  (defn is-equal [e]
    #(if-let [rs (validate (partial = e) :is-equal %)]
       (assoc rs :info e)))
  (testing "simple"
    (is (nil? ((is-equal :a) :a)) "valid")
    (is (= {:level :error :validator :is-equal :info :a}
	   ((is-equal :a) :b)))))

(with-test
  (defn coll-contains [key]
    (fn [coll] (validate #(contains? % key) :contains key coll)))
  (testing "simple"
    (is (nil? ((coll-contains :a) { :a 1 })))
    (is (= {:level :error :validator :contains :info :a} ((coll-contains :a) { :b 1 })))))

(def positive (partial validate pos? :positive))
(defn not-equal [v] (partial validate (partial not= v) :not-equal v))

(with-test
  (defn any [& validators]
    (fn [x] (let [rs (map #(% x) validators)]
	      (if-not (or (empty? rs) (some nil? rs))
		(let [rs (apply combine rs)]
		  (assoc rs :validator :or :reason (:reason rs)))))))
  (testing "simple"
    (is (nil? ((any) :dummy)) "not validators")
    (is (nil? ((any numeric) 1)) "single valid")
    (is (nil? ((any numeric positive) 1)) "multiple valid")
    (is (= {:level :error :validator :or :reason [{:level :error :validator :odd}]}
	   ((any odd) -2)) "single invalid")
    (is (= {:level :error :validator :or :reason [{:level :error :validator :positive}
						  {:level :error :validator :odd}]}
	   ((any positive odd) -2))) "multpile invalid"))

(defn with-level [level v]
  #(if-let [rs (v %)]
     (assoc rs :level level)))

(defn warn [v & args]
  (with-level :warn (apply v args)))

(run-tests)