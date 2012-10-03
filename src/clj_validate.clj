(ns clj-validate
  "A library to build validation functions that validate clojure data structures
   against certain conditions."
  (:use clojure.test))

(with-test 
  (defn combine [& rs]    
    "Combines a list of results to a result which references the results as causes.

     Example: =((combine {:info foo} {:info :boo})
                {:cause [{:info :foo}
                         {:info :boo}]})"

    (let [rs (remove nil? rs)]
      (if-not (empty? rs)
        {:cause rs})))

  (let [e {:cause :e}
	w {:cause :w}]
    (testing "empty and nils"
      (is (nil? (combine)) "empty")
      (is (nil? (combine nil)) "nil")
      (is (nil? (combine nil nil)) "nils"))
    (testing "single"
      (is (= {:cause [e]} (combine e))))
    (testing "multiple preserves order"
      (is (= {:cause [e w]} (combine e w)))
      (is (= {:cause [w e]} (combine w e))))
    (testing "some nil are filtered"
      (is (= {:cause [e w]} (combine e nil w))))))

(defn validate 
  ([f name x]
     (validate f name nil nil x))
  ([f name info x]
     (validate f name info nil x))
  ([f name info cause x]
     (try
       (when-not (f x)
         (merge (when name {:validator name})
                (when cause {:cause cause})
                (when info {:info info})))
       (catch Exception e
	 {:validator name
	  :cause :exception
	  :info info
	  :exception e}))))

(with-test
  (defn numeric [x]
    (validate number? :numeric x))
  (testing "numeric"
    (is (nil? (numeric 0)))
    (is (nil? (numeric 1/3)))
    (is (= {:validator :numeric} (numeric "a")))))

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
    (is (= {:validator :less-than :info 5} ((less-than 5) 10)))))

(with-test
  (defn greater-than [l] 
    (fn [x] (validate #(> % l) :greater-than l x)))
  (testing "simple"
    (is (nil? ((greater-than 0) 5)))
    (is (= {:validator :greater-than :info 10} ((greater-than 10) 5)))))

(with-test
  (defn in-range [l h] 
    (fn [x] (validate #(<= l % h) :in-range [l h] x)))
  (testing "simple"
    (is (nil? ((in-range 4 6) 5)))
    (is (nil? ((in-range 5 5) 5)))
    (is (nil? ((in-range 4 5) 5)))
    (is (nil? ((in-range 5 6) 5)))
    (is (= {:validator :in-range :info [3 4]} ((in-range 3 4) 5)))
    (is (= {:validator :in-range :info [6 7]} ((in-range 6 7) 5)))))

(with-test
  (defn applied [f validator name]
    (fn [x] (if-let [rs (combine (validator (f x)))]
	      (assoc rs :validator name))))
  (testing "count"
    (is (nil? ((applied count (less-than 5) :count) [1 2 3 4])))
    (is (= {:validator :count 
	    :cause [{:validator :less-than :info 5}] } 
	   ((applied count (less-than 5) :count) [1 2 3 4 5])))))

(with-test
  (defn all [& validators]
    (fn [x] (if-let [rs (apply combine (map #(% x) validators))]
	      (assoc rs :validator :all))))
  (testing "single"
    (is (nil? ((all numeric) 5)))
    (is (= {:validator :all :cause [{:validator :numeric}]}
	   ((all numeric) "a"))))
  (testing "multiple"
    (is (nil? ((all numeric odd) 5)))
    (is (= {:validator :all :cause [{:validator :odd}
                                     {:validator :less-than :info 5}]}
	   ((all odd (less-than 5)) 20)))))

(with-test
  (defn is-not [name validator]
    (fn [x] (if-not (validator x)
             {:validator name})))
  (testing "in-not"
    (is (nil? ((is-not :non-numeric numeric) "a")))
    (is (= {:validator :non-numeric}
           ((is-not :non-numeric numeric) 1)))))


(with-test
  (defn in [key validator]
    (fn [x] (if (coll? x)
	      (if (contains? x key)
		(if-let [rs (combine (validator (x key)))]
		  (assoc rs :validator :in :info key))
		{:validator :in :cause :not-contains :info key})
	      
	      {:validator :in :cause :not-coll :info key})))

  (testing "map"
    (is (nil? ((in :a numeric) { :a 1 })))
    (is (nil? ((in :a numeric) { :a 1 :b 2})))
    (is (= {:validator :in :cause [{:validator :numeric}] :info :a}
	   ((in :a numeric) { :a "a"})))
    (is (= {:validator :in :cause :not-contains :info :a}
	   ((in :a numeric) {})))))

(with-test
  (defn all-in-seq [validator]
    (fn [seq] (if (sequential? seq)
               (if-let [rs (apply combine
                                  (map-indexed #(if-let [r (validator %2)]
                                                  (assoc r :index %1))
                                               seq))]
		   (assoc rs :validator :all-in-seq))
               {:validator :all-in-seq :info :not-sequential})))
  (testing "valid"
    (is (nil? ((all-in-seq numeric) [])) "empty")
    (is (nil? ((all-in-seq numeric) [1])) "single")
    (is (nil? ((all-in-seq numeric) [1 2])) "two entries"))
  (testing "invalid"
    (is (= ((all-in-seq numeric) "a")
           {:validator :all-in-seq :info :not-sequential})
        "not a coll")
    (is (= {:validator :all-in-seq :cause [{:index 0 :validator :numeric}]} 
	   ((all-in-seq numeric) ["a"])) "single invalid")
    (is (= {:validator :all-in-seq :cause [{:index 0 :validator :numeric}
                                           {:index 2 :validator :numeric}]} 
	   ((all-in-seq numeric) ["a" 1 "b"])) "two invalid")
    (is (= {:validator :all-in-seq :cause [{:index 1 :validator :numeric}]} 
	   ((all-in-seq numeric) [1 "a" 2])) "invalid and valid")))

(with-test
  (defn is-equal [e]
    #(if-let [rs (validate (partial = e) :is-equal %)]
       (assoc rs :info e)))
  (testing "simple"
    (is (nil? ((is-equal :a) :a)) "valid")
    (is (= {:validator :is-equal :info :a}
	   ((is-equal :a) :b)))))

(with-test
  (defn coll-contains [key]
    (fn [coll] (validate #(contains? % key) :contains key coll)))
  (testing "simple"
    (is (nil? ((coll-contains :a) { :a 1 })))
    (is (= {:validator :contains :info :a} ((coll-contains :a) { :b 1 })))))

(defn positive [x] (validate pos? :positive x))
(defn negative [x] (validate neg? :positive x))
(defn not-equal [x] (fn [y] (validate #(not= x %) :not-equal x y)))
(defn empty [seq] (validate empty? :empty seq))
(defn not-empty [seq] (validate #(not (empty? %)) :not-empty seq))

(with-test
  (defn any [& validators]
    (fn [x] (let [rs (map #(% x) validators)]
	      (if-not (or (empty? rs) (some nil? rs))
		(let [rs (apply combine rs)]
		  (assoc rs :validator :any :cause (:cause rs)))))))
  (testing "simple"
    (is (nil? ((any) :dummy)) "not validators")
    (is (nil? ((any numeric) 1)) "single valid")
    (is (nil? ((any numeric positive) 1)) "multiple valid")
    (is (= {:validator :any :cause [{:validator :odd}]}
	   ((any odd) 2)) "single invalid")
    (is (= {:validator :any :cause [{:validator :positive}
                                    {:validator :odd}]}
	   ((any positive odd) -2))) "multpile invalid")
    (is (nil? ((any odd negative) 1)) "single invalid amoung valid"))