;   Copyright (c) Philipp Meier. All rights reserved.
;   The use and distribution terms for this software are covered by the
;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;   which can be found in the file epl-v10.html at the root of this distribution.
;   By using this software in any fashion, you are agreeing to be bound by
;   the terms of this license.
;   You must not remove this notice, or any other, from this software.

(ns test.clj-validate
  (:require [clj-validate :as v])
  (:use clojure.test))

(defn forbidden-name [name]
  (fn [x] (v/validate #(not (.contains % name)) :forbidden-name name x)))

(defn jabba-validate [j]
  ((v/all (v/in :name  (forbidden-name "Jabba"))
	(v/in :numbers (v/all (v/all-in-coll v/positive)
			  (v/applied count (v/is-equal 4) :count)))
	(v/in :location (v/all (v/coll-contains :city)
			   (v/in :planet (v/not-equal "Tattoine"))))
	(v/in :foos
	    (v/all-in-coll 
	     (v/all
	      (v/any (v/warn v/coll-contains :size) 
		   (v/warn v/coll-contains :length))
	      (v/in :name 
		  (v/warn v/applied count (v/less-than 2) :count)))))) j))

(def jabba-valid 
     {:name "Dschabba the Hutt"
      :age 441
      :numbers [1 2 3 1]
      :location {:planet "Endor" :city "Ewok home town"}
      :foos [{:name "d" :length 1} {:name "a" :size 5}]})

(def jabba-invalid
     {:name "Jabba the Hutt"
      :age 441
      :numbers [1 2 -3]
      :location {:planet "Tattoine"}
      :foos [{:name "d" } {:name "ab" :size 5}]})

(def expected 
     {:validator :all,
      :level :error,
      :reason
      [{:info :name,
	:validator :in,
	:level :error,
	:reason
	[{:info "Jabba", :validator :forbidden-name, :level :error}]}
       {:info :numbers,
	:validator :in,
	:level :error,
	:reason
	[{:validator :all,
	  :level :error,
	  :reason
	  [{:validator :coll,
	    :level :error,
	    :reason [{:key 2, :validator :positive, :level :error}]}
      {:validator :count,
       :level :error,
       :reason [{:info 4, :validator :is-equal, :level :error}]}]}]}
       {:info :location,
	:validator :in,
	:level :error,
	:reason
	[{:validator :all,
	  :level :error,
	  :reason
	  [{:info :city, :validator :contains, :level :error}
	   {:info :planet,
	    :validator :in,
	    :level :error,
	    :reason
	    [{:info "Tattoine", :validator :not-equal, :level :error}]}]}]}
       {:info :foos,
	:validator :in,
	:level :warn,
	:reason
	[{:validator :coll,
	  :level :warn,
	  :reason
	  [{:key 0,
	    :validator :all,
	    :level :warn,
	    :reason
	    [{:validator :or,
	      :level :warn,
	      :reason
	      [{:info :size, :validator :contains, :level :warn}
	       {:info :length, :validator :contains, :level :warn}]}]}
	   {:key 1,
	    :validator :all,
	    :level :warn,
	    :reason
	    [{:info :name,
	      :validator :in,
	      :level :warn,
	      :reason
	      [{:validator :count,
		:level :warn,
		:reason
           [{:info 2, :validator :less-than, :level :error}]}]}]}]}]}]})


(deftest jabba-test
  (testing "Failure"
    (is (= expected (jabba-validate jabba-invalid))))
  (testing "Valid"
    (is (nil? (jabba-validate jabba-valid)))))
