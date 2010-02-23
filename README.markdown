A clojure library to validate data structures.

Licensed under EPL 1.0


Sample Code (see also test)
---------------------------

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

    (jabba-validate jabba-valid) ;; -> nil

