;; author Zihe Wang (王子赫)
;; netid zwang199

(ns proj296.core)

(defrecord Map [Buildings Paths])
;; Buildings stores all the names of buildings on campus, in string format
;; Paths is a hash-map, the K & V are--- building name: paths from it


(defn get_paths_from
	"get all paths that from x"
	[cPaths x]
	(if (nil? cPaths) nil
		(get cPaths x)
		)
	)

(defn is_near_or_not
	"if x and y adjacent"
	[cPaths x y]
	((not nil) ? (get (get cPaths x) y))
	)

()

(defn add_a_path
	"add a path to the graph, at the same time, update the verticle book"
	[Cmap x y minutes]
	(if (nil? (:Buildings Cmap)) (Map. (list x y) (hash-map x (hash-map y minutes), y (hash-map x minutes)))
		(Map. (distinct (conj (:Buildings Cmap) x y)) 
				(merge  (:Paths Cmap)
						(merge (hash-map x (merge ((:Paths Cmap) x) (hash-map y minutes))) 
				               (hash-map y (merge ((:Paths Cmap) y) (hash-map x minutes))))
						)


			)
		)
	)














