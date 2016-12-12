;; author Zihe Wang (王子赫)
;; netid zwang199

(ns proj296.core)

(defrecord Map [Buildings Paths])
;; Buildings stores all the names of buildings on campus, in string format
;; Paths is a hash-map, the K & V are--- building name: paths from it

;; at the begining, I don't realize the real power of Clojure and I learnt Data Structure in JAVA
;; so I implemented these useless functions which are important in java graph

(defn get_paths_from
	"get all paths that from x"
	[cPaths x]
	(if (nil? cPaths) nil
		(get cPaths x))
	)


(defn is_near_or_not
	"if x and y adjacent"
	[cPaths x y]
	((not nil) ? (get (get cPaths x) y))
	)

(defn what_building_does_UOFI_have
	[Cmap]
	(:Buildings Cmap)
	)



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
;; the add_a_path function will  return a new UIUC map
;; if the adding path already exist in the map, the older version will be replaced
;; this function is only designed to initialize the UIUC map

(defn delete_a_node
	"delete a location in the campus map"
	[old-map location]
	(let [ new_name_book (filter (fn [x] (not= x location)) (:Buildings old-map))
		   map_del_node  (dissoc (:Paths old-map)     location)
		   new_Path		 (->> (keys map_del_node)
		   					  (map (fn [x] (dissoc (get map_del_node x) location)))
		   					  (zipmap (keys map_del_node))
		   					)
		]
		(Map. new_name_book new_Path)
		)
	)




;; When we are finding the shortest path,
;; I introduced a new map named routine
;; {node {:dist :prev}}
;; in order to implement dijkstra
(defn f-update
         [location
          neighbor
          dist-from-source
          dist-from-node
          dist-to-neighbor]
         (let [dist-new-neighbor (+ dist-from-source dist-from-node)]
           (if (or  (nil? dist-to-neighbor)  (and (not (nil? dist-to-neighbor)) (< dist-new-neighbor dist-to-neighbor)))
;; dis-so-far is null or if our location to neighbor is quicker than our previous routine
;; we update the better routine to the "routine" book
  {neighbor {:dist dist-new-neighbor :prev location}})))


(defn visit-node
  "visit a location and then update all its neighbors' information"
         [graph
          location
          routine]
         (let [neighbors (graph location)
               dist-from-source (if-let [curnode-in-path (get routine location)]
                                        (get curnode-in-path :dist) 0)
       ;; This is the source node, so we'll give it 0 as distance
               new-routine (->> neighbors
                             (map (fn [[neighbor dist-to-cur-node]]
                                    (f-update 
                                              location 
                                              neighbor
                                              dist-from-source
                                              dist-to-cur-node  
                                              (get (get routine neighbor) :dist))))
                             ;; we use map function to apply f-update on ever neighbor
                             (into routine))
               ]
               new-routine
               ) ;; return an updated routine
)

(defn dijkstra
         [graph]
         (loop [
                stack (keys graph)  ;; initialize, 
                routine {}
                ]
           (if (seq stack) 
             (let [current-location (first stack)]
               (recur (rest stack)
                      (visit-node 
                                  graph 
                                  current-location 
                                  routine)))
              routine)
           )
)

(defn shortest-path
          [Cmap 
           source   
           target]
          
          (let [  graph                   (:Paths Cmap)
                  dijkstra-shortest-paths (dijkstra graph)]
;; we compute the shortest paths
                (loop [new-target (get dijkstra-shortest-paths target)
                       routine [target]]

                      (let [next-new-target (get new-target :prev)]
                            (cond (nil? next-new-target) :error
                                  (= next-new-target source) (into [source] routine)
                                  :else (recur (get dijkstra-shortest-paths
                                         next-new-target)
       ;; we recur using the previous node
       ;; and adding it to the result path
                                  (into [next-new-target] routine)))
                            )
                      )
                )
          )

