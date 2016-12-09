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
;; otherwise we don't change

(defn visit-node
  "visit a location and then update all its neighbors' information"
         [graph
          location
          routine]
         (let [neighbors (graph location)
               dist-from-source (if-let [curnode-in-path (get routine
       location)]
                           (get curnode-in-path :dist)
                           0)
       ;; This is the source node, so we'll give it 0 as distance
               new-routine (->> neighbors ;; for each of its neighbors
                             (map (fn [[neighbor dist-to-cur-node]]
                                    (f-update 
                                              location 
                                              neighbor
                                              dist-from-source
                                              dist-to-cur-node
                                              (get (get routine neighbor) :dist)
                                     ) ;; use the f-update function to update the routine book
                                    ) 
                             )
                             (into routine)
                        )
               ]
                ;; which we'll return as new-path
              new-routine) ;; return value, an updated routine
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
          [graph 
           source   
           target]
          
          (let [dijkstra-shortest-paths (dijkstra graph)]
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







