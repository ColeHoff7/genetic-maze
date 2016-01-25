(ns ga-maze.core (:require [quil.core :as q]))    

(comment
  Made by Cole Hoffbauer
  First try at doing anything with Genetic Algorithms
  Code may not be the best optimized
  Uses the GA to try and get individuals through a maze
)

(def genSize 50)
(def moves 20);; must be even
(def mutationChance 20);; (100-xx)%
(declare breed)
(declare -main)               
(declare moveMe)

(def dataStuff (atom {:square 1 :gen 1 :ind 1}))

;(comment
(defn setup []
  (q/frame-rate 200)                    
  (q/background 200))                 
                                      
(defn draw []
  (q/background 190 190 190)
  (q/text-size 16)
  (q/text (str @dataStuff) 10 540)
  (q/fill 255)
  (q/rect 0 0 500 500)
  ;;drawing grid
  (q/stroke 230)
  (q/rect 100 0 0 500)
  (q/rect 200 0 0 500)
  (q/rect 300 0 0 500)
  (q/rect 400 0 0 500)
  (q/rect 0 100 500 0)
  (q/rect 0 200 500 0)
  (q/rect 0 300 500 0)
  (q/rect 0 400 500 0)
  ;;drawing lines for maze
  (q/stroke 0)
  (q/rect 200 0 0 100)
  (q/rect 200 100 200 0)
  (q/rect 100 100 0 300)
  (q/rect 0 400 100 0)
  (q/rect 200 200 300 0)
  (q/rect 200 300 300 0)
  (q/rect 200 400 200 0)
  (q/rect 200 300 0 100)
  (q/rect 0 500 500 0)
  ;;drawing food
  (q/fill 255 0 0)
  (q/ellipse 250 50 30 30)
  ;;drawing ind
  (q/fill 0 0 255)
  (let [s (@dataStuff :square)]
    (cond
      (= s 1) (q/ellipse 50 450 40 40)
      (= s 2) (q/ellipse 150 450 40 40)
      (= s 3) (q/ellipse 250 450 40 40)
      (= s 4) (q/ellipse 350 450 40 40)
      (= s 5) (q/ellipse 450 450 40 40)
      (= s 6) (q/ellipse 50 350 40 40)
      (= s 7) (q/ellipse 150 350 40 40)
      (= s 8) (q/ellipse 250 350 40 40)
      (= s 9) (q/ellipse 350 350 40 40)
      (= s 10) (q/ellipse 450 350 40 40)
      (= s 11) (q/ellipse 50 250 40 40)
      (= s 12) (q/ellipse 150 250 40 40)
      (= s 13) (q/ellipse 250 250 40 40)
      (= s 14) (q/ellipse 350 250 40 40)
      (= s 15) (q/ellipse 450 250 40 40)
      (= s 16) (q/ellipse 50 150 40 40)
      (= s 17) (q/ellipse 150 150 40 40)
      (= s 18) (q/ellipse 250 150 40 40)
      (= s 19) (q/ellipse 350 150 40 40)
      (= s 20) (q/ellipse 450 150 40 40)
      (= s 21) (q/ellipse 50 50 40 40)
      (= s 22) (q/ellipse 150 50 40 40)
      (= s 23) (q/ellipse 250 50 40 40)
      (= s 24) (q/ellipse 350 50 40 40)
      (= s 25) (q/ellipse 450 50 40 40)
      ))
  )         

(q/defsketch example                  
  :title "Maze"    
  :settings #(q/smooth 2)             
  :setup setup                        
  :draw draw                          
  :size [500 550]) 
;)


(defn animateInd [[ind & rest] genNum indNum currentSquare]
  (if (not= nil ind)
    (do
      (let [newSquare (moveMe currentSquare ind)]
        
        ;(println "Square: " newSquare "Gen: " genNum "Ind: " indNum)
        (swap! dataStuff assoc :square newSquare :gen genNum :ind indNum)
        
        (Thread/sleep 50)
        (animateInd rest genNum indNum newSquare)))))

(defn animate [[f & rest] genNum indNum]
  (Thread/sleep 1000)
  (if (< indNum genSize)
    (do (animateInd f genNum indNum 1) (animate rest genNum (inc indNum)))))

;;up down left right
(def validMoves {:1 [nil nil nil 2], :2 [7 nil 1 3], :3 [nil nil 2 4], :4 [nil nil 3 5], :5 [10 nil 4 nil], :6 [11 nil nil nil], :7 [12 2 nil nil], :8 [nil nil nil 9], :9 [nil nil 8 10], :10 [nil 5 9 nil],
                 :11 [16 6 nil nil], :12 [17 7 nil 13], :13 [nil nil 12 14], :14 [nil nil 13 15], :15 [nil nil 14 nil], :16 [21 11 nil nil], :17 [22 12 nil 18], :18 [nil nil 17 19],
                 :19 [nil nil 18 20], :20 [25 nil 19 nil], :21 [nil 16 nil 22], :22 [nil 17 21 nil], :23 [nil nil nil nil], :24 [nil nil 23 25], :25 [nil 20 24 nil]})

(defn moveMe "Moves the individual to their next location if they can move that way. If they hit a wall return currentSquare" [currentSquare direction] 
  (if (not= nil (nth ((keyword (str currentSquare)) validMoves) direction))  (nth ((keyword (str currentSquare)) validMoves) direction) currentSquare))


(defn createFirstGen "Randomly generates the first generation of maze-inhabitants, returns a list of genSize lists with 10 nums 0-3. Recursive" [gen num]
  (if (< num genSize) 
    (createFirstGen (conj gen (take moves (repeatedly #(rand-int 4)))) (inc num)) ;;add another to the map
    gen));;otherwise return the map as is

(defn endingSquare "finds the square that a member of a generation ends on" [currentSquare [firstMove & rest]]
  (if (= nil firstMove)
    currentSquare
    (let [newSquare (moveMe currentSquare firstMove)] 
      (if (not= nil newSquare) (endingSquare newSquare rest) (endingSquare currentSquare rest)))))
  

(defn fitness "Returns a number (0-15) based on how fit the member of the generation is. Used to find who got closest to the food" [ind]
  (let [end (endingSquare 1 ind)] 
    (cond
      (= 1 end) 5
      (= 2 end) 6
      (= 3 end) 5
      (= 4 end) 4
      (= 5 end) 3
      (= 6 end) 4
      (= 7 end) 7
      (= 8 end) 0
      (= 9 end) 1
      (= 10 end) 2
      (= 11 end) 5
      (= 12 end) 8
      (= 13 end) 7
      (= 14 end) 6
      (= 15 end) 5
      (= 16 end) 6
      (= 17 end) 9
      (= 18 end) 10
      (= 19 end) 11
      (= 20 end) 12
      (= 21 end) 7
      (= 22 end) 8
      (= 23 end) 15
      (= 24 end) 14
      (= 25 end) 13)))

(defn determineNextGen "Determines the fittest individuals of a generation and sends them off to do the no-no dance and make the babies. Returns a new generation ready to face the horrors the maze" [generation]
  (let [sorted (sort-by fitness generation)]
    (breed '() (list (last sorted) (nth sorted (- (count sorted) 2)) (nth sorted (- (count sorted) 3)) (nth sorted (- (count sorted) 4))))))

(defn breed "This is where the magic happens boys. Returns a new generation which has learned from the blunders of the previous generation. Recursive. four possible parents, bred at random" [generation fittest]
  (let [mom (nth fittest (rand-int (count fittest))) dad (nth fittest (rand-int (count fittest)))] 
    (if (> (count generation) (- genSize 1))
      generation ;;returns the generation if the size is up to genSize
       (let [child (flatten (conj (take-last (/ moves 2) mom) (take (/ moves 2) dad))) mutated-allele (rand-int moves)] ;;otherwise it recursively calls breed adding a new child to the generation
         (if (> (rand-int 100) mutationChance) ;;mutation chance
           (let [mutatedChild (seq (assoc (vec child) mutated-allele (rand-int 4)))] (breed (conj generation mutatedChild) fittest))
             (breed (conj generation child) fittest))))))

(defn printGenResults [genNumber fittest]
  (println)
  (println "New Generation: " genNumber)
  (println "fittest" fittest))

(defn generationManager "The 'hub' method I guess. Where every generation gets sent to and gets animated and sent off to make a new gen" [gen genNumber]
  (animate gen genNumber 0)
  (let [fittest (fitness (last (sort-by fitness gen)))]
    (printGenResults genNumber fittest)
    
     (if (< fittest 15)
       (generationManager (determineNextGen gen) (inc genNumber))
       genNumber
       )))

(defn statGetter "runs the simulation x times and returns the average number of generations it took" [avg i x] 
  (println i)
  (if (> i x)
    (/ (apply + avg) (count avg))
    (statGetter (conj avg (-main)) (inc i) x)))

(defn getStats [x]
  (statGetter '() 0 x))
        
      

(defn -main [] 
  ;;(swap! currentGen merge (createFirstGen {} 0))
  (let [gen (createFirstGen '() 0)]
   (generationManager gen 1)))

(-main)




