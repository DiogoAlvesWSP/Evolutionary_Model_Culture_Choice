;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Model of cultural adaptation and group change            ;;
;; Builds on an evolutionary metaphor (see "Info" flap)     ;;
;; Developed by Diogo Jorge Vieira Alves                    ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Requires the rnd extension to do a weighted random draw from a distribution:
extensions [rnd]

; Breeds, agent types and global and agent variables:
breed [Partygoers partygoer]

globals
[
  total
  center-area
  area-group-0
  area-group-1
  area-group-2
  area-group-3
  area-group-4
  ; group/culture statistics are collected in group chromosomes:
  group-chromosome-group-0
  group-chromosome-group-1
  group-chromosome-group-2
  group-chromosome-group-3
  group-chromosome-group-4
  mean-group-chromosome-group-0
  mean-group-chromosome-group-1
  mean-group-chromosome-group-2
  mean-group-chromosome-group-3
  mean-group-chromosome-group-4
  refractors-group-0
  refractors-group-1
  refractors-group-2
  refractors-group-3
  refractors-group-4

  ; Global variables that subsume statistics of interest:
  proportion-group-0
  proportion-group-1
  proportion-group-2
  proportion-group-3
  proportion-group-4
  list-proportions
  agent-deaths
]

Partygoers-own
[
  id; unique turtle identifier
  age; Age in weeks
  culture; group or culture agent belongs to.
  individual-chromosome ; of length 6, measures the attitudes of partygoers w.r.t the culture traits of groups
  distance-individual-group; number that quantifies the disagreement between partygoer and culture he's in
]

; Useful reporters and procedures for the rest of the code that are used by agents:
to-report chromosome-distance [chromosome1 chromosome2]
  ;; compute the euclidean distance between allele distributions (taken from Robby the Robot model!)
  let dist sum (map [ [a1 a2] -> (a1  - a2) ^ 2 ] chromosome1  chromosome2)
  report dist
end

to-report aggregate-lists [list-of-lists]
  ; Takes all valuations of partygoers w.r.t a given topic and adds them.
  report reduce [[?1 ?2] -> (map + ?1 ?2)] list-of-lists
end

to-report add-to-total [x]
  set total (x + total)
  report total
end

to-report get-weighted-list [chromosome a-length]
  ; Takes a social chromosome and divides its elements by a given length (partygoer variable, will equal group length)
  let divided-vec map [ i -> i / a-length ] chromosome
  set divided-vec map [ i -> precision i 3 ] divided-vec
  report divided-vec
end

to-report weighted-rand [relative-weights]
  ; This reporter performs a weighted random draw given the size distribution of cultures.
  ; If the appropriate parameter is turned on, partygoer is more likely to choose a culture with more people
  let cultures [0 1 2 3 4]
  ; Combines the two in an appropriate form:
let pairs (map list cultures relative-weights)
  ; Do the weighting and the random draw:
let choice rnd:weighted-one-of-list pairs [ [p] -> last p ]
  ; return the corresponding culture:
  report item 0 choice
end

; Set of twin procedures to determine how many agents change cultures:
to-report list-difference [culture-list1 culture-list2]
 let a (map != culture-list1 culture-list2)
report a
end

; The next takes as input the list of booleans produced by list-difference:
to-report boolean-to-numeric [list-of-booleans]
  let bool list-of-booleans
  let bool2 map [ i -> ifelse-value ( i = true ) [ 1 ] [ 0 ] ] bool
report bool2
end

to-report take-ordered-subset [number list-elements]
  ; reporter that filters the first n refractory elements of each culture
  report sublist list-elements 0 min list number (length list-elements)
end

to-report get-closest-group [own-chromosome list-of-chromosomes]
  ; reporter that, given a partygoer chromosome and a list of social chromosomes, returns the group with lowest
  ; euclidean distance. Used by partygoers to determine their (next) preferred group. Returns the group closest to agent.
  let distance-list []
  ; for debugging purposes:
  set list-of-chromosomes (list mean-group-chromosome-group-0 mean-group-chromosome-group-1 mean-group-chromosome-group-2
  mean-group-chromosome-group-3 mean-group-chromosome-group-4)
  set distance-list map [ i -> chromosome-distance own-chromosome i ] list-of-chromosomes
  let closest-group position min distance-list distance-list
  report closest-group
end

; Mutation and crossover between genetic strings (adapted from the Simple GA model):
to-report crossover [chromosome1 chromosome2]
  ;Assume a fixed splitting point for now WLOG, right at the middle of the chromosome:
  let split-point 3
  report (ifelse-value random-float 100.0 < social-influenceability
    ; if the random draw is smaller than crossover-rate return crossovered chromosome
  [one-of (list (sentence (sublist chromosome1 0 split-point)
                        (sublist chromosome2 split-point length chromosome2))
              (sentence (sublist chromosome2 0 split-point)
        (sublist chromosome1 split-point length chromosome1)))]
    ; else, return simply original chromosome:
      [chromosome1]
    )
end

;; This procedure causes random mutations to occur in a solution's bits.
to-report mutate [chromosome1]
  ; Custom mutation operator that adds or subtracts 1 to a random sentiment over a topic by a partygoer
  let support [-1 1]
  let allele random length chromosome1
  let bit-to-flip item allele chromosome1
  report (ifelse-value random-float 100.0 < mutation-rate
      [ replace-item allele chromosome1 (bit-to-flip + one-of support)]
      [ chromosome1]
  )
end

to-report patches-in-territory [Center]
  let ptr []
  ask Center [set ptr patches in-radius 4]
  report ptr
end

; Reporter that, given a list of chromosomes, performs a set of n crossovers and mutation (with a given probability):
; Setup of agent properties and world.
; Need to have: each culture sprouts in a different part of the world
; Possibly, have agents utility depend on number of other partygoers in group.

to setup
  clear-all
  reset-ticks
  random-seed 47822
  ; Next follows an auxiliary procedure that initializes a variable to build partial sums:
  set total 0
  setup-partygoers
end

to setup-partygoers
 set area-group-0 patches-in-territory patch 12 -12
 set area-group-1 patches-in-territory patch 12 12
 set area-group-2 patches-in-territory patch -12 -12
 set area-group-3 patches-in-territory patch 0 12
 set area-group-4 patches-in-territory patch -12 0
 set center-area patches-in-territory patch 0 0

  ifelse (initial-groups?)
  ; If an initial distribution is specified. Code is a bit clumsy but works for now:
      [
    let group-sizes read-from-string initial-group-sizes
    let cumsum map add-to-total group-sizes
    create-partygoers sum group-sizes
    ask partygoers[set id who
    hide-turtle]
    ; Group 0:
    ask partygoers with [id < item 0 cumsum][
      set culture 0
      set size 2
      set age starting-age * 52 + random (30 - starting-age) * 52
      set shape "person"
      ; Agents' color depends on their group
      coloring-of-groups
      set individual-chromosome n-values 6 [one-of[-1 0 1]]
      move-to-area
    show-turtle]
    ; Group 1:
    ask partygoers with [id >= item 0 cumsum and id < item 1 cumsum][
      set culture 1
      set size 2
      set age starting-age * 52 + random (30 - starting-age) * 52
      set shape "person"
      ; Agents' color depends on their group
      coloring-of-groups
      set individual-chromosome n-values 6 [one-of[-1 0 1]]
      move-to-area
    show-turtle]

    ask partygoers with [id >= item 1 cumsum and id < item 2 cumsum][
      set culture 2
      set size 2
      set age starting-age * 52 + random (30 - starting-age) * 52
      set shape "person"
      ; Agents' color depends on their group
      coloring-of-groups
      set individual-chromosome n-values 6 [one-of[-1 0 1]]
      move-to-area
    show-turtle]

    ask partygoers with [id >= item 2 cumsum and id < item 3 cumsum][
      set culture 3
      set size 2
      set age starting-age * 52 + random (30 - starting-age) * 52
      set shape "person"
      ; Agents' color depends on their group
      coloring-of-groups
      set individual-chromosome n-values 6 [one-of[-1 0 1]]
      move-to-area
    show-turtle]

    ask partygoers with [id >= item 3 cumsum and id < item 4 cumsum][
      set culture 4
      set size 2
      set age starting-age * 52 + random (30 - starting-age) * 52
      set shape "person"
      ; Agents' color depends on their group
      coloring-of-groups
      set individual-chromosome n-values 6 [one-of[-1 0 1]]
      move-to-area
    show-turtle]


  ]; end of specification of initial distribution
  ; if no initial groups are specified, but rather an initial population is specified:
      [
  create-partygoers initial-population
  ask partygoers [
    set culture random 5
    set size 2
    set id who
    set age starting-age * 52 + random (30 - starting-age) * 52
    set shape "person"
    ; Agents' color depends on their group
    coloring-of-groups
    set individual-chromosome n-values 6 [one-of[-1 0 1]]
    move-to-area
  ]
  ]; end the ifelse case for no groups
end

to move-to-area
  ; Agents move to be closer to their kin:
  ask partygoers with [culture = 0][move-to one-of area-group-0]
  ask partygoers with [culture = 1][move-to one-of area-group-1]
  ask partygoers with [culture = 2][move-to one-of area-group-2]
  ask partygoers with [culture = 3][move-to one-of area-group-3]
  ask partygoers with [culture = 4][move-to one-of area-group-4]
end

to move-to-center
  move-to one-of center-area
end

to go-once
  if ticks >= number-of-periods * 52 [ stop ]
  coloring-of-groups
  compute-group-attitudes
  compute-distance-group-personal
  determine-refractories
  trade-genes-and-reset-culture
  move-to-area
  tick
  coloring-of-groups
  ; increments their age:
  ask partygoers [set age age + 1]
  set agent-deaths 0
  (ifelse random-float 100.0 < death-probability
  [ask n-of death-probability partygoers [ die ]
    set agent-deaths n-of death-probability partygoers]
  [set agent-deaths 0])
  ; compute and collect statistics of interest:
  compute-statistics
end

to coloring-of-groups
  ; Not very efficient coding but works for now, singles out groups and colors them differently
  ask partygoers with [culture = 0][
  set color red
  ]
  ask partygoers with [culture = 1][
  set color yellow
  ]
  ask partygoers with [culture = 2][
  set color green
  ]
  ask partygoers with [culture = 3][
  set color brown
  ]
  ask partygoers with [culture = 4][
  set color blue
  ]
end

; Procedures that are implemented by agents and are the cornerstone in their decision method:
to compute-group-attitudes
  ; This is the procedure that does most of the heavy lifting in the model:
  set group-chromosome-group-0 []
  set group-chromosome-group-1 []
  set group-chromosome-group-2 []
  set group-chromosome-group-3 []
  set group-chromosome-group-4 []
  set mean-group-chromosome-group-0 []
  set mean-group-chromosome-group-1 []
  set mean-group-chromosome-group-2 []
  set mean-group-chromosome-group-3 []
  set mean-group-chromosome-group-4 []

  ask partygoers with [culture = 0][
    ; ask agents within the group to determine global attitudes of group w.r.t a given topic, first by having them place chromosomes inside:
    set group-chromosome-group-0 lput [individual-chromosome] of self group-chromosome-group-0]
    ; then by aggregating opinions (sum of the individual chromosomes):
    set group-chromosome-group-0 aggregate-lists group-chromosome-group-0
    ; The computing the weighted mean:
    set mean-group-chromosome-group-0 get-weighted-list group-chromosome-group-0 count partygoers with [culture = 0]

  ask partygoers with [culture = 1][
    set group-chromosome-group-1 lput [individual-chromosome] of self group-chromosome-group-1]

    ; then by aggregating opinions (sum of the individual chromosomes):
    set group-chromosome-group-1 aggregate-lists group-chromosome-group-1
    ; The computing the weighted mean:
    set mean-group-chromosome-group-1 get-weighted-list group-chromosome-group-1 count partygoers with [culture = 1]


  ask partygoers with [culture = 2][
    set group-chromosome-group-2 lput [individual-chromosome] of self group-chromosome-group-2]
    ; then by aggregating opinions (sum of the individual chromosomes):
    set group-chromosome-group-2 aggregate-lists group-chromosome-group-2
    ; The computing the weighted mean:
    set mean-group-chromosome-group-2 get-weighted-list group-chromosome-group-2 count partygoers with [culture = 2]

  ask partygoers with [culture = 3][
    set group-chromosome-group-3 lput [individual-chromosome] of self group-chromosome-group-3]

    ; then by aggregating opinions (sum of the individual chromosomes):
    set group-chromosome-group-3 aggregate-lists group-chromosome-group-3
    ; The computing the weighted mean:
    set mean-group-chromosome-group-3 get-weighted-list group-chromosome-group-3 count partygoers with [culture = 3]

  ask partygoers with [culture = 4][
    set group-chromosome-group-4 lput [individual-chromosome] of self group-chromosome-group-4]
    ; then by aggregating opinions (sum of the individual chromosomes):
    set group-chromosome-group-4 aggregate-lists group-chromosome-group-4
    ; The computing the weighted mean:
    set mean-group-chromosome-group-4 get-weighted-list group-chromosome-group-4 count partygoers with [culture = 4]

end

to compute-distance-group-personal
  ; Self-explanatory
  ask partygoers with [culture = 0][
    ; ask agents within the group to determine global attitudes of group w.r.t a given topic, first by having them place chromosomes inside:
      set distance-individual-group chromosome-distance [individual-chromosome] of self mean-group-chromosome-group-0]

    ask partygoers with [culture = 1][
    ; ask agents within the group to determine global attitudes of group w.r.t a given topic, first by having them place chromosomes inside:
      set distance-individual-group chromosome-distance [individual-chromosome] of self mean-group-chromosome-group-1]

    ask partygoers with [culture = 2][
    ; ask agents within the group to determine global attitudes of group w.r.t a given topic, first by having them place chromosomes inside:
      set distance-individual-group chromosome-distance [individual-chromosome] of self mean-group-chromosome-group-2]

    ask partygoers with [culture = 3][
    ; ask agents within the group to determine global attitudes of group w.r.t a given topic, first by having them place chromosomes inside:

      set distance-individual-group chromosome-distance [individual-chromosome] of self mean-group-chromosome-group-3
  ]
    ask partygoers with [culture = 4][
    ; ask agents within the group to determine global attitudes of group w.r.t a given topic, first by having them place chromosomes inside:
      set distance-individual-group chromosome-distance [individual-chromosome] of self mean-group-chromosome-group-4
  ]
end

to determine-refractories
  ; Determines which agents within each group are going to leave for another, based on a sorting of their personal-group distance
    set refractors-group-0 sort-on [(- distance-individual-group)] partygoers with [culture = 0]
  ; Filters to a given percentage of it (user-controlled in a slider):
    set refractors-group-0 take-ordered-subset (refractory-rate / 100 * length refractors-group-0) refractors-group-0
  ; rest is exactly the same syntax, but now for other groups:
    set refractors-group-1 sort-on [(- distance-individual-group)] partygoers with [culture = 1]
    set refractors-group-1 take-ordered-subset (refractory-rate / 100 * length refractors-group-1) refractors-group-1
    set refractors-group-2 sort-on [(- distance-individual-group)] partygoers with [culture = 2]
    set refractors-group-2 take-ordered-subset (refractory-rate / 100 * length refractors-group-2) refractors-group-2
    set refractors-group-3 sort-on [(- distance-individual-group)] partygoers with [culture = 3]
    set refractors-group-3 take-ordered-subset (refractory-rate / 100 * length refractors-group-3) refractors-group-3
    set refractors-group-4 sort-on [(- distance-individual-group)] partygoers with [culture = 4]
    set refractors-group-4 take-ordered-subset (refractory-rate / 100 * length refractors-group-4) refractors-group-4
end

to trade-genes-and-reset-culture
  ; by this procedure, all refractories move to the center of the world
  ; and trade information (mutation and recombination), thus updating their genes of opinions about topics:
  ; This is the main function of the algorithm.
  ; Uses preferential attachment when computing the new culture (see weighted-rand in topics-to-consider, which picks out the chromosome of the most "popular" culture
  ; (in terms of probability)
  let list-of-group-topics (list mean-group-chromosome-group-0 mean-group-chromosome-group-1 mean-group-chromosome-group-2
  mean-group-chromosome-group-3 mean-group-chromosome-group-4)
  let list-of-weighted-cultures (list proportion-group-0 proportion-group-1 proportion-group-2 proportion-group-3 proportion-group-4)

  foreach refractors-group-0 [a -> ask a [
    set color white
    let topics-to-consider []
    ; move to center of world:
    move-to-center
    ; Uses preferential attachment: will take the chromosome of the culture that has the highest number of partygoers, by distribution
    set topics-to-consider item weighted-rand list-of-weighted-cultures list-of-group-topics
    ifelse (evolution?)
      [set individual-chromosome mutate crossover individual-chromosome topics-to-consider]
      [set individual-chromosome individual-chromosome]
    ; reset closests culture
    set culture get-closest-group individual-chromosome list-of-group-topics
    ]
  ]

  foreach refractors-group-1 [a -> ask a [
    set color white
    let topics-to-consider []
    ; move to center of world:
    move-to-center
   set topics-to-consider item weighted-rand list-of-weighted-cultures list-of-group-topics
    ifelse (evolution?)
      [set individual-chromosome mutate crossover individual-chromosome topics-to-consider]
      [set individual-chromosome individual-chromosome]
    set culture get-closest-group individual-chromosome list-of-group-topics
    ]]

  foreach refractors-group-2 [a -> ask a [
    let topics-to-consider []
    set color white
    ; move to center of world:
    move-to-center
    set topics-to-consider item weighted-rand list-of-weighted-cultures list-of-group-topics
    ifelse (evolution?)
      [set individual-chromosome mutate crossover individual-chromosome topics-to-consider]
      [set individual-chromosome individual-chromosome]
    set culture get-closest-group individual-chromosome list-of-group-topics
    ]]

  foreach refractors-group-3 [a -> ask a [
    let topics-to-consider []
    set color white
    ; move to center of world:
    move-to-center

    set topics-to-consider item weighted-rand list-of-weighted-cultures list-of-group-topics
    ifelse (evolution?)
      [set individual-chromosome mutate crossover individual-chromosome topics-to-consider]
      [set individual-chromosome individual-chromosome]
    set culture get-closest-group individual-chromosome list-of-group-topics
    ]]

  foreach refractors-group-4 [a -> ask a [
    let topics-to-consider []
    set color white
    ; move to center of world:
    move-to-center

    set topics-to-consider item weighted-rand list-of-weighted-cultures list-of-group-topics
    ifelse (evolution?)
      [set individual-chromosome mutate crossover individual-chromosome topics-to-consider]
      [set individual-chromosome individual-chromosome]
    set culture get-closest-group individual-chromosome list-of-group-topics
    ]]
end

to compute-statistics
  ; This procedure yields the observables we want to see by computing them each period
  set proportion-group-0 precision (count partygoers with [culture = 0] / count partygoers) 2
  set proportion-group-1 precision (count partygoers with [culture = 1] / count partygoers) 2
  set proportion-group-2 precision (count partygoers with [culture = 2] / count partygoers) 2
  set proportion-group-3 precision (count partygoers with [culture = 3] / count partygoers) 2
  set proportion-group-4 precision (count partygoers with [culture = 4] / count partygoers) 2
  set list-proportions (list proportion-group-0 proportion-group-1 proportion-group-2 proportion-group-3 proportion-group-4)
end
@#$#@#$#@
GRAPHICS-WINDOW
391
145
664
419
-1
-1
8.030303030303031
1
10
1
1
1
0
1
1
1
-16
16
-16
16
0
0
1
ticks
30.0

BUTTON
85
141
151
174
Setup
setup
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

INPUTBOX
34
216
103
276
starting-age
21.0
1
0
Number

TEXTBOX
150
112
428
152
Control Flow
16
0.0
1

TEXTBOX
24
10
809
98
Evolutionary model of culture selection and culture growth
22
0.0
1

SLIDER
25
337
133
370
social-influenceability
social-influenceability
0
100
80.0
1
1
NIL
HORIZONTAL

SLIDER
136
337
244
370
mutation-rate
mutation-rate
0
100
8.0
1
1
NIL
HORIZONTAL

TEXTBOX
136
323
286
341
Mutation rate in %:
11
0.0
1

TEXTBOX
25
323
175
341
Crossover rate in %:
11
0.0
1

TEXTBOX
100
297
324
337
Socio-Genetic Parameters
16
0.0
1

TEXTBOX
137
373
287
391
Refractory rate in %:
11
0.0
1

SLIDER
135
388
243
421
refractory-rate
refractory-rate
0
99
84.0
1
1
NIL
HORIZONTAL

TEXTBOX
122
187
272
207
Adjustable numbers
16
0.0
1

BUTTON
157
141
223
174
go * 1
go-once
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

SLIDER
24
388
132
421
initial-population
initial-population
0
1000
250.0
100
1
NIL
HORIZONTAL

TEXTBOX
25
373
175
391
Initial population:
11
0.0
1

BUTTON
229
141
302
174
Go
go-once
T
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

TEXTBOX
154
80
487
165
User input
18
12.0
1

TEXTBOX
424
109
692
153
Graphical representation
18
12.0
1

TEXTBOX
800
65
1080
109
Plotting and output monitoring
18
12.0
1

SWITCH
247
337
345
370
evolution?
evolution?
0
1
-1000

TEXTBOX
248
323
398
341
Evolutionary change?
11
0.0
1

PLOT
679
117
908
267
Proportion of Agents in each subculture
Time
Proportions
0.0
10.0
0.0
1.0
true
true
"" ""
PENS
"0" 1.0 0 -2674135 true "" "plot proportion-group-0"
"1" 1.0 0 -1184463 true "" "plot proportion-group-1"
"2" 1.0 0 -10899396 true "" "plot proportion-group-2"
"3" 1.0 0 -6459832 true "" "plot proportion-group-3"
"4" 1.0 0 -13345367 true "" "plot proportion-group-4"

PLOT
938
117
1167
267
Histogram of group size
Cultures
Num Partygoers
0.0
5.0
0.0
200.0
true
true
"" ""
PENS
"Culture" 1.0 1 -16777216 true "" "histogram [culture] of partygoers"

INPUTBOX
107
216
170
276
number-of-periods
5.0
1
0
Number

PLOT
678
293
908
443
Sentiment over a topic by a subculture
time
sentiment
0.0
10.0
-1.0
1.0
true
true
"" "\n"
PENS
"Topic0" 1.0 0 -16777216 true "set mean-group-chromosome-group-0 [0 0 0 0 0 0]" "plot item 0 mean-group-chromosome-group-0"
"Topic1" 1.0 0 -7500403 true "" "plot item 1 mean-group-chromosome-group-0"
"Topic2" 1.0 0 -2674135 true "" "plot item 2 mean-group-chromosome-group-0"
"Topic3" 1.0 0 -955883 true "" "plot item 3 mean-group-chromosome-group-0"
"Topic4" 1.0 0 -6459832 true "" "plot item 4 mean-group-chromosome-group-0"
"Topic5" 1.0 0 -1184463 true "" "plot item 5 mean-group-chromosome-group-0"

INPUTBOX
268
216
380
276
initial-group-sizes
[150 100 70 20 20]
1
0
String

SWITCH
174
242
264
275
initial-groups?
initial-groups?
1
1
-1000

TEXTBOX
178
215
254
241
Does user define initial group size?
9
0.0
1

PLOT
940
292
1168
442
"Deaths"
time
Agents disappearing
0.0
10.0
0.0
10.0
true
false
"" ""
PENS
"default" 1.0 0 -16777216 true "" "(ifelse agent-deaths != 0\n[plot count agent-deaths]\n[plot 0]\n)"

TEXTBOX
679
101
924
129
Proportion of agents in each subculture over time
11
0.0
1

TEXTBOX
939
100
1089
118
Subculture Sizes by # Agents
11
0.0
1

TEXTBOX
679
276
999
304
How does subculture 0 feel about the set of topics?
11
0.0
1

TEXTBOX
940
274
1211
302
Proportion of agents leaving per time step
11
0.0
1

MONITOR
1174
163
1230
208
Cluster-0
proportion-group-0
17
1
11

MONITOR
1174
209
1230
254
Cluster-1
proportion-group-1
17
1
11

MONITOR
1174
255
1230
300
Cluster-2
proportion-group-2
17
1
11

MONITOR
1174
302
1230
347
Cluster-3
proportion-group-3
17
1
11

MONITOR
1174
349
1229
394
Cluster-4
proportion-group-4
17
1
11

SLIDER
246
388
346
421
death-probability
death-probability
0
10
1.0
1
1
NIL
HORIZONTAL

TEXTBOX
246
373
428
401
Exogenous death rate
11
0.0
1

@#$#@#$#@
## WHAT IS IT?

This is an original model of (sub)culture diffusion built by Diogo Jorge Vieira Alves. 
It features a set of agents (dubbed "partygoers") organized initially in clusters, having properties such as age and a chromosome of opinions about 6 different topics. The partygoers interact with a set of cultures (also having a set of opinions subsuming those of its members), in the sense of refractory or unhappy members of each setting about to find a new culture and trading information encoded in the genetic string (originally encoded as -1, 0, and 1, resp. a negative, neutral, and positive opinion about each of the 6 traits/aspects, e.g. the use of recreational drugs). There are 5 subcultures that both influence (through the aforementioned genetic operations of mutation and recombination of chromosomes simulating exchange of opinions) and are influenced by its members (since a group is a weighted average of the opinions and actions of its constituents). The objective of this feedback loop is to investigate under which conditions certain subculture sizes emerge, but the model is open to many other kinds of explorations as well.

## HOW IT WORKS

Agents are initially grouped in one of the 5 cultures, displayed by a different color and location in the world. The user can either define a random initial distribution, in which case all groups will have similar number of members initially (in a statistical sense, that is), or specify the size of each group. 
Each tick, standing for a weekend, the agents compute the distance between their own chromosome of opinions and that of their current group, and the percentage refractory-rate of each of the group elements that is most disgruntled will move to the center to seek a new subculture.

The agent chromosome, if under the evolutionary regime, will change by recombination and mutation with that of a culture the agent comes in contact with (in a preferential attachment scheme, i.e cultures with more members will tend to be selected for genetic recombination). The agent, with her new vector of opinions, will then compute the distance between this vector and that of the cultures and move to the one closer to her own opinion. The subcultures will change as a result of this movement, thus making agents reevaluate their preferences and start a new shift.

There is an exogenous probability of death, which should be kept low (say 1 or 2 %). 

## HOW TO USE IT

Start by pressing SETUP to initialize the model. The user has a large number of tuneable parameters to explore, including the initial age and number of periods of the simulation (currently at 21 years and 5 years). 
A random population of agents randomly split among the cultures can be defined if the initial-group-size switch is turned off. If on, then the user should input the number of agents in each subculture initially.
As per standard practice, pressing GO will start the simulation for the specified number of periods.
If the evolutionary switch (EVOLUTION?) is turned off, then agents won't change their chromosomes and the model behavior is flat in terms of group change, as expected. 
The most interesting case by far is when the evolutionary switch is turned on, since then agents will change their opinions by performing genetic exchange with group chromosomes.
The standard crossover rate (SOCIAL-INFLUENCEABILITY) controls how many crossovers of opinions between group and individual chromosomes will occur along the simulation. The mutation slider (MUTATION-RATE) controls the probability with which a random element from an individual agent chromosome will suffer an increase or decrease by 1, i.e a random change in opinion of a given agent with respect to a random topic of the 6.
The REFRACTORY-RATE slider controls the percentage of agents within each group that will set out to find a new culture.

It is recommended to press SETUP in normal speed, but then have the simulation run at a somewhat reduced speed in order to see the agents congregating in the center of the world, turning white (in their "disgruntled" stage) and then adopting a new color and moving to the corner where their new group is located. 

## THINGS TO NOTICE

The agents looking for a new group will join the biggest culture with a probability proportional to its current size in terms of agent membership. Do these waves of incoming agents correspond to massive shifts in group size and group opinion about topics?

Interestingly, the REFRACTORY-RATE does parameter does not need to be set high in order for big shifts in group composition and size to appear. The MUTATION-RATE and SOCIAL-INFLUENCEABILITY parameters introduce the variety needed for large shifts to occur, as they will affect both the incoming agent to a group, and the group chromosome itself. 

## THINGS TO TRY

Try moving the SOCIAL-INFLUENCEABILITY and MUTATION-RATE sliders, and in particular check the difference in amplitude in group composition and size with and without the evolutionary mode on. It is interesting to see that turning that parameter on, even for relatively low values of crossover, mutation, and refractory rates, increases the subculture shuffling by several orders of magnitude.

## EXTENDING THE MODEL

Possibilities of extension are manifold. A possibility is to make the interests of each agent, i.e the chromosome alleles, depend on the age (both of these are currently random), if a suitable description of each of the dimensions is forthcoming.

Another is to change the distance metric the agents use to determine how unhappy they are in the group.

## RELATED MODELS

This model draws heavy inspiration, in the sense of borrowing either the underlying philosophy, or even bits and bobs of code, from the following:

- K-means
- Preferential Attachment
- Echo. 

The first one provided the inspiration of matching agents to data clusters, the PA supplied the popular mechanism of the "Rich get richer" in order to lend stability to cluster sizes, and the Echo family of models provided the inspiration for the evolutionary part of the model and matching of agent alleles. This model is, in some sense, a mutation and recombination of all these.
@#$#@#$#@
default
true
0
Polygon -7500403 true true 150 5 40 250 150 205 260 250

airplane
true
0
Polygon -7500403 true true 150 0 135 15 120 60 120 105 15 165 15 195 120 180 135 240 105 270 120 285 150 270 180 285 210 270 165 240 180 180 285 195 285 165 180 105 180 60 165 15

arrow
true
0
Polygon -7500403 true true 150 0 0 150 105 150 105 293 195 293 195 150 300 150

box
false
0
Polygon -7500403 true true 150 285 285 225 285 75 150 135
Polygon -7500403 true true 150 135 15 75 150 15 285 75
Polygon -7500403 true true 15 75 15 225 150 285 150 135
Line -16777216 false 150 285 150 135
Line -16777216 false 150 135 15 75
Line -16777216 false 150 135 285 75

bug
true
0
Circle -7500403 true true 96 182 108
Circle -7500403 true true 110 127 80
Circle -7500403 true true 110 75 80
Line -7500403 true 150 100 80 30
Line -7500403 true 150 100 220 30

butterfly
true
0
Polygon -7500403 true true 150 165 209 199 225 225 225 255 195 270 165 255 150 240
Polygon -7500403 true true 150 165 89 198 75 225 75 255 105 270 135 255 150 240
Polygon -7500403 true true 139 148 100 105 55 90 25 90 10 105 10 135 25 180 40 195 85 194 139 163
Polygon -7500403 true true 162 150 200 105 245 90 275 90 290 105 290 135 275 180 260 195 215 195 162 165
Polygon -16777216 true false 150 255 135 225 120 150 135 120 150 105 165 120 180 150 165 225
Circle -16777216 true false 135 90 30
Line -16777216 false 150 105 195 60
Line -16777216 false 150 105 105 60

car
false
0
Polygon -7500403 true true 300 180 279 164 261 144 240 135 226 132 213 106 203 84 185 63 159 50 135 50 75 60 0 150 0 165 0 225 300 225 300 180
Circle -16777216 true false 180 180 90
Circle -16777216 true false 30 180 90
Polygon -16777216 true false 162 80 132 78 134 135 209 135 194 105 189 96 180 89
Circle -7500403 true true 47 195 58
Circle -7500403 true true 195 195 58

circle
false
0
Circle -7500403 true true 0 0 300

circle 2
false
0
Circle -7500403 true true 0 0 300
Circle -16777216 true false 30 30 240

cow
false
0
Polygon -7500403 true true 200 193 197 249 179 249 177 196 166 187 140 189 93 191 78 179 72 211 49 209 48 181 37 149 25 120 25 89 45 72 103 84 179 75 198 76 252 64 272 81 293 103 285 121 255 121 242 118 224 167
Polygon -7500403 true true 73 210 86 251 62 249 48 208
Polygon -7500403 true true 25 114 16 195 9 204 23 213 25 200 39 123

cylinder
false
0
Circle -7500403 true true 0 0 300

dot
false
0
Circle -7500403 true true 90 90 120

face happy
false
0
Circle -7500403 true true 8 8 285
Circle -16777216 true false 60 75 60
Circle -16777216 true false 180 75 60
Polygon -16777216 true false 150 255 90 239 62 213 47 191 67 179 90 203 109 218 150 225 192 218 210 203 227 181 251 194 236 217 212 240

face neutral
false
0
Circle -7500403 true true 8 7 285
Circle -16777216 true false 60 75 60
Circle -16777216 true false 180 75 60
Rectangle -16777216 true false 60 195 240 225

face sad
false
0
Circle -7500403 true true 8 8 285
Circle -16777216 true false 60 75 60
Circle -16777216 true false 180 75 60
Polygon -16777216 true false 150 168 90 184 62 210 47 232 67 244 90 220 109 205 150 198 192 205 210 220 227 242 251 229 236 206 212 183

fish
false
0
Polygon -1 true false 44 131 21 87 15 86 0 120 15 150 0 180 13 214 20 212 45 166
Polygon -1 true false 135 195 119 235 95 218 76 210 46 204 60 165
Polygon -1 true false 75 45 83 77 71 103 86 114 166 78 135 60
Polygon -7500403 true true 30 136 151 77 226 81 280 119 292 146 292 160 287 170 270 195 195 210 151 212 30 166
Circle -16777216 true false 215 106 30

flag
false
0
Rectangle -7500403 true true 60 15 75 300
Polygon -7500403 true true 90 150 270 90 90 30
Line -7500403 true 75 135 90 135
Line -7500403 true 75 45 90 45

flower
false
0
Polygon -10899396 true false 135 120 165 165 180 210 180 240 150 300 165 300 195 240 195 195 165 135
Circle -7500403 true true 85 132 38
Circle -7500403 true true 130 147 38
Circle -7500403 true true 192 85 38
Circle -7500403 true true 85 40 38
Circle -7500403 true true 177 40 38
Circle -7500403 true true 177 132 38
Circle -7500403 true true 70 85 38
Circle -7500403 true true 130 25 38
Circle -7500403 true true 96 51 108
Circle -16777216 true false 113 68 74
Polygon -10899396 true false 189 233 219 188 249 173 279 188 234 218
Polygon -10899396 true false 180 255 150 210 105 210 75 240 135 240

house
false
0
Rectangle -7500403 true true 45 120 255 285
Rectangle -16777216 true false 120 210 180 285
Polygon -7500403 true true 15 120 150 15 285 120
Line -16777216 false 30 120 270 120

leaf
false
0
Polygon -7500403 true true 150 210 135 195 120 210 60 210 30 195 60 180 60 165 15 135 30 120 15 105 40 104 45 90 60 90 90 105 105 120 120 120 105 60 120 60 135 30 150 15 165 30 180 60 195 60 180 120 195 120 210 105 240 90 255 90 263 104 285 105 270 120 285 135 240 165 240 180 270 195 240 210 180 210 165 195
Polygon -7500403 true true 135 195 135 240 120 255 105 255 105 285 135 285 165 240 165 195

line
true
0
Line -7500403 true 150 0 150 300

line half
true
0
Line -7500403 true 150 0 150 150

pentagon
false
0
Polygon -7500403 true true 150 15 15 120 60 285 240 285 285 120

person
false
0
Circle -7500403 true true 110 5 80
Polygon -7500403 true true 105 90 120 195 90 285 105 300 135 300 150 225 165 300 195 300 210 285 180 195 195 90
Rectangle -7500403 true true 127 79 172 94
Polygon -7500403 true true 195 90 240 150 225 180 165 105
Polygon -7500403 true true 105 90 60 150 75 180 135 105

plant
false
0
Rectangle -7500403 true true 135 90 165 300
Polygon -7500403 true true 135 255 90 210 45 195 75 255 135 285
Polygon -7500403 true true 165 255 210 210 255 195 225 255 165 285
Polygon -7500403 true true 135 180 90 135 45 120 75 180 135 210
Polygon -7500403 true true 165 180 165 210 225 180 255 120 210 135
Polygon -7500403 true true 135 105 90 60 45 45 75 105 135 135
Polygon -7500403 true true 165 105 165 135 225 105 255 45 210 60
Polygon -7500403 true true 135 90 120 45 150 15 180 45 165 90

sheep
false
15
Circle -1 true true 203 65 88
Circle -1 true true 70 65 162
Circle -1 true true 150 105 120
Polygon -7500403 true false 218 120 240 165 255 165 278 120
Circle -7500403 true false 214 72 67
Rectangle -1 true true 164 223 179 298
Polygon -1 true true 45 285 30 285 30 240 15 195 45 210
Circle -1 true true 3 83 150
Rectangle -1 true true 65 221 80 296
Polygon -1 true true 195 285 210 285 210 240 240 210 195 210
Polygon -7500403 true false 276 85 285 105 302 99 294 83
Polygon -7500403 true false 219 85 210 105 193 99 201 83

square
false
0
Rectangle -7500403 true true 30 30 270 270

square 2
false
0
Rectangle -7500403 true true 30 30 270 270
Rectangle -16777216 true false 60 60 240 240

star
false
0
Polygon -7500403 true true 151 1 185 108 298 108 207 175 242 282 151 216 59 282 94 175 3 108 116 108

target
false
0
Circle -7500403 true true 0 0 300
Circle -16777216 true false 30 30 240
Circle -7500403 true true 60 60 180
Circle -16777216 true false 90 90 120
Circle -7500403 true true 120 120 60

tree
false
0
Circle -7500403 true true 118 3 94
Rectangle -6459832 true false 120 195 180 300
Circle -7500403 true true 65 21 108
Circle -7500403 true true 116 41 127
Circle -7500403 true true 45 90 120
Circle -7500403 true true 104 74 152

triangle
false
0
Polygon -7500403 true true 150 30 15 255 285 255

triangle 2
false
0
Polygon -7500403 true true 150 30 15 255 285 255
Polygon -16777216 true false 151 99 225 223 75 224

truck
false
0
Rectangle -7500403 true true 4 45 195 187
Polygon -7500403 true true 296 193 296 150 259 134 244 104 208 104 207 194
Rectangle -1 true false 195 60 195 105
Polygon -16777216 true false 238 112 252 141 219 141 218 112
Circle -16777216 true false 234 174 42
Rectangle -7500403 true true 181 185 214 194
Circle -16777216 true false 144 174 42
Circle -16777216 true false 24 174 42
Circle -7500403 false true 24 174 42
Circle -7500403 false true 144 174 42
Circle -7500403 false true 234 174 42

turtle
true
0
Polygon -10899396 true false 215 204 240 233 246 254 228 266 215 252 193 210
Polygon -10899396 true false 195 90 225 75 245 75 260 89 269 108 261 124 240 105 225 105 210 105
Polygon -10899396 true false 105 90 75 75 55 75 40 89 31 108 39 124 60 105 75 105 90 105
Polygon -10899396 true false 132 85 134 64 107 51 108 17 150 2 192 18 192 52 169 65 172 87
Polygon -10899396 true false 85 204 60 233 54 254 72 266 85 252 107 210
Polygon -7500403 true true 119 75 179 75 209 101 224 135 220 225 175 261 128 261 81 224 74 135 88 99

wheel
false
0
Circle -7500403 true true 3 3 294
Circle -16777216 true false 30 30 240
Line -7500403 true 150 285 150 15
Line -7500403 true 15 150 285 150
Circle -7500403 true true 120 120 60
Line -7500403 true 216 40 79 269
Line -7500403 true 40 84 269 221
Line -7500403 true 40 216 269 79
Line -7500403 true 84 40 221 269

wolf
false
0
Polygon -16777216 true false 253 133 245 131 245 133
Polygon -7500403 true true 2 194 13 197 30 191 38 193 38 205 20 226 20 257 27 265 38 266 40 260 31 253 31 230 60 206 68 198 75 209 66 228 65 243 82 261 84 268 100 267 103 261 77 239 79 231 100 207 98 196 119 201 143 202 160 195 166 210 172 213 173 238 167 251 160 248 154 265 169 264 178 247 186 240 198 260 200 271 217 271 219 262 207 258 195 230 192 198 210 184 227 164 242 144 259 145 284 151 277 141 293 140 299 134 297 127 273 119 270 105
Polygon -7500403 true true -1 195 14 180 36 166 40 153 53 140 82 131 134 133 159 126 188 115 227 108 236 102 238 98 268 86 269 92 281 87 269 103 269 113

x
false
0
Polygon -7500403 true true 270 75 225 30 30 225 75 270
Polygon -7500403 true true 30 75 75 30 270 225 225 270
@#$#@#$#@
NetLogo 6.3.0
@#$#@#$#@
@#$#@#$#@
@#$#@#$#@
<experiments>
  <experiment name="Experiment_1" repetitions="1" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go-once</go>
    <metric>proportion-group-0</metric>
    <metric>proportion-group-1</metric>
    <metric>proportion-group-2</metric>
    <metric>proportion-group-3</metric>
    <metric>proportion-group-4</metric>
    <steppedValueSet variable="social-influenceability" first="0" step="10" last="100"/>
    <steppedValueSet variable="mutation-rate" first="0" step="5" last="30"/>
    <enumeratedValueSet variable="initial-population">
      <value value="500"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="initial-groups?">
      <value value="false"/>
    </enumeratedValueSet>
    <steppedValueSet variable="refractory-rate" first="0" step="10" last="90"/>
    <enumeratedValueSet variable="number-of-periods">
      <value value="260"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="evolution?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="death-probability">
      <value value="2"/>
    </enumeratedValueSet>
  </experiment>
</experiments>
@#$#@#$#@
@#$#@#$#@
default
0.0
-0.2 0 0.0 1.0
0.0 1 1.0 0.0
0.2 0 0.0 1.0
link direction
true
0
Line -7500403 true 150 150 90 180
Line -7500403 true 150 150 210 180
@#$#@#$#@
0
@#$#@#$#@
