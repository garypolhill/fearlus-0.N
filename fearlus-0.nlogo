; <one line to give the program's name and a brief idea of what it does.>
; Copyright (C) <year>  <name of author>
;
; This program is free software: you can redistribute it and/or modify
; it under the terms of the GNU General Public License as published by
; the Free Software Foundation, either version 3 of the License, or
; (at your option) any later version.
;
; This program is distributed in the hope that it will be useful,
; but WITHOUT ANY WARRANTY; without even the implied warranty of
; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
; GNU General Public License for more details.
;
; You should have received a copy of the GNU General Public License
; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;2345678901234567890123456789012345678901234567890123456789012345678901234567890
;        1         2         3         4         5         6         7         8

extensions [ table csv bitstring ]

globals [
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ; Globals needed for the template
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  error?
  warnings
  notes
  n-fails
  n-tests

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ; Model-specific globals here
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  climate
  economy
  the-land-allocator
  climate-t
  economy-t
]

breed [ land-managers land-manager ]
land-managers-own [
  nbr-weight
  p-imitate
  p-nbr-mutate
  aspiration
  wealth
  memory-size

  age
  alive?
  some-parcels-lost?
  all-parcels-lost?
  parcels-gained

  sub-pop
  parcels-list

  climate-memory
  economy-memory

  initial-strategy
  satisfice-strategy
  imitative-strategy
  experiment-strategy
]

breed [ strategies strategy ]
strategies-own [
  name
  is-imitative?
  is-historical?
  algorithm
  data
]

breed [ land-uses land-use ]
land-uses-own [
  match
  wild-card
]

breed [ land-allocators land-allocator ]
land-allocators-own [
  credit
  parcels-for-sale
]

breed [ sub-populations sub-population ]
sub-populations-own [
  name
  p-subpop
  n-removals
  nbr-weight-dist
  p-imitate-dist
  aspiration-dist
  memory-size-dist
  initial-strategy-str
  satisfice-strategy-str
  imitative-strategy-str
  experiment-strategy-str
]

patches-own [
  biophysical-properties
  yield

  owner
  next-owner
  use
  next-use
  new-manager?

  prev-yields
  prev-uses
  neighbours

  n-use-change
  n-mgr-change
]

to unit-test
  clear-all

  set n-fails 0
  set n-tests 0

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ; Call unit testing procedures here
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ; End of unit test: report results
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  output-print (word "Unit tests complete: " n-fails " failures of " n-tests " tests")
  if n-tests > 0 [
    output-print (word "Pass rate: " (precision ((n-tests - n-fails) * 100 / n-tests) 1) "%")
  ]
end


; {observer} setup
;
; Set up the model

to setup
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ; Template setup code
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  print "FEARLUS-0  Copyright (C) 2025  The James Hutton Institute"
  print "This program comes with ABSOLUTELY NO WARRANTY."
  print "This is free software, and you are welcome to redistribute it"
  print "under certain conditions. For more information on this and"
  print "the (lack of) warranty, see the LICENCE section in the Info tab."
  clear-all

  ; Initialize error? condition to false and create tables for warnings and notes

  set error? false
  set warnings table:make
  set notes table:make

  ; Control the seed

  if random-seed? [
    set rng-seed new-seed
  ]
  random-seed rng-seed

  carefully [
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ; Model-specific code to implement the setup
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

    set climate-t []
    if use-climate-file? and file-exists? climate-file [
      set climate-t read-bitstrings climate-file climate-bitstring-length
    ]
    set economy-t []
    if use-economy-file? and file-exists? economy-file [
      set economy-t read-bitstrings economy-file economy-bitstring-length
    ]

    initialize-patches
    connect-patches
    ifelse use-patch-file? and file-exists? patch-file [
      read-patches patch-file
    ] [
      clump-patches
      if use-patch-file? [
        save-patches patch-file
      ]
    ]
    initialize-land-uses
    if use-lu-file? [
      ifelse file-exists? land-use-file [
        read-land-uses land-use-file
      ] [
        save-land-uses land-use-file
      ]
    ]
    ifelse length climate-t > 0 [
      set climate first climate-t
      set climate-t but-first climate-t
    ] [
      set climate bitstring:random climate-bitstring-length 0.5
      determine-climate
    ]
    ifelse length economy-t > 0 [
      set economy first economy-t
      set economy-t but-first economy-t
    ] [
      set economy bitstring:random economy-bitstring-length 0.5
      determine-economy
    ]
    initialize-sub-populations
    initialize-land-allocator
    ask land-managers [
      allocate-initial-land-uses
    ]
    ask patches [
      set use next-use
      calculate-yield
    ]

    visualization

    if use-go-seed? [
      random-seed go-seed
    ]
  ] [
    reset-ticks
    ; Set the error? condition
    output-error error-message
  ]

  reset-ticks
end

to visualization
  ask patches [
    (ifelse visualize = "Land Uses" [
      ifelse is-turtle? use [
        set pcolor [color] of use
      ] [
        set pcolor black
      ]
    ] visualize = "Land Managers" [
      ifelse is-turtle? owner [
        set pcolor [color] of owner
      ] [
        set pcolor black
      ]
    ] visualize = "Sub-populations" [
      ifelse is-turtle? owner [
        set pcolor [[color] of sub-pop] of owner
      ] [
        set pcolor black
      ]
    ] visualize = "Biophysical Properties" [
      ifelse biophysical-properties-bit-to-visualize > 0 and biophysical-properties-bit-to-visualize <= patch-bitstring-length [
        ifelse bitstring:get? biophysical-properties (biophysical-properties-bit-to-visualize - 1) [
          set pcolor brown
        ] [
          set pcolor blue
        ]
      ] [
        set pcolor black
      ]
    ] [
      set pcolor black
    ])
  ]
end

; {observer} go
;
; Perform one timestep of the model

to go
  ; Check the error? condition

  if error? [
    print-progress (word "error tick " ticks)
    tick
    stop
  ]

  carefully [
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ; Model-specific code to implement time step here
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

    new-year
    ask sub-populations [
      reset-removals
    ]
    ask patches [
      update-land-manager
    ]
    ask land-managers [
      allocate-land-uses
    ]
    ask patches [
      update-land-use
    ]
    determine-climate
    determine-economy
    ask patches [
      calculate-yield
    ]
    ask land-managers [
      harvest
    ]
    ask land-managers [
      sell-land
    ]
    ask the-land-allocator [
      transfer-land
    ]
    ask land-managers [
      increase-age
    ]
    ask land-allocators [
      retire-managers
    ]
    ask land-managers [
      update-strategy
    ]

    visualization
  ] [
    tick
    ; Set the error? condition
    output-error error-message
  ]
  tick
end

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Initialization Schedule
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; global

to initialize-patches
  ask patches [
    set biophysical-properties bitstring:random patch-bitstring-length 0.5
    set yield 0
    set new-manager? false
    set prev-uses []
    set prev-yields []
    set n-use-change 0
    set n-mgr-change 0
  ]
end

; global

to connect-patches
  ask patches [
    let nbrs (patch-set nobody)
    (ifelse neighbourhood = "Moore" [
      ask neighbors [
        set nbrs (patch-set self nbrs)
      ]
    ] neighbourhood = "von Neumann" [
      ask neighbors4 [
        set nbrs (patch-set self nbrs)
      ]
    ] neighbourhood = "Hexagonal" [
      let x pxcor
      let y pycor
      ask neighbors4 [
        set nbrs (patch-set self nbrs)
      ]
      if patch-at -1 1 != nobody [
        set nbrs (patch-set (patch-at -1 1) nbrs)
      ]
      if patch-at 1 -1 != nobody [
        set nbrs (patch-set (patch-at 1 -1) nbrs)
      ]
    ] [
      output-error (word "Unrecognized neighbourhood \"" neighbourhood "\"")
    ])
    set neighbours nbrs
  ]
end

; global

to read-patches [ file-name ]
  let patch-list csv:from-file file-name
  let header first patch-list
  if not member? "x" header or not member? "y" header or not member? "biophysical-properties" header [
    output-error (word "Header of patch file \"" file-name "\" lacks required column names")
  ]
  let xpos position "x" header
  let ypos position "y" header
  let bpos position "biophysical-properties" header
  foreach but-first patch-list [ patch-data ->
    let x item xpos patch-data
    let y item ypos patch-data
    let bp item bpos patch-data
    if first bp != "b" [
      output-error (word "Bitstring \"" bp "\" in patch file \"" file-name "\" doesn't start with a \"b\"")
    ]
    set bp but-first bp
    if length bp != patch-bitstring-length [
      output-error (word "Bitstring \"" bp "\" in patch file \"" file-name "\" is the wrong size for current parameter settings")
    ]
    ifelse x >= min-pxcor and x <= max-pxcor and y >= min-pycor and y <= max-pycor [
      ask patch x y [
        set biophysical-properties bitstring:from-string bp
      ]
    ] [
      output-warning (word "Patch " x " " y " in patch file \"" file-name "\" is out of this world's bounds")
    ]
  ]
end

; global

to clump-patches
  foreach range patch-bitstring-length [ i ->
    if n-clump-swaps > 0 [
      let zero []
      let ones []
      ask patches [
        ifelse bitstring:get? biophysical-properties i [
          set ones lput self ones
        ] [
          set zero lput self zero
        ]
      ]
      let n-swaps 0
      while [ length zero > 0 and n-swaps < n-clump-swaps ] [
        let a first zero
        let edge-a [edge-match-count i] of a
        let corner-a [corner-match-count i] of a

        ; N.B. allow for non-toroidal worlds
        let edge-nbr-a [count neighbors4] of a
        let corner-nbr-a [(count neighbors) - (count neighbors4)] of a

        set zero but-first zero

        let success? false
        let bb nobody
        foreach ones [ b ->
          if not success? [
            let edge-b [edge-match-count i] of b
            let edge-nbr-b [count neighbors4] of b

            ifelse 2 * (edge-a + edge-b) = (edge-nbr-b + edge-nbr-a) [
              let corner-b [corner-match-count i] of b
              let corner-nbr-b [(count neighbors) - (count neighbors4)] of b

              if 2 * (corner-b + corner-a) < (corner-nbr-b + corner-nbr-a) [
                set success? true
                set bb b
              ]
            ] [
              if 2 * (edge-b + edge-a) < (edge-nbr-b + edge-nbr-a) [
                set success? true
                set bb b
              ]
            ]
          ]
        ]
        if success? [
          ask a [
            set biophysical-properties bitstring:set biophysical-properties i true
          ]
          ask bb [
            set biophysical-properties bitstring:set biophysical-properties i false
          ]
          set ones lput a (remove bb ones)
          set zero lput bb zero
          set n-swaps n-swaps + 1
        ]
      ]
      print-progress (word "Clumped bit " (i + 1))
    ]
  ]
end

to-report edge-match-count [ i ]
  let n 0
  let bit bitstring:get? biophysical-properties i
  ask neighbors4 [
    if bit = bitstring:get? biophysical-properties i [
      set n n + 1
    ]
  ]
  report n
end

to-report corner-match-count [ i ]
  let n 0
  let bit bitstring:get? biophysical-properties i
  let n4 neighbors4
  ask neighbors with [not member? self n4] [
    if bit = bitstring:get? biophysical-properties i [
      set n n + 1
    ]
  ]
  report n
end

; global

to save-patches [ file-name ]
  let patch-list [["x" "y" "biophysical-properties"]]
  foreach n-values world-width [ i -> i + min-pxcor ] [ x ->
    foreach n-values world-height [ i -> i + min-pycor ] [ y ->
      set patch-list lput (list x y (word "b" [bitstring:to-string biophysical-properties] of patch x y)) patch-list
    ]
  ]
  csv:to-file file-name patch-list
end

; global

to initialize-sub-populations
  if use-subpop-file? and file-exists? subpop-file [
    read-subpops subpop-file
  ]

  let subpop-list get-subpop-list

  if length subpop-list = 0 [
    output-error "No sub-populations have been specified"
  ]

  let p-total 0
  let subpop-names table:make
  let colours sp-colour-list

  foreach subpop-list [ sp-data ->
    let p-sp item 0 sp-data
    let sp-name item 1 sp-data
    let sp-nw-d parse-dist item 2 sp-data
    let sp-pi-d parse-dist item 3 sp-data
    let sp-as-d parse-dist item 4 sp-data
    let sp-ms-d parse-dist item 5 sp-data
    let sp-init item 6 sp-data
    let sp-sats item 7 sp-data
    let sp-copy item 8 sp-data
    let sp-expt item 9 sp-data

    if table:has-key? subpop-names sp-name [
      output-error (word "Duplicate sub-population name \"" sp-name "\"")
    ]

    if p-sp < 0 or p-total + p-sp > 1.001 [
      output-error (word "Subpopulation \"" sp-name "\" has invalid probability")
    ]
    if sp-name = item 1 (last subpop-list) [
      if p-total + p-sp < 0.999 [
        output-error (word "Subpopulation probabilities do not sum near enough to 1: " (p-total + p-sp))
      ]
      set p-sp 1 - p-total
    ]

    set p-total p-total + p-sp

    create-sub-populations 1 [
      set hidden? true
      if length colours > 0 [
        set color first colours
        set colours but-first colours
      ]
      set name sp-name
      set p-subpop p-sp
      set n-removals 0
      set nbr-weight-dist sp-nw-d
      set p-imitate-dist sp-pi-d
      set aspiration-dist sp-as-d
      set memory-size-dist sp-ms-d
      set initial-strategy-str sp-init
      set satisfice-strategy-str sp-sats
      set imitative-strategy-str sp-copy
      set experiment-strategy-str sp-expt
    ]
  ]

  if use-subpop-file? and not file-exists? subpop-file [
    save-subpops subpop-file
  ]
end

to add-subpop-str
  let subpop-list get-subpop-list

  let p-total 0
  let subpop-names table:make
  foreach subpop-list [ sp ->
    table:put subpop-names (item 1 sp) (item 0 sp)
    set p-total p-total + (item 0 sp)
  ]
  if table:has-key? subpop-names subpop-name [
    user-message (word "There is already a sub-population with name \"" subpop-name "\"\nNames must be unique.")
    stop
  ]
  if p-total + new-subpop-p > 1 [
    user-message (word "Sub-population probabilities must sum to 1\nUse Add Last Subpop")
    stop
  ]

  set subpop-list lput (list
    new-subpop-p
    subpop-name
    new-subpop-nbr-weight-dist
    new-subpop-p-imitate-dist
    new-subpop-aspiration-dist
    new-subpop-memory-size-dist
    new-subpop-initial-strategy
    new-subpop-satisfice-strategy
    new-subpop-imitative-strategy
    new-subpop-experiment-strategy
  ) subpop-list

  update-subpop-str subpop-list
end

to add-subpop-str-last
  let subpop-list get-subpop-list

  let p-total 0
  foreach subpop-list [ sp ->
    set p-total p-total + (item 0 sp)
  ]

  set new-subpop-p 1 - p-total

  add-subpop-str
end

to del-subpop-str
  let subpop-list get-subpop-list
  set subpop-list filter [ sp -> subpop-name != item 1 sp ] subpop-list
  update-subpop-str subpop-list
end

to equalize-subpop-str-p
  let subpop-list get-subpop-list

  if length subpop-list > 0 [
    let eq-p-subpop 1 / (length subpop-list)
    foreach n-values (length subpop-list) [i -> i] [ i ->
      set subpop-list replace-item i subpop-list (replace-item 0 (item i subpop-list) eq-p-subpop)
    ]
  ]

  update-subpop-str subpop-list
end

to-report get-subpop-list
  if length subpop-str = 0 or first subpop-str != "[" [
    set subpop-str "[]"
  ]
  report read-from-string subpop-str
end

to update-subpop-str [ subpop-list ]
  ifelse length subpop-list = 0 [
    set subpop-str "[]"
  ] [
    set subpop-str (word "[" (reduce [ [so-far next] -> (word so-far "\n" next) ] (map [ a -> format-subpop-list a ] subpop-list)) "]")
  ]
end

to-report format-subpop-list [ subpop-list ]
  let qstr reduce [[so-far next] -> (word so-far " \"" next "\"")] subpop-list
  report (word "[" qstr "]")
end

to save-subpops [ file-name ]
  if length subpop-str = 0 or first subpop-str != "[" [
    set subpop-str "[]"
  ]
  let subpop-list read-from-string subpop-str

  set subpop-list fput subpop-file-headings subpop-list

  csv:to-file file-name subpop-list
end

to-report subpop-file-headings
  report ["P" "name" "nbr-weight-dist" "aspiration-dist" "memory-size-dist"
    "initial-strategy" "satisfice-strategy" "imitative-strategy" "experiment-strategy"]
end

to read-subpops [ file-name ]
  let subpop-read csv:from-file file-name
  let headers first subpop-read
  let htable table:make

  foreach subpop-file-headings [ colname ->
    ifelse member? colname headers [
      table:put htable colname (position colname headers)
    ] [
      output-error (word "Column name \"" colname "\" not found in subpop-file \"" file-name "\"")
    ]
  ]

  let subpop-list []

  foreach but-first subpop-read [ sp-data ->
    let subpop-data []
    foreach subpop-file-headings [ colname ->
      set subpop-data lput (item (table:get htable colname) sp-data) subpop-data
    ]
    set subpop-list lput subpop-data subpop-list
  ]

  update-subpop-str subpop-list
end

; global

to initialize-land-uses
  let colours lu-colour-list
  create-land-uses n-land-use [
    set hidden? true
    set match bitstring:random (patch-bitstring-length + climate-bitstring-length + economy-bitstring-length) 0.5
    set wild-card bitstring:random (patch-bitstring-length + climate-bitstring-length + economy-bitstring-length) p-lu-wild
    if length colours > 0 [
      set color first colours
      set colours but-first colours
    ]
  ]
end

; global

to read-land-uses [ file-name ]
  ask land-uses [
    die
  ]
  let lu-list csv:from-file file-name
  let header first lu-list
  if not member? "match" header or not member? "wild-card" header [
    output-error (word "Header of land use file \"" file-name "\" lacks required column names")
  ]
  let matchpos position "match" header
  let wildpos position "wild-card" header
  let cpos -1
  if member? "colour" header [
    set cpos position "colour" header
  ]

  foreach but-first lu-list [ lu-data ->
    let mstr item matchpos lu-data
    if first mstr != "b" [
      output-error (word "Match bitstring \"" mstr "\" in land use file \"" file-name "\" doesn't start with a \"b\"")
    ]
    set mstr but-first mstr
    if length mstr != patch-bitstring-length + climate-bitstring-length + economy-bitstring-length [
      output-error (word "Land use bitstrings in \"" file-name "\" are the wrong size for current parameter settings")
    ]
    let wstr item wildpos lu-data
    if first wstr != "b" [
      output-error (word "Wildcard bitstring \"" wstr "\" in land use file \"" file-name "\" doesn't start with a \"b\"")
    ]
    set wstr but-first wstr
    if length mstr != patch-bitstring-length + climate-bitstring-length + economy-bitstring-length [
      output-error (word "Land use bitstrings in \"" file-name "\" are the wrong size for current parameter settings")
    ]
    create-land-uses 1 [
      set hidden? true
      set match bitstring:from-string mstr
      set wild-card bitstring:from-string wstr
      if cpos >= 0 [
        set color item cpos lu-data
      ]
    ]
  ]
end

; global

to-report read-bitstrings [file-name expected-size]
  if not file-exists? file-name [
    output-error (word "File of bitstrings \"" file-name "\" not found")
  ]
  let bitstring-list []
  file-open file-name
  while [ not file-at-end? ] [
    let str file-read-line
    if first str = "b" [
      set str but-first str
    ]
    if length str != expected-size [
      output-error (word "Bitstring \"" str "\" is not of expected size " expected-size)
    ]
    set bitstring-list lput bitstring:from-string str bitstring-list
  ]
  file-close
  report bitstring-list
end

; global

to save-land-uses [ file-name ]
  let lu-list [["match" "wild-card" "colour"]]
  ask land-uses [
    set lu-list lput (list (word "b" bitstring:to-string match) (word "b" bitstring:to-string wild-card) color) lu-list
  ]
  csv:to-file file-name lu-list
end

; global

to initialize-land-allocator
  create-land-allocators 1 [
    set hidden? true
    set credit 0
    set parcels-for-sale []
    set the-land-allocator self
  ]
  ask patches [
    set owner [random-land-manager] of the-land-allocator
    set next-owner owner
    ask owner [
      set parcels-list lput myself parcels-list
    ]
  ]
end

; land-managers

to allocate-initial-land-uses
  foreach parcels-list [ parcel ->
    ask initial-strategy [
      choose-land-use parcel myself
    ]
  ]
end


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Model Schedule
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; global

to new-year
  output-print "------------------------------------------------------------------------------"
  output-print (word "New year: " ticks)
end

; sub-populations

to reset-removals
  set n-removals 0
end

; patches

to update-land-manager
  ifelse owner != next-owner [
    set n-mgr-change n-mgr-change + 1
    set new-manager? true
  ] [
    set new-manager? false
  ]
  set owner next-owner
end

; land-managers

to allocate-land-uses
  foreach parcels-list [ parcel ->

    ifelse [last-yield] of parcel < aspiration [
      ifelse random-float 1 <= p-imitate [
        ask imitative-strategy [
          choose-land-use parcel myself
        ]
      ] [
        ask experiment-strategy [
          choose-land-use parcel myself
        ]
      ]
    ] [
      ask satisfice-strategy [
        choose-land-use parcel myself
      ]
    ]

  ]
end

; patches

to update-land-use
  if next-use != use [
    set n-use-change n-use-change + 1
  ]
  set prev-uses lput use prev-uses
  set use next-use
end

; global

to determine-climate
  ifelse use-climate-file? and length climate-t > 0 [
    set climate first climate-t
    set climate-t but-first climate-t
  ] [
    set climate bitstring:jitter climate p-climate-change
    if use-climate-file? [
      file-open climate-file
      file-print (word "b" bitstring:to-string climate)
      file-close
    ]
  ]
end

; global

to determine-economy
  ifelse use-economy-file? and length economy-t > 0 [
    set economy first economy-t
    set economy-t but-first economy-t
  ] [
    set economy bitstring:jitter economy p-economy-change
    if use-economy-file? [
      file-open economy-file
      file-print (word "b" bitstring:to-string economy)
      file-close
    ]
  ]
end

; patches

to calculate-yield
  set prev-yields lput yield prev-yields
  set yield [ match-result biophysical-properties ] of use
end

; land-managers

to harvest
  foreach parcels-list [ parcel ->
    set wealth wealth + ([yield] of parcel) - break-even-threshold
  ]
  set climate-memory lput climate climate-memory
  if length climate-memory > memory-size [
    set climate-memory but-first climate-memory
  ]
  set economy-memory lput economy economy-memory
  if length economy-memory > memory-size [
    set economy-memory but-first economy-memory
  ]
end

; land-managers

to sell-land
  set some-parcels-lost? false
  while [ wealth < 0 and length parcels-list > 0 ] [
    let parcel first parcels-list
    ask the-land-allocator [
      set parcels-for-sale lput parcel parcels-for-sale
    ]
    set parcels-list but-first parcels-list
    set wealth wealth + patch-price
    set some-parcels-lost? true
  ]
  set all-parcels-lost? (length parcels-list = 0)
end

; land-allocator

to transfer-land
  ask land-managers [
    set parcels-gained 0
  ]
  while [ length parcels-for-sale > 0 ] [
    let parcel first parcels-for-sale
    set parcels-for-sale but-first parcels-for-sale

    let mgrs ([manager-neighbours] of parcel) with [eligible-for-parcels?]

    let n-opts 1 + count mgrs

    let new-mgr ifelse-value random n-opts = 0 [ random-land-manager ] [ one-of mgrs ]

    ask new-mgr [
      set parcels-list lput parcel parcels-list
      if member? self mgrs [
        set parcels-gained parcels-gained + 1
      ]
    ]
    ask parcel [
      set next-owner new-mgr
    ]
  ]
end

; land-managers

to increase-age
  set age age + 1
end

; land-allocator

to retire-managers
  let credit-change 0
  ask land-managers with [ all-parcels-lost? ] [
    set credit-change credit-change - wealth
    ask sub-pop [
      set n-removals n-removals + 1
    ]
    die
  ]
  set credit credit + credit-change
end

; land-managers

to update-strategy
  set nbr-weight nbr-weight - (parcels-gained * neighbour-weight-adjust)
  if nbr-weight < 0 [
    set nbr-weight 0
  ]
end

; strategy

to choose-land-use [ parcel mgr ]
  let lu (runresult algorithm parcel mgr)
  ask parcel [
    set next-use lu
  ]
end

; land-parcels

to-report last-yield
  if not is-list? prev-yields or length prev-yields = 0 [
    output-error "No last yield"
  ]
  report last prev-yields
end

; land-uses

to-report match-result [ biophys ]
  report parcel-climate-economy-match-result biophys climate economy
end

to-report parcel-match-result [ biophys ]
  let parcel-bits bitstring:sub match 0 patch-bitstring-length
  report bitstring:count1 (wild-card bitstring:or (parcel-bits bitstring:parity biophys))
end

to-report parcel-climate-economy-match-result [ biophys clim econ ]
  let bio-clim-econ bitstring:cat biophys (bitstring:cat clim econ)
  report bitstring:count1 (wild-card bitstring:or (match bitstring:parity bio-clim-econ))
end

; land-managers

to-report eligible-for-parcels?
  ifelse wealth >= patch-price [
    ifelse all-parcels-lost? [
      output-error "A manager with no parcels is eligible for a new parcel!"
      report false
    ] [
      report true
    ]
  ] [
    report false
  ]
end

; land-parcels

to-report manager-neighbours
  let nbrs (turtle-set nobody)
  ask neighbours [
    set nbrs (turtle-set owner nbrs)
  ]
  report nbrs
end

; land-managers

to-report social-neighbours
  let nbrs (turtle-set nobody)
  let me self
  foreach parcels-list [ parcel ->
    ask parcel [
      ask neighbours with [owner != me] [
        set nbrs (turtle-set owner nbrs)
      ]
    ]
  ]
  report nbrs
end

; land-managers

to-report parcel-neighbours
  let nbrs (turtle-set nobody)
  let plist parcels-list
  foreach parcels-list [ parcel ->
    ask parcel [
      ask neighbours with [not member? self plist] [
        set nbrs (turtle-set self nbrs)
      ]
    ]
  ]
  report nbrs
end

; land-allocator

to-report random-land-manager
  let sp weighted-choice sub-populations [ -> p-subpop ]
  report [spawn-land-manager] of sp
end

; sub-population

to-report spawn-land-manager
  let lm nobody
  hatch-land-managers 1 [
    set sub-pop myself
    set color (10 * random 12) + (2 + random 7)

    set nbr-weight sample [nbr-weight-dist] of sub-pop
    set p-imitate sample [p-imitate-dist] of sub-pop
    set p-nbr-mutate sample (list "uniform" neighbour-noise-min neighbour-noise-max)
    set aspiration sample [aspiration-dist] of sub-pop
    set wealth 0
    set memory-size sample [memory-size-dist] of sub-pop

    set age 0
    set alive? true
    set some-parcels-lost? false
    set all-parcels-lost? false
    set parcels-gained 0
    set parcels-list []

    set climate-memory []
    set economy-memory []

    set initial-strategy [parse-strategy initial-strategy-str false false] of sub-pop
    set satisfice-strategy [parse-strategy satisfice-strategy-str 0 false] of sub-pop
    set imitative-strategy [parse-strategy imitative-strategy-str 0 true] of sub-pop
    set experiment-strategy [parse-strategy experiment-strategy-str 0 false] of sub-pop

    set lm self
  ]
  report lm
end

; land-manager
; CautiousImitativeOptimumStrategy not implemented
; Cautious uses expected variance as the score rather than expected mean -- not in any paper

to-report parse-strategy [ strat-str is-hist? is-imit? ]
  let s nobody
  (ifelse strat-str = "Null" or strat-str = "none" or strat-str = "NoStrategy" or strat-str = "{}" [
    set s no-strategy
  ] strat-str = "Habit" or strat-str = "habit" or strat-str = "HabitStrategy" or strat-str = "H" [
    set s habit-strategy
  ] strat-str = "Random" or strat-str = "random" or strat-str = "RandomStrategy" or strat-str = "R" or strat-str = "RS" [
    set s random-strategy
  ] strat-str = "Specialist" or strat-str = "eccentric-specialist" or strat-str = "EccentricSpecialistStrategy" or strat-str = "X" [
    set s eccentric-specialist-strategy
  ] strat-str = "Fickle" or strat-str = "fickle" or strat-str = "FickleStrategy" or strat-str = "F" [
    set s fickle-strategy
  ] strat-str = "BestMatch" or strat-str = "match-best" or strat-str = "StochasticMatchWeightedOptimumStrategy" or strat-str = "OS" [
    set s match-weighted-optimum-strategy
  ] strat-str = "StableMatch" or strat-str = "match-ordered" or strat-str = "MatchWeightedOptimumStrategy" or strat-str = "OD" [
    set s match-weighted-deterministic-strategy
  ] strat-str = "WeightedMatch" or strat-str = "match-weighted" or strat-str = "MatchWeightedRandomStrategy" or strat-str = "O" [
    set s match-weighted-random-strategy
  ] strat-str = "StableLast" or strat-str = "last-order" or strat-str = "LastYearsOptimumStrategy" or strat-str = "LD" [
    set s last-years-optimum-strategy
  ] strat-str = "LastBest" or strat-str = "last-best" or strat-str = "StochasticLastYearsOptimumStrategy" or strat-str = "LS" [
    set s stochastic-last-years-optimum-strategy
  ] strat-str = "WeightedLast" or strat-str = "last-weighted" [
    set s weighted-last-years-optimum-strategy
  ] strat-str = "StableLastN" or strat-str = "last-n-order" or strat-str = "LastNYearsOptimumStrategy" or strat-str = "LnD" [
    set s last-n-years-optimum-strategy
  ] strat-str = "LastNBest" or strat-str = "last-n-best" or strat-str = "StochasticLastNYearsOptimumStrategy" or strat-str = "LnS" [
    set s stochastic-last-n-years-strategy
  ] strat-str = "WeightedLastN" or strat-str = "last-n-weighted" [
    set s weighted-last-n-years-strategy
  ] strat-str = "MajorityBest" or strat-str = "majority-best"
    or strat-str = "MajorityCopyingStrategy" or strat-str = "SimpleRandomOptimumCopyingStrategy" or strat-str = "SSI" [
    set s majority-copying-strategy
  ] strat-str = "WeightedMajority" or strat-str = "majority-weighted" or strat-str = "SimpleCopyingStrategy" or strat-str = "SI" [
    set s simple-copying-strategy
  ] strat-str = "PhysicalMajorityBest" or strat-str = "physical-majority-best" [
    set s physical-majority-copying-strategy
  ] strat-str = "PhysicalWeightedMajority" or strat-str = "physical-majority-weighted"
    or strat-str = "SimplePhysicalCopyingStrategy" or strat-str = "Sp" [
    set s physical-simple-copying-strategy
  ] strat-str = "CopyLastBest" or strat-str = "last-best-copy" or strat-str = "NeighbouringOptimumStrategy" or strat-str = "N" [
    set s last-years-best-copying-strategy
  ] strat-str = "CopyWeightedLast" or strat-str = "last-best-weighted" [
    set s last-years-weighted-copying-strategy
  ] strat-str = "CopyBestMatch" or strat-str = "match-best-copy" [
    set s match-best-copying-strategy
  ] strat-str = "CopyWeightedMatch" or strat-str = "match-weighted-copy" or strat-str = "ParcelWeightedCopyingStrategy" or strat-str = "P" [
    set s match-weighted-copying-strategy
  ] strat-str = "CopyOther" or strat-str = "other-copying" or strat-str = "RandomCopyingStrategy" or strat-str = "RC" [
    set s other-copying-strategy
  ] strat-str = "SmartCopyBest" or strat-str = "smart-best-copy" or strat-str = "StochasticImitativeOptimumStrategy"
    or strat-str = "IS" or strat-str = "II" [
    set s intelligent-best-copying-strategy
  ] strat-str = "StableSmartCopy" or strat-str = "smart-ordered-copy" or strat-str = "ImitativeOptimumStrategy" or strat-str = "ID" [
    set s intelligent-order-copying-strategy
  ] strat-str = "WeightedSmartCopy" or strat-str = "smart-weighted-copy" [
    set s intelligent-weighted-copying-strategy
  ] strat-str = "WeightedYieldCopy" or strat-str = "yield-weighted-copy" or strat-str = "YieldWeightedCopyingStrategy" or strat-str = "Y" [
    set s yield-weighted-copying-strategy
  ] strat-str = "MatchWeightedYieldCopy" or strat-str = "match-yield-weighted-copy"
    or strat-str = "ParcelCorrectedYieldWeightedCopyingStrategy" or strat-str = "YP" [
    set s parcel-corrected-yield-weighted-copying-strategy
  ] strat-str = "MeanWeightedYieldCopy" or strat-str = "yield-mean-weighted-copy" [
    set s yield-average-weighted-copying-strategy
  ] strat-str = "WeightedYieldTCopy" or strat-str = "yield-t-weighted-copy" or strat-str = "YieldWeightedTemporalCopyingStrategy"
    or strat-str = "YI" [
    set s yield-weighted-temporal-copying-strategy
  ] strat-str = "MeanWeightedYieldTCopy" or strat-str = "yield-t-mean-weighted-copy"
    or strat-str = "YieldAverageWeightedTemporalCopyingStrategy" or strat-str = "BI" [
    set s yield-average-weighted-temporal-copying-strategy
  ] strat-str = "MeanYieldTBestCopy" or strat-str = "yield-t-mean-best-copy"
    or strat-str = "YieldRandomOptimumTemporalCopyingStrategy" or strat-str = "SBI" [
    set s yield-random-optimum-temporal-copying-strategy
  ] strat-str = "YieldTBestCopy" or strat-str = "yield-t-best-copy"
    or strat-str = "YieldTotalRandomOptimumTemporalCopyingStrategy" or strat-str = "SYI" [
    set s yield-total-random-optimum-temporal-copying-strategy
  ] [
    output-error (word "Strategy \"" strat-str "\" not recognized")
  ])

  if is-boolean? is-hist? and is-boolean? [is-historical?] of s [
    if [is-historical?] of s != is-hist? [
      ifelse is-hist? [
        output-error (word "Non-historical strategy \"" strat-str "\" provided where historical strategy required")
      ] [
        output-error (word "Historical strategy \"" strat-str "\" provided where non-historical strategy required")
      ]
    ]
  ]

  if is-boolean? is-imit? and is-boolean? [is-imitative?] of s [
    if [is-imitative?] of s != is-imit? [
      ifelse is-imit? [
        output-error (word "Non-imitative strategy \"" strat-str "\" provided where imitative strategy required")
      ] [
        output-error (word "Imitative strategy \"" strat-str "\" provided where non-imitative strategy required")
      ]
    ]
  ]

  report s
end


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Strategies
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

to-report get-strategy [ a-name is-hist? is-imit? alg ]
  if any? strategies with [name = a-name] [
    report one-of strategies with [name = a-name]
  ]
  let s nobody
  hatch-strategies 1 [
    set name a-name
    set is-imitative? is-imit?
    set is-historical? is-hist?
    set algorithm alg
    set data []
    set s self
  ]
  report s
end

to-report no-strategy
  report get-strategy "none" 0 0 [ [lp lm] -> no-algorithm lp lm ]
end

to-report no-algorithm [ lp lm ]
  output-error "Null strategy asked for a land use!"
end

to-report habit-strategy
  report get-strategy "habit" true false [ [lp lm] -> habit-algorithm lp lm ]
end

to-report habit-algorithm [ lp lm ]
  report [ use ] of lp
end

to-report random-strategy
  report get-strategy "random" false false [ [lp lm] -> random-algorithm lp lm ]
end

to-report random-algorithm [ lp lm ]
  report one-of land-uses
end

to-report eccentric-specialist-strategy
  report get-strategy "eccentric-specialist" false false [ [lp lm] -> eccentric-specialist-algorithm lp lm ]
end

to-report eccentric-specialist-algorithm [ lp lm ]
  if length data = 0 [
    set data lput (one-of land-uses) data
  ]
  report last data
end

to-report fickle-strategy
  report get-strategy "fickle" false false [ [lp lm] -> fickle-algorithm lp lm ]
end

to-report fickle-algorithm [ lp lm ]
  if length data = 0 or ticks != first data [
    set data (list ticks (one-of land-uses))
  ]
  report last data
end

to-report match-weighted-optimum-strategy
  report get-strategy "match-best" false false [ [lp lm] -> match-weighted-algorithm lp lm true false true ]
end

to-report match-weighted-deterministic-strategy
  report get-strategy "match-ordered" false false [ [lp lm] -> match-weighted-algorithm lp lm true true true ]
end

to-report match-weighted-random-strategy
  report get-strategy "match-weighted" false false [ [lp lm] -> match-weighted-algorithm lp lm false false false ]
end

to-report match-weighted-algorithm [ lp lm best? order? wild1s? ]
  let lu-list []
  let scores []
  ask land-uses [
    set lu-list lput self lu-list
    let wild1s 0
    if wild1s? [
      set wild1s bitstring:count1 (bitstring:sub wild-card patch-bitstring-length
        (patch-bitstring-length + climate-bitstring-length + economy-bitstring-length))
    ]
    set scores lput (wild1s + (parcel-match-result [biophysical-properties] of lp)) scores
  ]
  report ifelse-value best? [ifelse-value order?
    [deterministic-best-choice-lists lu-list scores] [best-choice-lists lu-list scores]
  ] [weighted-choice-lists lu-list scores]
end

to-report last-years-optimum-strategy
  report get-strategy "last-order" true false [ [lp lm] -> last-years-algorithm lp lm true true ]
end

to-report stochastic-last-years-optimum-strategy
  report get-strategy "last-best" true false [ [lp lm] -> last-years-algorithm lp lm true false ]
end

to-report weighted-last-years-optimum-strategy
  report get-strategy "last-weighted" true false [ [lp lm] -> last-years-algorithm lp lm false false ]
end

to-report last-years-algorithm [ lp lm best? order? ]
  let lu-list []
  let scores []
  ask land-uses [
    set lu-list lput self lu-list
    set scores lput (match-result [biophysical-properties] of lp) scores
  ]
  report ifelse-value best? [ifelse-value order?
    [deterministic-best-choice-lists lu-list scores] [best-choice-lists lu-list scores]
  ] [weighted-choice-lists lu-list scores]
end

to-report last-n-years-optimum-strategy
  report get-strategy "last-n-order" true false [ [lp lm] -> last-n-years-algorithm lp lm true true ]
end

to-report stochastic-last-n-years-strategy
  report get-strategy "last-n-best" true false [ [lp lm] -> last-n-years-algorithm lp lm true false ]
end

to-report weighted-last-n-years-strategy
  report get-strategy "last-n-weighted" true false [ [lp lm] -> last-n-years-algorithm lp lm false false ]
end

to-report last-n-years-algorithm [ lp lm best? order? ]
  let lu-list []
  let scores []
  ask land-uses [
    set lu-list lput self lu-list
    set scores lput 0 scores
  ]
  foreach n-values length climate-memory [ i -> i ] [ ix ->
    let clim item ix climate-memory
    let econ item ix economy-memory
    foreach n-values length lu-list [ i -> i ] [ jx ->
      let score [parcel-climate-economy-match-result [biophysical-properties] of lp clim econ] of item jx lu-list
      set scores replace-item jx scores (score + item jx scores)
    ]
  ]
  report ifelse-value best? [ifelse-value order?
    [deterministic-best-choice-lists lu-list scores] [best-choice-lists lu-list scores]
  ] [weighted-choice-lists lu-list scores]
end

to-report majority-copying-strategy
  report get-strategy "majority-best" true true [ [lp lm] -> majority-copying-algorithm lp lm true]
end

to-report simple-copying-strategy
  report get-strategy "majority-weighted" true true [ [lp lm] -> majority-copying-algorithm lp lm false]
end

to-report majority-copying-algorithm [ lp lm best? ]
  let lu-table table:make
  ask land-uses [
    table:put lu-table who 0
  ]
  ask lm [
    foreach parcels-list [ parcel ->
      table:put lu-table ([[who] of use] of parcel) (1 + table:get lu-table ([[who] of use] of parcel))
    ]
    ask social-neighbours [
      foreach parcels-list [ parcel ->
        table:put lu-table ([[who] of use] of parcel) (nbr-weight + table:get lu-table ([[who] of use] of parcel))
      ]
    ]
  ]
  report land-use (ifelse-value best? [best-choice-table lu-table] [weighted-choice-table lu-table])
end

to-report physical-majority-copying-strategy
  report get-strategy "physical-majority-best" true true [ [lp lm] -> physical-majority-copying-algorithm lp lm true ]
end

to-report physical-simple-copying-strategy
  report get-strategy "physical-majority-weighted" true true [ [lp lm] -> physical-majority-copying-algorithm lp lm false ]
end

to-report physical-majority-copying-algorithm [ lp lm best? ]
  let lu-table table:make
  ask land-uses [
    table:put lu-table who 0
  ]
  ask lm [
    foreach parcels-list [ parcel ->
      table:put lu-table ([[who] of use] of parcel) (1 + table:get lu-table ([[who] of use] of parcel))
    ]
    ask parcel-neighbours [
      table:put lu-table ([who] of use) nbr-weight + table:get lu-table ([who] of use)
    ]
  ]
  report land-use (ifelse-value best? [best-choice-table lu-table] [weighted-choice-table lu-table])
end

to-report last-years-best-copying-strategy
  report get-strategy "last-best-copy" true true [ [lp lm] -> last-years-copying-algorithm lp lm true ]
end

to-report last-years-weighted-copying-strategy
  report get-strategy "last-weighted-copy" true true [ [lp lm] -> last-years-copying-algorithm lp lm false ]
end

to-report last-years-copying-algorithm [ lp lm best? ]
  let bp [biophysical-properties] of lp
  let lu-table table:make
  ask land-uses [
    table:put lu-table who 0
  ]
  ask lm [
    foreach parcels-list [ parcel ->
      let lu [use] of parcel
      let score [match-result bp] of lu
      table:put lu-table ([who] of lu) score
    ]
    ask social-neighbours [
      foreach parcels-list [ parcel ->
        let lu [use] of parcel
        let score [match-result bp] of lu
        table:put lu-table ([who] of lu) (nbr-weight * score)
      ]
    ]
  ]
  report land-use (ifelse-value best? [best-choice-table lu-table] [weighted-choice-table lu-table])
end

to-report match-best-copying-strategy
  report get-strategy "match-best-copy" true true [ [lp lm] -> match-copying-algorithm lp lm true ]
end

to-report match-weighted-copying-strategy
  report get-strategy "match-weighted-copy" true true [ [lp lm] -> match-copying-algorithm lp lm false ]
end

to-report match-copying-algorithm [ lp lm best? ]
  let bp [biophysical-properties] of lp
  let lu-table table:make
  ask land-uses [
    table:put lu-table who 0
  ]
  ask lm [
    foreach parcels-list [ parcel ->
      let lu [use] of parcel
      let score [parcel-match-result bp] of lu
      table:put lu-table ([who] of lu) (score + table:get lu-table ([who] of lu))
    ]
    ask social-neighbours [
      foreach parcels-list [ parcel ->
        let lu [use] of parcel
        let score [parcel-match-result bp] of lu
        table:put lu-table ([who] of lu) ((nbr-weight * score) + table:get lu-table ([who] of lu))
      ]
    ]
  ]
  report land-use (ifelse-value best? [best-choice-table lu-table] [weighted-choice-table lu-table])
end

to-report other-copying-strategy
  report get-strategy "other-copying" true true [ [lp lm] -> other-copying-algorithm lp lm ]
end

to-report other-copying-algorithm [ lp lm ]
  let lp-lu [use] of lp
  let lu-table table:make
  ask (turtle-set lm [social-neighbours] of lm) [
    foreach parcels-list [ parcel ->
      let lu [use] of parcel
      if lu != lp-lu [
        table:put lu-table ([who] of lu) 1
      ]
    ]
  ]
  ifelse table:length lu-table = 0 [
    report lp-lu
  ] [
    report land-use (weighted-choice-table lu-table)
  ]
end

to-report intelligent-best-copying-strategy
  report get-strategy "smart-best-copy" true true [ [lp lm] -> intelligent-copying-algorithm lp lm true false ]
end

to-report intelligent-order-copying-strategy
  report get-strategy "smart-ordered-copy" true true [ [lp lm] -> intelligent-copying-algorithm lp lm true true ]
end

to-report intelligent-weighted-copying-strategy
  report get-strategy "smart-weighted-copy" true true [ [lp lm] -> intelligent-copying-algorithm lp lm false false ]
end

to-report intelligent-copying-algorithm [lp lm best? order?]
  let lu-table table:make

  ask lm [
    foreach parcels-list [ parcel ->
      let lu [use] of parcel
      if not table:has-key? lu-table ([who] of lu) [
        table:put lu-table ([who] of lu) ([expected-yield parcel] of lp)
      ]
    ]
    ask social-neighbours [
      foreach parcels-list [ parcel ->
        let lu [use] of parcel
        if not table:has-key? lu-table ([who] of lu) [
          table:put lu-table ([who] of lu) ([expected-yield parcel] of lp) * nbr-weight
        ]
      ]
    ]
  ]
  report land-use (ifelse-value best? [ifelse-value order?
    [deterministic-best-choice-table lu-table] [best-choice-table lu-table]
  ] [weighted-choice-table lu-table])
end

to-report yield-weighted-copying-strategy
  report get-strategy "yield-weighted-copy" true true [ [lp lm] -> yield-copying-algorithm lp lm false false ]
end

to-report parcel-corrected-yield-weighted-copying-strategy
  report get-strategy "match-yield-weighted-copy" true true [ [lp lm] -> yield-copying-algorithm lp lm false true ]
end

to-report yield-average-weighted-copying-strategy
  report get-strategy "yield-mean-weighted-copy" true true [ [lp lm] -> yield-copying-algorithm lp lm true false ]
end

to-report yield-copying-algorithm [lp lm mean? correct-bp?]
  let lu-table table:make
  let lu-n-table table:make
  let bp [biophysical-properties] of lp
  ask land-uses [
    table:put lu-table who 0
    table:put lu-n-table who 0
  ]
  ask lm [
    foreach parcels-list [ parcel ->
      let lu [[who] of use] of parcel
      let score yield
      if correct-bp? [
        set score score * bp bitstring:parity [biophysical-properties] of parcel
      ]
      table:put lu-table lu (score + table:get lu-table lu)
      table:put lu-n-table lu (1 + table:get lu-n-table lu)
    ]
    ask social-neighbours [
      foreach parcels-list [ parcel ->
        let lu [[who] of use] of parcel
        let score yield * nbr-weight
        if correct-bp? [
          let other-bp [biophysical-properties] of parcel
          set score score * (bp bitstring:parity (bitstring:jitter other-bp ([p-nbr-mutate] of lm)))
        ]
        table:put lu-table lu (score + table:get lu-table lu)
        table:put lu-n-table lu (1 + table:get lu-n-table lu)
      ]
    ]
  ]
  if mean? [
    foreach table:keys lu-table [ key ->
      table:put lu-table key (table:get lu-table key / table:get lu-n-table key)
    ]
  ]
  report land-use (weighted-choice-table lu-table)
end

to-report yield-weighted-temporal-copying-strategy
  report get-strategy "yield-t-weighted-copy" true true [ [lp lm] -> temporal-yield-copying-algorithm lp lm false false ]
end

to-report yield-average-weighted-temporal-copying-strategy
  report get-strategy "yield-t-mean-weighted-copy" true true [ [lp lm] -> temporal-yield-copying-algorithm lp lm false true ]
end

to-report yield-random-optimum-temporal-copying-strategy
  report get-strategy "yield-t-mean-best-copy" true true [ [lp lm] -> temporal-yield-copying-algorithm lp lm true true ]
end

to-report yield-total-random-optimum-temporal-copying-strategy
  report get-strategy "yield-t-best-copy" true true [ [lp lm] -> temporal-yield-copying-algorithm lp lm true false ]
end

to-report temporal-yield-copying-algorithm [lp lm best? mean?]
  let lu-table table:make
  let lu-n-table table:make
  ask land-uses [
    table:put lu-table who 0
    table:put lu-n-table who 0
  ]

  ask lm [
    let mem memory-size
    foreach parcels-list [ parcel ->
      let y-list reverse [prev-yields] of parcel
      let u-list reverse [prev-uses] of parcel
      repeat mem [
        if length y-list > 0 [
          let u [who] of (first u-list)
          let y first y-list

          table:put lu-table u (y + table:get lu-table u)
          table:put lu-n-table u (1 + table:get lu-n-table u)

          set u-list but-first u-list
          set y-list but-first y-list
        ]
      ]
    ]
    ask social-neighbours [
      foreach parcels-list [ parcel ->
        let y-list reverse [prev-yields] of parcel
        let u-list reverse [prev-uses] of parcel
        repeat mem [
          if length y-list > 0 [
            let u [who] of (first u-list)
            let y first y-list

            table:put lu-table u (nbr-weight * y) + table:get lu-table u
            table:put lu-n-table u (1 + table:get lu-n-table u)

            set u-list but-first u-list
            set y-list but-first y-list
          ]
        ]
      ]
    ]
  ]
  if mean? [
    foreach table:keys lu-table [ key ->
      table:put lu-table key (table:get lu-table key / table:get lu-n-table key)
    ]
  ]
  report land-use (ifelse-value best? [best-choice-table lu-table] [weighted-choice-table lu-table])
end

; parcel

to-report expected-yield [ other-lp ]
  let other-bp [biophysical-properties] of other-lp
  if owner != [owner] of other-lp [
    set other-bp bitstring:jitter other-bp ([p-nbr-mutate] of owner)
  ]
  let anti-match bitstring:count1 (biophysical-properties bitstring:xor other-bp)
  let lubits patch-bitstring-length + climate-bitstring-length + economy-bitstring-length
  let eyield 0
  let denom combinations lubits yield
  foreach range (1 + int yield) [ i ->

    set eyield eyield + ((yield + anti-match - (2 * i)) * (combinations anti-match i)
      * (combinations (lubits - anti-match) (yield - i)) / denom)

  ]
  report eyield
end

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Presets
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

to set-env-param-imitation [environment]
  set neighbourhood "Moore"
  set patch-price 20
  set break-even-threshold 10.5
  set p-economy-change 0
  set economy-bitstring-length 0
  set neighbour-weight-adjust 0
  set neighbour-noise-min 0
  set neighbour-noise-max 0
  set n-land-use 6
  set n-clump-swaps 0
  set p-lu-wild 0
  (ifelse environment = 1 [
    set patch-bitstring-length 1
    set climate-bitstring-length 19
    set p-climate-change 0.1
  ] environment = 2 [
    set patch-bitstring-length 10
    set climate-bitstring-length 10
    set p-climate-change 0.1
  ] environment = 3 [
    set patch-bitstring-length 1
    set climate-bitstring-length 19
    set p-climate-change 0.5
  ] [
    output-error "Environment should be 1, 2 or 3"
  ])
end

to add-subpop-imitation [subpop]
  (ifelse subpop = "RS" [
    set subpop-name "RS"
    set new-subpop-p 0.5
    set new-subpop-initial-strategy "Random"
    set new-subpop-satisfice-strategy "Null"
    set new-subpop-imitative-strategy "Null"
    set new-subpop-experiment-strategy "Random"
    set new-subpop-nbr-weight-dist "1"
    set new-subpop-p-imitate-dist "0"
    set new-subpop-aspiration-dist "21"
    set new-subpop-memory-size-dist "1"
  ] subpop = "OS" [
    set subpop-name "OS"
    set new-subpop-p 0.5
    set new-subpop-initial-strategy "Random"
    set new-subpop-satisfice-strategy "Null"
    set new-subpop-imitative-strategy "Null"
    set new-subpop-experiment-strategy "BestMatch"
    set new-subpop-nbr-weight-dist "1"
    set new-subpop-p-imitate-dist "0"
    set new-subpop-aspiration-dist "21"
    set new-subpop-memory-size-dist "1"
  ] subpop = "OD" [
    set subpop-name "OD"
    set new-subpop-p 0.5
    set new-subpop-initial-strategy "Random"
    set new-subpop-satisfice-strategy "Null"
    set new-subpop-imitative-strategy "Null"
    set new-subpop-experiment-strategy "StableMatch"
    set new-subpop-nbr-weight-dist "1"
    set new-subpop-p-imitate-dist "0"
    set new-subpop-aspiration-dist "21"
    set new-subpop-memory-size-dist "1"
  ] subpop = "LS" [
    set subpop-name "LS"
    set new-subpop-p 0.5
    set new-subpop-initial-strategy "Random"
    set new-subpop-satisfice-strategy "Null"
    set new-subpop-imitative-strategy "Null"
    set new-subpop-experiment-strategy "LastBest"
    set new-subpop-nbr-weight-dist "1"
    set new-subpop-p-imitate-dist "0"
    set new-subpop-aspiration-dist "21"
    set new-subpop-memory-size-dist "1"
  ] subpop = "LD" [
    set subpop-name "LD"
    set new-subpop-p 0.5
    set new-subpop-initial-strategy "Random"
    set new-subpop-satisfice-strategy "Null"
    set new-subpop-imitative-strategy "Null"
    set new-subpop-experiment-strategy "StableLast"
    set new-subpop-nbr-weight-dist "1"
    set new-subpop-p-imitate-dist "0"
    set new-subpop-aspiration-dist "21"
    set new-subpop-memory-size-dist "1"
  ] subpop = "SI" [
    set subpop-name "SI"
    set new-subpop-p 0.5
    set new-subpop-initial-strategy "Random"
    set new-subpop-satisfice-strategy "Null"
    set new-subpop-imitative-strategy "WeightedMajority"
    set new-subpop-experiment-strategy "Null"
    set new-subpop-nbr-weight-dist "1"
    set new-subpop-p-imitate-dist "1"
    set new-subpop-aspiration-dist "21"
    set new-subpop-memory-size-dist "1"
  ] subpop = "YI" [
    set subpop-name "YI"
    set new-subpop-p 0.5
    set new-subpop-initial-strategy "Random"
    set new-subpop-satisfice-strategy "Null"
    set new-subpop-imitative-strategy "WeightedYieldCopy"
    set new-subpop-experiment-strategy "Null"
    set new-subpop-nbr-weight-dist "1"
    set new-subpop-p-imitate-dist "1"
    set new-subpop-aspiration-dist "21"
    set new-subpop-memory-size-dist "1"
  ] subpop = "HYI" [
    set subpop-name "HYI"
    set new-subpop-p 0.5
    set new-subpop-initial-strategy "Random"
    set new-subpop-satisfice-strategy "Habit"
    set new-subpop-imitative-strategy "WeightedYieldCopy"
    set new-subpop-experiment-strategy "Null"
    set new-subpop-nbr-weight-dist "1"
    set new-subpop-p-imitate-dist "1"
    set new-subpop-aspiration-dist "11"
    set new-subpop-memory-size-dist "1"
  ] subpop = "HRYI" [
    set subpop-name "HRYI"
    set new-subpop-p 0.5
    set new-subpop-initial-strategy "Random"
    set new-subpop-satisfice-strategy "Habit"
    set new-subpop-imitative-strategy "WeightedYieldCopy"
    set new-subpop-experiment-strategy "Random"
    set new-subpop-nbr-weight-dist "1"
    set new-subpop-p-imitate-dist "0.9375"
    set new-subpop-aspiration-dist "11"
    set new-subpop-memory-size-dist "1"
  ] subpop = "II" [
    set subpop-name "II"
    set new-subpop-p 0.5
    set new-subpop-initial-strategy "Random"
    set new-subpop-satisfice-strategy "Null"
    set new-subpop-imitative-strategy "SmartCopyBest"
    set new-subpop-experiment-strategy "Null"
    set new-subpop-nbr-weight-dist "1"
    set new-subpop-p-imitate-dist "1"
    set new-subpop-aspiration-dist "21"
    set new-subpop-memory-size-dist "1"
  ] [
    output-error (word "Sub-population \"" subpop "\" not in Polhill et al. (2001)")
  ])
  add-subpop-str
end

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Utilities
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

to-report combinations [ n r ]
  if n < 0 or r < 0 or r > n [
    report 0
  ]
  let comb 1
  foreach n-values r [ i -> i ] [ i ->
    set comb comb * (n - i) / (i + 1)
  ]
  report comb
end

to-report parse-dist [ dist-str ]
  if member? (first dist-str) map [ i -> (word i) ] n-values 10 [ i -> i ] [
    report (list dist-str)
  ]
  if last dist-str != "]" or not member? "[" dist-str [
    output-error (word "Invalid distribution string \"" dist-str "\" -- no parameters in square brackets")
  ]
  set dist-str but-last dist-str
  let name-param (csv:from-row dist-str "[")
  if length name-param != 2 [
    output-error (word "Invalid distribution string \"" dist-str "\" -- too many open square brackets")
  ]
  let param-list (csv:from-row (item 1 name-param) " ")
  report fput (item 0 name-param) param-list
end

; utility

to-report sample [ dist ]
  let dist-name first dist

  if member? first dist-name map [ i -> (word i) ] n-values 10 [ i -> i ] [
    report read-from-string dist-name
  ]

  let dist-parm but-first dist
  (ifelse dist-name = "uniform" or dist-name = "U" [
    if length dist-parm < 2 or item 0 dist-parm > item 1 dist-parm [
      output-error (word "Invalid parameters for uniform distribution: " dist-parm)
    ]
    ifelse item 0 dist-parm = item 1 dist-parm [
      report item 0 dist-parm
    ] [
      report (item 0 dist-parm) + random-float ((item 1 dist-parm) - (item 0 dist-parm))
    ]

  ] dist-name = "uniform-integer" or dist-name = "uniform-int" or dist-name = "UI" [
    if length dist-parm < 2 or item 0 dist-parm > item 1 dist-parm [
      output-error (word "Invalid parameters for uniform integer distribution: " dist-parm)
    ]
    ifelse item 0 dist-parm = item 1 dist-parm [
      report item 0 dist-parm
    ] [
      report (item 0 dist-parm) + random (1 + (item 1 dist-parm) - (item 0 dist-parm))
    ]

  ] dist-name = "normal" or dist-name = "N" [
    if length dist-parm < 2 or item 1 dist-parm < 0 [
      output-error (word "Invalid parameters for normal distribution: " dist-parm)
    ]
    ifelse item 1 dist-parm = 0 [
      report item 0 dist-parm
    ] [
      report random-normal (item 0 dist-parm) (item 1 dist-parm)
    ]

  ] dist-name = "truncated-normal" or dist-name = "trunc-normal" or dist-name = "NT" [
    if length dist-parm < 4
    or item 1 dist-parm < 0
    or item 2 dist-parm > item 1 dist-parm
    or (item 1 dist-parm = 0 and (item 2 dist-parm > item 0 dist-parm) or (item 3 dist-parm < item 0 dist-parm)) [
      output-error (word "Invalid parameters for truncated normal distribution: " dist-parm)
    ]
    ifelse item 1 dist-parm = 0 [
      report item 0 dist-parm
    ] [
      let s random-normal (item 0 dist-parm) (item 1 dist-parm)
      (ifelse s < item 2 dist-parm [
        report item 2 dist-parm
      ] s > item 3 dist-parm [
        report item 3 dist-parm
      ] [
        report s
      ])
    ]
  ] [
    output-error (word "Unrecognized distribution name \"" dist-name "\"")
    report 0
  ])
end

; utility

to-report weighted-choice [ some-agents numeric-reporter ]
  let options []
  let weights []

  ask some-agents [
    set options lput self options
    set weights lput (runresult numeric-reporter) weights
  ]

  report weighted-choice-lists options weights
end

to-report best-choice [ some-agents numeric-reporter ]
  let options []
  let weights []

  ask some-agents [
    set options lput self options
    set weights lput (runresult numeric-reporter) weights
  ]

  report best-choice-lists options weights
end

to-report weighted-choice-table [ a-table ]
  let options []
  let weights []

  foreach table:keys a-table [ key ->
    set options lput key options
    set weights lput (table:get a-table key) weights
  ]

  report weighted-choice-lists options weights
end

to-report best-choice-table [ a-table ]
  let options []
  let weights []

  foreach table:keys a-table [ key ->
    set options lput key options
    set weights lput (table:get a-table key) weights
  ]

  report best-choice-lists options weights
end

to-report deterministic-best-choice-table [ a-table ]
  let options []
  let weights []

  foreach table:keys a-table [ key ->
    set options lput key options
    set weights lput (table:get a-table key) weights
  ]

  report deterministic-best-choice-lists options weights
end

; utility

to-report weighted-choice-lists [ options weights ]
  let neg-wgt? reduce [ [ so-far next ] -> so-far or next < 0 ] fput false weights
  if neg-wgt? [
    output-error "Attempt to make weighted choice with negative weight"
  ]
  let cumulative-sum but-first reduce [
    [ so-far next ] -> lput (next + last so-far) so-far
  ] fput [0] weights

  let choice random-float last cumulative-sum

  let ix reduce [
    ; we want the first ix such than choice <= cumulative-sum_ix
    [ i j ] -> ifelse-value choice <= (item i cumulative-sum) [ i ] [ j ]
  ] n-values (length weights) [ i -> i ]

  report item ix options
end

to-report best-choice-lists [ options weights ]
  let ixes reduce [
    [ so-far next ] -> ifelse-value length so-far = 0 [ (list next) ] [
      ifelse-value (item (first so-far) weights = item next weights) [ lput next so-far ] [
        ifelse-value (item (first so-far) weights < item next weights) [ (list next) ] [ so-far ]
      ]
    ]
  ] fput [] range length weights
  report item (one-of ixes) options
end

to-report deterministic-best-choice-lists [ options weights ]
  let ixes reduce [
    [ so-far next ] -> ifelse-value length so-far = 0 [ (list next) ] [
      ifelse-value (item (first so-far) weights = item next weights) [ lput next so-far ] [
        ifelse-value (item (first so-far) weights < item next weights) [ (list next) ] [ so-far ]
      ]
    ]
  ] fput [] range length weights
  let choice item first ixes options
  if is-turtle? choice and length ixes > 1 [
    let id [who] of choice
    foreach but-first ixes [ other-ix ->
      let other-option item other-ix options
      if [who] of other-option < id [
        set choice other-option
        set id [who] of choice
      ]
    ]
  ]
  report choice
end

to-report lu-colour-list
  report (list (green - 1) (brown - 2) (yellow - 2) (lime - 2) (turquoise - 1)
    (green + 1) (turquoise + 2) (brown + 1) (lime + 1) (brown - 3) (green - 2) (yellow - 3) )
end

to-report sp-colour-list
  report (list (red + 1) (blue - 1) (cyan + 1)  (magenta - 1) (pink - 2) (orange - 1) (violet - 1) (sky + 1) )
end

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Unit testing procedures
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


to fail
  set n-fails n-fails + 1
  set n-tests n-tests + 1
end

to pass
  set n-tests n-tests + 1
end

to assert-list-equals [ msg want get ]
  (ifelse not (is-list? want and is-list? get) [
    output-print (word msg ": FAIL (wanted "
      (ifelse-value is-list? want [ "is" ] [ "is not" ]) " a list; got "
      (ifelse-value is-list? get [ "is" ] [ "is not" ]) " a list)")
    fail
  ] length want != length get [
    output-print (word msg ": FAIL (wanted list length " (length want)
      "; got list length " (length get) ")")
    fail
  ] [
    foreach (n-values (length want) [ i -> i ]) [ i ->
      assert-equals (word msg " [" i "]") (item i want) (item i get)
    ]
    pass
  ])
end

to assert-equals [ msg want get ]
  ifelse want = get [
    output-print (word msg ": OK")
    pass
  ] [
    output-print (word msg ": FAIL (wanted \"" want "\"; got \"" get "\")")
    fail
  ]
end

to assert-true [ msg get? ]
  (ifelse not is-boolean? get? [
    output-print (word msg ": FAIL (wanted a boolean, got \"" get? "\")")
    fail
  ] get? [
    output-print (word msg ": OK")
    pass
  ] [
    output-print (word msg ": FAIL (wanted true; got false)")
    fail
  ])
end

to assert-false [ msg get? ]
  (ifelse not is-boolean? get? [
    output-print (word msg ": FAIL (wanted a boolean, got \"" get? "\")")
    fail
  ] get? [
    output-print (word msg ": FAIL (wanted false; got true)")
    fail
  ] [
    output-print (word msg ": OK")
    pass
  ])
end



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Notification procedures
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

to output-error [string]
  set error? true
  error string
end

to output-warning [string]
  if warnings = 0 [
    set warnings table:make
  ]
  ifelse table:has-key? warnings string [
    table:put warnings string 1 + table:get warnings string
  ] [
    output-print (word "WARNING [" timer "]: " string)
    table:put warnings string 1
  ]
end

to output-note [string]
  if notes = 0 [
    set notes table:make
  ]
  ifelse table:has-key? notes string [
    table:put notes string 1 + table:get notes string
  ] [
    output-print (word "NOTE [" timer "]: " string)
    table:put notes string 1
  ]
end

to print-progress [string]
  print (word "PROGRESS [" timer "]: " string)
end
@#$#@#$#@
GRAPHICS-WINDOW
278
58
741
522
-1
-1
7.0
1
10
1
1
1
0
1
1
1
-32
32
-32
32
0
0
1
ticks
30.0

BUTTON
10
16
77
49
NIL
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

BUTTON
78
16
141
49
step
go
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
0

BUTTON
143
16
206
49
NIL
go
T
1
T
OBSERVER
NIL
NIL
NIL
NIL
0

SWITCH
10
51
141
84
random-seed?
random-seed?
0
1
-1000

INPUTBOX
10
86
141
146
rng-seed
-4.01351177E8
1
0
Number

OUTPUT
745
10
1075
522
10

SWITCH
143
51
274
84
use-go-seed?
use-go-seed?
1
1
-1000

INPUTBOX
143
85
274
145
go-seed
0.0
1
0
Number

SWITCH
10
147
141
180
use-patch-file?
use-patch-file?
1
1
-1000

INPUTBOX
10
181
274
241
patch-file
NIL
1
0
String

BUTTON
143
147
204
180
choose
set patch-file user-file
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

BUTTON
206
147
274
180
new
set patch-file user-new-file
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

SWITCH
10
243
138
276
use-lu-file?
use-lu-file?
1
1
-1000

INPUTBOX
10
277
274
337
land-use-file
NIL
1
0
String

BUTTON
140
243
204
276
choose
set land-use-file user-file
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

BUTTON
206
243
274
276
new
set land-use-file user-new-file
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
10
670
274
703
patch-bitstring-length
patch-bitstring-length
0
100
10.0
1
1
NIL
HORIZONTAL

SLIDER
10
775
274
808
break-even-threshold
break-even-threshold
0
100
10.5
0.1
1
NIL
HORIZONTAL

SLIDER
10
705
274
738
climate-bitstring-length
climate-bitstring-length
0
100
10.0
1
1
NIL
HORIZONTAL

SLIDER
10
740
274
773
economy-bitstring-length
economy-bitstring-length
0
100
0.0
1
1
NIL
HORIZONTAL

SLIDER
10
811
274
844
patch-price
patch-price
1
200
20.0
1
1
NIL
HORIZONTAL

SLIDER
10
846
274
879
neighbour-weight-adjust
neighbour-weight-adjust
0
1
0.0
0.01
1
NIL
HORIZONTAL

SLIDER
10
881
274
914
neighbour-noise-min
neighbour-noise-min
0
1
0.0
0.01
1
NIL
HORIZONTAL

SLIDER
10
917
274
950
neighbour-noise-max
neighbour-noise-max
0
1
0.0
0.01
1
NIL
HORIZONTAL

SLIDER
10
952
274
985
p-economy-change
p-economy-change
0
0.5
0.0
0.005
1
NIL
HORIZONTAL

SLIDER
10
987
274
1020
p-climate-change
p-climate-change
0
0.5
0.1
0.005
1
NIL
HORIZONTAL

SWITCH
10
340
151
373
use-climate-file?
use-climate-file?
1
1
-1000

INPUTBOX
10
374
274
434
climate-file
NIL
1
0
String

BUTTON
153
340
211
373
choose
set climate-file user-file
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

BUTTON
213
340
274
373
new
set climate-file user-new-file
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

SWITCH
10
437
162
470
use-economy-file?
use-economy-file?
1
1
-1000

INPUTBOX
10
471
274
531
economy-file
NIL
1
0
String

BUTTON
164
437
219
470
choose
set economy-file user-file
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

BUTTON
219
437
274
470
new
set economy-file user-new-file
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
439
750
578
783
n-land-use
n-land-use
1
20
6.0
1
1
NIL
HORIZONTAL

SLIDER
276
750
437
783
p-lu-wild
p-lu-wild
0
0.1
0.0
0.001
1
NIL
HORIZONTAL

CHOOSER
581
750
780
795
neighbourhood
neighbourhood
"Moore" "von Neumann" "Hexagonal"
0

CHOOSER
334
10
464
55
visualize
visualize
"Land Uses" "Land Managers" "Sub-populations" "Biophysical Properties"
0

SLIDER
466
22
741
55
biophysical-properties-bit-to-visualize
biophysical-properties-bit-to-visualize
0
100
1.0
1
1
NIL
HORIZONTAL

INPUTBOX
276
793
378
853
subpop-name
II
1
0
String

INPUTBOX
782
774
973
834
new-subpop-nbr-weight-dist
1
1
0
String

INPUTBOX
782
836
973
896
new-subpop-p-imitate-dist
1
1
0
String

INPUTBOX
782
898
973
958
new-subpop-aspiration-dist
21
1
0
String

INPUTBOX
782
960
973
1020
new-subpop-memory-size-dist
1
1
0
String

CHOOSER
580
834
780
879
new-subpop-initial-strategy
new-subpop-initial-strategy
"Random" "Specialist" "Fickle"
0

CHOOSER
580
881
780
926
new-subpop-satisfice-strategy
new-subpop-satisfice-strategy
"Null" "Habit" "Random" "Specialist" "Fickle" "BestMatch" "StableMatch" "WeightedMatch" "StableLast" "LastBest" "WeightedLast" "StableLastN" "LastNBest" "WeightedLastN"
0

CHOOSER
580
928
780
973
new-subpop-imitative-strategy
new-subpop-imitative-strategy
"Null" "MajorityBest" "WeightedMajority" "PhysicalMajorityBest" "PhysicalWeightedMajority" "CopyLastBest" "CopyWeightedLast" "CopyBestMatch" "CopyWeightedMatch" "CopyOther" "SmartCopyBest" "StableSmartCopy" "WeightedSmartCopy" "WeightedYieldCopy" "MatchWeightedYieldCopy" "MeanWeightedYieldCopy" "WeightedYieldTCopy" "MeanWeightedYieldTCopy" "MeanYieldTBestCopy" "YieldTBestCopy"
10

CHOOSER
580
975
780
1020
new-subpop-experiment-strategy
new-subpop-experiment-strategy
"Null" "Random" "Fickle" "BestMatch" "StableMatch" "WeightedMatch" "StableLast" "LastBest" "WeightedLast" "StableLastN" "LastNBest" "WeightedLastN" "CopyOther"
0

SLIDER
580
798
711
831
new-subpop-p
new-subpop-p
0
1
0.5
0.01
1
NIL
HORIZONTAL

INPUTBOX
276
855
578
1020
subpop-str
[[0.5 \"HRYI\" \"1\" \"0.9375\" \"11\" \"1\" \"Random\" \"Habit\" \"WeightedYieldCopy\" \"Random\"]\n[0.5 \"II\" \"1\" \"1\" \"21\" \"1\" \"Random\" \"Null\" \"SmartCopyBest\" \"Null\"]]
1
1
String

BUTTON
379
785
455
818
Add Subpop
add-subpop-str
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

BUTTON
379
820
488
853
Add Last Subpop
add-subpop-str-last
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

BUTTON
456
785
578
818
Remove Subpop
del-subpop-str
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

BUTTON
489
820
578
853
Clear Subpops
set subpop-str \"[]\"
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
10
568
274
628
subpop-file
NIL
1
0
String

BUTTON
10
630
144
663
Save Subpops
save-subpops subpop-file
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

BUTTON
146
630
274
663
Load Subpops
read-subpops subpop-file
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

SWITCH
10
534
154
567
use-subpop-file?
use-subpop-file?
1
1
-1000

BUTTON
156
534
211
567
choose
set subpop-file user-file
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

BUTTON
213
534
274
567
new
set subpop-file user-new-file
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

PLOT
279
525
652
745
Land Uses
NIL
NIL
0.0
10.0
0.0
10.0
true
false
"ask land-uses [\n  create-temporary-plot-pen (word \"LU\" who)\n  set-plot-pen-color color\n]" "ask land-uses [\n  set-current-plot-pen (word \"LU\" who)\n  plot count patches with [use = myself]\n]"
PENS

PLOT
655
525
1075
740
Sub-populations
NIL
NIL
0.0
10.0
0.0
10.0
true
true
"ask sub-populations [\n  create-temporary-plot-pen name\n  set-plot-pen-color color\n]" "ask sub-populations [\n  set-current-plot-pen name\n  plot sum [length parcels-list] of land-managers with [sub-pop = myself]\n]"
PENS

BUTTON
714
798
780
831
=P(sp)
equalize-subpop-str-p
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
782
740
974
773
n-clump-swaps
n-clump-swaps
0
20000
0.0
100
1
NIL
HORIZONTAL

CHOOSER
975
741
1075
786
imit-2001-env
imit-2001-env
1 2 3
1

CHOOSER
975
824
1075
869
imit-2001-sp
imit-2001-sp
"RS" "OS" "OD" "LS" "LD" "SI" "YI" "HYI" "HRYI" "II"
9

BUTTON
975
788
1075
821
Set 2001 Env
set-env-param-imitation imit-2001-env
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

BUTTON
975
871
1075
904
Add 2001 SP
add-subpop-imitation imit-2001-sp
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

BUTTON
208
16
274
49
go201
repeat 201 [\n  go\n]
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
0

BUTTON
277
16
332
49
viz
visualization
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

@#$#@#$#@
## WHAT IS IT?

(a general understanding of what the model is trying to show or explain)

## HOW IT WORKS

(what rules the agents use to create the overall behavior of the model)

## HOW TO USE IT

(how to use the model, including a description of each of the items in the Interface tab)

## THINGS TO NOTICE

(suggested things for the user to notice while running the model)

## THINGS TO TRY

(suggested things for the user to try to do (move sliders, switches, etc.) with the model)

## EXTENDING THE MODEL

(suggested things to add or change in the Code tab to make the model more complicated, detailed, accurate, etc.)

## NETLOGO FEATURES

(interesting or unusual features of NetLogo that the model uses, particularly in the Code tab; or where workarounds were needed for missing features)

## RELATED MODELS

(models in the NetLogo Models Library and elsewhere which are of related interest)

## CREDITS AND REFERENCES

(a reference to the model's URL on the web if it has one, as well as any other necessary credits, citations, and links)

## LICENCE
```text
                        GNU GENERAL PUBLIC LICENSE
                           Version 3, 29 June 2007
    
     Copyright (C) 2007 Free Software Foundation, Inc. <http://fsf.org/>
     Everyone is permitted to copy and distribute verbatim copies
     of this license document, but changing it is not allowed.
    
                                Preamble
    
      The GNU General Public License is a free, copyleft license for
    software and other kinds of works.
    
      The licenses for most software and other practical works are designed
    to take away your freedom to share and change the works.  By contrast,
    the GNU General Public License is intended to guarantee your freedom to
    share and change all versions of a program--to make sure it remains free
    software for all its users.  We, the Free Software Foundation, use the
    GNU General Public License for most of our software; it applies also to
    any other work released this way by its authors.  You can apply it to
    your programs, too.
    
      When we speak of free software, we are referring to freedom, not
    price.  Our General Public Licenses are designed to make sure that you
    have the freedom to distribute copies of free software (and charge for
    them if you wish), that you receive source code or can get it if you
    want it, that you can change the software or use pieces of it in new
    free programs, and that you know you can do these things.
    
      To protect your rights, we need to prevent others from denying you
    these rights or asking you to surrender the rights.  Therefore, you have
    certain responsibilities if you distribute copies of the software, or if
    you modify it: responsibilities to respect the freedom of others.
    
      For example, if you distribute copies of such a program, whether
    gratis or for a fee, you must pass on to the recipients the same
    freedoms that you received.  You must make sure that they, too, receive
    or can get the source code.  And you must show them these terms so they
    know their rights.
    
      Developers that use the GNU GPL protect your rights with two steps:
    (1) assert copyright on the software, and (2) offer you this License
    giving you legal permission to copy, distribute and/or modify it.
    
      For the developers' and authors' protection, the GPL clearly explains
    that there is no warranty for this free software.  For both users' and
    authors' sake, the GPL requires that modified versions be marked as
    changed, so that their problems will not be attributed erroneously to
    authors of previous versions.
    
      Some devices are designed to deny users access to install or run
    modified versions of the software inside them, although the manufacturer
    can do so.  This is fundamentally incompatible with the aim of
    protecting users' freedom to change the software.  The systematic
    pattern of such abuse occurs in the area of products for individuals to
    use, which is precisely where it is most unacceptable.  Therefore, we
    have designed this version of the GPL to prohibit the practice for those
    products.  If such problems arise substantially in other domains, we
    stand ready to extend this provision to those domains in future versions
    of the GPL, as needed to protect the freedom of users.
    
      Finally, every program is threatened constantly by software patents.
    States should not allow patents to restrict development and use of
    software on general-purpose computers, but in those that do, we wish to
    avoid the special danger that patents applied to a free program could
    make it effectively proprietary.  To prevent this, the GPL assures that
    patents cannot be used to render the program non-free.
    
      The precise terms and conditions for copying, distribution and
    modification follow.
    
                           TERMS AND CONDITIONS
    
      0. Definitions.
    
      "This License" refers to version 3 of the GNU General Public License.
    
      "Copyright" also means copyright-like laws that apply to other kinds of
    works, such as semiconductor masks.
    
      "The Program" refers to any copyrightable work licensed under this
    License.  Each licensee is addressed as "you".  "Licensees" and
    "recipients" may be individuals or organizations.
    
      To "modify" a work means to copy from or adapt all or part of the work
    in a fashion requiring copyright permission, other than the making of an
    exact copy.  The resulting work is called a "modified version" of the
    earlier work or a work "based on" the earlier work.
    
      A "covered work" means either the unmodified Program or a work based
    on the Program.
    
      To "propagate" a work means to do anything with it that, without
    permission, would make you directly or secondarily liable for
    infringement under applicable copyright law, except executing it on a
    computer or modifying a private copy.  Propagation includes copying,
    distribution (with or without modification), making available to the
    public, and in some countries other activities as well.
    
      To "convey" a work means any kind of propagation that enables other
    parties to make or receive copies.  Mere interaction with a user through
    a computer network, with no transfer of a copy, is not conveying.
    
      An interactive user interface displays "Appropriate Legal Notices"
    to the extent that it includes a convenient and prominently visible
    feature that (1) displays an appropriate copyright notice, and (2)
    tells the user that there is no warranty for the work (except to the
    extent that warranties are provided), that licensees may convey the
    work under this License, and how to view a copy of this License.  If
    the interface presents a list of user commands or options, such as a
    menu, a prominent item in the list meets this criterion.
    
      1. Source Code.
    
      The "source code" for a work means the preferred form of the work
    for making modifications to it.  "Object code" means any non-source
    form of a work.
    
      A "Standard Interface" means an interface that either is an official
    standard defined by a recognized standards body, or, in the case of
    interfaces specified for a particular programming language, one that
    is widely used among developers working in that language.
    
      The "System Libraries" of an executable work include anything, other
    than the work as a whole, that (a) is included in the normal form of
    packaging a Major Component, but which is not part of that Major
    Component, and (b) serves only to enable use of the work with that
    Major Component, or to implement a Standard Interface for which an
    implementation is available to the public in source code form.  A
    "Major Component", in this context, means a major essential component
    (kernel, window system, and so on) of the specific operating system
    (if any) on which the executable work runs, or a compiler used to
    produce the work, or an object code interpreter used to run it.
        
      The "Corresponding Source" for a work in object code form means all
    the source code needed to generate, install, and (for an executable
    work) run the object code and to modify the work, including scripts to
    control those activities.  However, it does not include the work's
    System Libraries, or general-purpose tools or generally available free
    programs which are used unmodified in performing those activities but
    which are not part of the work.  For example, Corresponding Source
    includes interface definition files associated with source files for
    the work, and the source code for shared libraries and dynamically
    linked subprograms that the work is specifically designed to require,
    such as by intimate data communication or control flow between those
    subprograms and other parts of the work.
    
      The Corresponding Source need not include anything that users
    can regenerate automatically from other parts of the Corresponding
    Source.
    
      The Corresponding Source for a work in source code form is that
    same work.
    
      2. Basic Permissions.
    
      All rights granted under this License are granted for the term of
    copyright on the Program, and are irrevocable provided the stated
    conditions are met.  This License explicitly affirms your unlimited
    permission to run the unmodified Program.  The output from running a
    covered work is covered by this License only if the output, given its
    content, constitutes a covered work.  This License acknowledges your
    rights of fair use or other equivalent, as provided by copyright law.
    
      You may make, run and propagate covered works that you do not
    convey, without conditions so long as your license otherwise remains
    in force.  You may convey covered works to others for the sole purpose
    of having them make modifications exclusively for you, or provide you
    with facilities for running those works, provided that you comply with
    the terms of this License in conveying all material for which you do
    not control copyright.  Those thus making or running the covered works
    for you must do so exclusively on your behalf, under your direction
    and control, on terms that prohibit them from making any copies of
    your copyrighted material outside their relationship with you.
    
      Conveying under any other circumstances is permitted solely under
    the conditions stated below.  Sublicensing is not allowed; section 10
    makes it unnecessary.
    
      3. Protecting Users' Legal Rights From Anti-Circumvention Law.
    
      No covered work shall be deemed part of an effective technological
    measure under any applicable law fulfilling obligations under article
    11 of the WIPO copyright treaty adopted on 20 December 1996, or
    similar laws prohibiting or restricting circumvention of such
    measures.
    
      When you convey a covered work, you waive any legal power to forbid
    circumvention of technological measures to the extent such circumvention
    is effected by exercising rights under this License with respect to
    the covered work, and you disclaim any intention to limit operation or
    modification of the work as a means of enforcing, against the work's
    users, your or third parties' legal rights to forbid circumvention of
    technological measures.
    
      4. Conveying Verbatim Copies.
    
      You may convey verbatim copies of the Program's source code as you
    receive it, in any medium, provided that you conspicuously and
    appropriately publish on each copy an appropriate copyright notice;
    keep intact all notices stating that this License and any
    non-permissive terms added in accord with section 7 apply to the code;
    keep intact all notices of the absence of any warranty; and give all
    recipients a copy of this License along with the Program.
    
      You may charge any price or no price for each copy that you convey,
    and you may offer support or warranty protection for a fee.
    
      5. Conveying Modified Source Versions.
    
      You may convey a work based on the Program, or the modifications to
    produce it from the Program, in the form of source code under the
    terms of section 4, provided that you also meet all of these conditions:
    
        a) The work must carry prominent notices stating that you modified
        it, and giving a relevant date.
    
        b) The work must carry prominent notices stating that it is
        released under this License and any conditions added under section
        7.  This requirement modifies the requirement in section 4 to
        "keep intact all notices".
    
        c) You must license the entire work, as a whole, under this
        License to anyone who comes into possession of a copy.  This
        License will therefore apply, along with any applicable section 7
        additional terms, to the whole of the work, and all its parts,
        regardless of how they are packaged.  This License gives no
        permission to license the work in any other way, but it does not
        invalidate such permission if you have separately received it.
    
        d) If the work has interactive user interfaces, each must display
        Appropriate Legal Notices; however, if the Program has interactive
        interfaces that do not display Appropriate Legal Notices, your
        work need not make them do so.
    
      A compilation of a covered work with other separate and independent
    works, which are not by their nature extensions of the covered work,
    and which are not combined with it such as to form a larger program,
    in or on a volume of a storage or distribution medium, is called an
    "aggregate" if the compilation and its resulting copyright are not
    used to limit the access or legal rights of the compilation's users
    beyond what the individual works permit.  Inclusion of a covered work
    in an aggregate does not cause this License to apply to the other
    parts of the aggregate.
    
      6. Conveying Non-Source Forms.
    
      You may convey a covered work in object code form under the terms
    of sections 4 and 5, provided that you also convey the
    machine-readable Corresponding Source under the terms of this License,
    in one of these ways:
    
        a) Convey the object code in, or embodied in, a physical product
        (including a physical distribution medium), accompanied by the
        Corresponding Source fixed on a durable physical medium
        customarily used for software interchange.
    
        b) Convey the object code in, or embodied in, a physical product
        (including a physical distribution medium), accompanied by a
        written offer, valid for at least three years and valid for as
        long as you offer spare parts or customer support for that product
        model, to give anyone who possesses the object code either (1) a
        copy of the Corresponding Source for all the software in the
        product that is covered by this License, on a durable physical
        medium customarily used for software interchange, for a price no
        more than your reasonable cost of physically performing this
        conveying of source, or (2) access to copy the
        Corresponding Source from a network server at no charge.
    
        c) Convey individual copies of the object code with a copy of the
        written offer to provide the Corresponding Source.  This
        alternative is allowed only occasionally and noncommercially, and
        only if you received the object code with such an offer, in accord
        with subsection 6b.
    
        d) Convey the object code by offering access from a designated
        place (gratis or for a charge), and offer equivalent access to the
        Corresponding Source in the same way through the same place at no
        further charge.  You need not require recipients to copy the
        Corresponding Source along with the object code.  If the place to
        copy the object code is a network server, the Corresponding Source
        may be on a different server (operated by you or a third party)
        that supports equivalent copying facilities, provided you maintain
        clear directions next to the object code saying where to find the
        Corresponding Source.  Regardless of what server hosts the
        Corresponding Source, you remain obligated to ensure that it is
        available for as long as needed to satisfy these requirements.
    
        e) Convey the object code using peer-to-peer transmission, provided
        you inform other peers where the object code and Corresponding
        Source of the work are being offered to the general public at no
        charge under subsection 6d.
    
      A separable portion of the object code, whose source code is excluded
    from the Corresponding Source as a System Library, need not be
    included in conveying the object code work.
    
      A "User Product" is either (1) a "consumer product", which means any
    tangible personal property which is normally used for personal, family,
    or household purposes, or (2) anything designed or sold for incorporation
    into a dwelling.  In determining whether a product is a consumer product,
    doubtful cases shall be resolved in favor of coverage.  For a particular
    product received by a particular user, "normally used" refers to a
    typical or common use of that class of product, regardless of the status
    of the particular user or of the way in which the particular user
    actually uses, or expects or is expected to use, the product.  A product
    is a consumer product regardless of whether the product has substantial
    commercial, industrial or non-consumer uses, unless such uses represent
    the only significant mode of use of the product.
    
      "Installation Information" for a User Product means any methods,
    procedures, authorization keys, or other information required to install
    and execute modified versions of a covered work in that User Product from
    a modified version of its Corresponding Source.  The information must
    suffice to ensure that the continued functioning of the modified object
    code is in no case prevented or interfered with solely because
    modification has been made.
    
      If you convey an object code work under this section in, or with, or
    specifically for use in, a User Product, and the conveying occurs as
    part of a transaction in which the right of possession and use of the
    User Product is transferred to the recipient in perpetuity or for a
    fixed term (regardless of how the transaction is characterized), the
    Corresponding Source conveyed under this section must be accompanied
    by the Installation Information.  But this requirement does not apply
    if neither you nor any third party retains the ability to install
    modified object code on the User Product (for example, the work has
    been installed in ROM).
    
      The requirement to provide Installation Information does not include a
    requirement to continue to provide support service, warranty, or updates
    for a work that has been modified or installed by the recipient, or for
    the User Product in which it has been modified or installed.  Access to a
    network may be denied when the modification itself materially and
    adversely affects the operation of the network or violates the rules and
    protocols for communication across the network.
    
      Corresponding Source conveyed, and Installation Information provided,
    in accord with this section must be in a format that is publicly
    documented (and with an implementation available to the public in
    source code form), and must require no special password or key for
    unpacking, reading or copying.
    
      7. Additional Terms.
    
      "Additional permissions" are terms that supplement the terms of this
    License by making exceptions from one or more of its conditions.
    Additional permissions that are applicable to the entire Program shall
    be treated as though they were included in this License, to the extent
    that they are valid under applicable law.  If additional permissions
    apply only to part of the Program, that part may be used separately
    under those permissions, but the entire Program remains governed by
    this License without regard to the additional permissions.
    
      When you convey a copy of a covered work, you may at your option
    remove any additional permissions from that copy, or from any part of
    it.  (Additional permissions may be written to require their own
    removal in certain cases when you modify the work.)  You may place
    additional permissions on material, added by you to a covered work,
    for which you have or can give appropriate copyright permission.
    
      Notwithstanding any other provision of this License, for material you
    add to a covered work, you may (if authorized by the copyright holders of
    that material) supplement the terms of this License with terms:
    
        a) Disclaiming warranty or limiting liability differently from the
        terms of sections 15 and 16 of this License; or
    
        b) Requiring preservation of specified reasonable legal notices or
        author attributions in that material or in the Appropriate Legal
        Notices displayed by works containing it; or
    
        c) Prohibiting misrepresentation of the origin of that material, or
        requiring that modified versions of such material be marked in
        reasonable ways as different from the original version; or
    
        d) Limiting the use for publicity purposes of names of licensors or
        authors of the material; or
    
        e) Declining to grant rights under trademark law for use of some
        trade names, trademarks, or service marks; or
    
        f) Requiring indemnification of licensors and authors of that
        material by anyone who conveys the material (or modified versions of
        it) with contractual assumptions of liability to the recipient, for
        any liability that these contractual assumptions directly impose on
        those licensors and authors.
    
      All other non-permissive additional terms are considered "further
    restrictions" within the meaning of section 10.  If the Program as you
    received it, or any part of it, contains a notice stating that it is
    governed by this License along with a term that is a further
    restriction, you may remove that term.  If a license document contains
    a further restriction but permits relicensing or conveying under this
    License, you may add to a covered work material governed by the terms
    of that license document, provided that the further restriction does
    not survive such relicensing or conveying.
    
      If you add terms to a covered work in accord with this section, you
    must place, in the relevant source files, a statement of the
    additional terms that apply to those files, or a notice indicating
    where to find the applicable terms.
    
      Additional terms, permissive or non-permissive, may be stated in the
    form of a separately written license, or stated as exceptions;
    the above requirements apply either way.
    
      8. Termination.
    
      You may not propagate or modify a covered work except as expressly
    provided under this License.  Any attempt otherwise to propagate or
    modify it is void, and will automatically terminate your rights under
    this License (including any patent licenses granted under the third
    paragraph of section 11).
    
      However, if you cease all violation of this License, then your
    license from a particular copyright holder is reinstated (a)
    provisionally, unless and until the copyright holder explicitly and
    finally terminates your license, and (b) permanently, if the copyright
    holder fails to notify you of the violation by some reasonable means
    prior to 60 days after the cessation.
    
      Moreover, your license from a particular copyright holder is
    reinstated permanently if the copyright holder notifies you of the
    violation by some reasonable means, this is the first time you have
    received notice of violation of this License (for any work) from that
    copyright holder, and you cure the violation prior to 30 days after
    your receipt of the notice.
    
      Termination of your rights under this section does not terminate the
    licenses of parties who have received copies or rights from you under
    this License.  If your rights have been terminated and not permanently
    reinstated, you do not qualify to receive new licenses for the same
    material under section 10.
    
      9. Acceptance Not Required for Having Copies.
    
      You are not required to accept this License in order to receive or
    run a copy of the Program.  Ancillary propagation of a covered work
    occurring solely as a consequence of using peer-to-peer transmission
    to receive a copy likewise does not require acceptance.  However,
    nothing other than this License grants you permission to propagate or
    modify any covered work.  These actions infringe copyright if you do
    not accept this License.  Therefore, by modifying or propagating a
    covered work, you indicate your acceptance of this License to do so.
    
      10. Automatic Licensing of Downstream Recipients.
    
      Each time you convey a covered work, the recipient automatically
    receives a license from the original licensors, to run, modify and
    propagate that work, subject to this License.  You are not responsible
    for enforcing compliance by third parties with this License.
    
      An "entity transaction" is a transaction transferring control of an
    organization, or substantially all assets of one, or subdividing an
    organization, or merging organizations.  If propagation of a covered
    work results from an entity transaction, each party to that
    transaction who receives a copy of the work also receives whatever
    licenses to the work the party's predecessor in interest had or could
    give under the previous paragraph, plus a right to possession of the
    Corresponding Source of the work from the predecessor in interest, if
    the predecessor has it or can get it with reasonable efforts.
    
      You may not impose any further restrictions on the exercise of the
    rights granted or affirmed under this License.  For example, you may
    not impose a license fee, royalty, or other charge for exercise of
    rights granted under this License, and you may not initiate litigation
    (including a cross-claim or counterclaim in a lawsuit) alleging that
    any patent claim is infringed by making, using, selling, offering for
    sale, or importing the Program or any portion of it.
    
      11. Patents.
    
      A "contributor" is a copyright holder who authorizes use under this
    License of the Program or a work on which the Program is based.  The
    work thus licensed is called the contributor's "contributor version".
    
      A contributor's "essential patent claims" are all patent claims
    owned or controlled by the contributor, whether already acquired or
    hereafter acquired, that would be infringed by some manner, permitted
    by this License, of making, using, or selling its contributor version,
    but do not include claims that would be infringed only as a
    consequence of further modification of the contributor version.  For
    purposes of this definition, "control" includes the right to grant
    patent sublicenses in a manner consistent with the requirements of
    this License.
    
      Each contributor grants you a non-exclusive, worldwide, royalty-free
    patent license under the contributor's essential patent claims, to
    make, use, sell, offer for sale, import and otherwise run, modify and
    propagate the contents of its contributor version.
    
      In the following three paragraphs, a "patent license" is any express
    agreement or commitment, however denominated, not to enforce a patent
    (such as an express permission to practice a patent or covenant not to
    sue for patent infringement).  To "grant" such a patent license to a
    party means to make such an agreement or commitment not to enforce a
    patent against the party.
    
      If you convey a covered work, knowingly relying on a patent license,
    and the Corresponding Source of the work is not available for anyone
    to copy, free of charge and under the terms of this License, through a
    publicly available network server or other readily accessible means,
    then you must either (1) cause the Corresponding Source to be so
    available, or (2) arrange to deprive yourself of the benefit of the
    patent license for this particular work, or (3) arrange, in a manner
    consistent with the requirements of this License, to extend the patent
    license to downstream recipients.  "Knowingly relying" means you have
    actual knowledge that, but for the patent license, your conveying the
    covered work in a country, or your recipient's use of the covered work
    in a country, would infringe one or more identifiable patents in that
    country that you have reason to believe are valid.
    
      If, pursuant to or in connection with a single transaction or
    arrangement, you convey, or propagate by procuring conveyance of, a
    covered work, and grant a patent license to some of the parties
    receiving the covered work authorizing them to use, propagate, modify
    or convey a specific copy of the covered work, then the patent license
    you grant is automatically extended to all recipients of the covered
    work and works based on it.
    
      A patent license is "discriminatory" if it does not include within
    the scope of its coverage, prohibits the exercise of, or is
    conditioned on the non-exercise of one or more of the rights that are
    specifically granted under this License.  You may not convey a covered
    work if you are a party to an arrangement with a third party that is
    in the business of distributing software, under which you make payment
    to the third party based on the extent of your activity of conveying
    the work, and under which the third party grants, to any of the
    parties who would receive the covered work from you, a discriminatory
    patent license (a) in connection with copies of the covered work
    conveyed by you (or copies made from those copies), or (b) primarily
    for and in connection with specific products or compilations that
    contain the covered work, unless you entered into that arrangement,
    or that patent license was granted, prior to 28 March 2007.
    
      Nothing in this License shall be construed as excluding or limiting
    any implied license or other defenses to infringement that may
    otherwise be available to you under applicable patent law.
    
      12. No Surrender of Others' Freedom.
    
      If conditions are imposed on you (whether by court order, agreement or
    otherwise) that contradict the conditions of this License, they do not
    excuse you from the conditions of this License.  If you cannot convey a
    covered work so as to satisfy simultaneously your obligations under this
    License and any other pertinent obligations, then as a consequence you may
    not convey it at all.  For example, if you agree to terms that obligate you
    to collect a royalty for further conveying from those to whom you convey
    the Program, the only way you could satisfy both those terms and this
    License would be to refrain entirely from conveying the Program.
    
      13. Use with the GNU Affero General Public License.
    
      Notwithstanding any other provision of this License, you have
    permission to link or combine any covered work with a work licensed
    under version 3 of the GNU Affero General Public License into a single
    combined work, and to convey the resulting work.  The terms of this
    License will continue to apply to the part which is the covered work,
    but the special requirements of the GNU Affero General Public License,
    section 13, concerning interaction through a network will apply to the
    combination as such.
    
      14. Revised Versions of this License.
    
      The Free Software Foundation may publish revised and/or new versions of
    the GNU General Public License from time to time.  Such new versions will
    be similar in spirit to the present version, but may differ in detail to
    address new problems or concerns.
    
      Each version is given a distinguishing version number.  If the
    Program specifies that a certain numbered version of the GNU General
    Public License "or any later version" applies to it, you have the
    option of following the terms and conditions either of that numbered
    version or of any later version published by the Free Software
    Foundation.  If the Program does not specify a version number of the
    GNU General Public License, you may choose any version ever published
    by the Free Software Foundation.
    
      If the Program specifies that a proxy can decide which future
    versions of the GNU General Public License can be used, that proxy's
    public statement of acceptance of a version permanently authorizes you
    to choose that version for the Program.
    
      Later license versions may give you additional or different
    permissions.  However, no additional obligations are imposed on any
    author or copyright holder as a result of your choosing to follow a
    later version.
    
      15. Disclaimer of Warranty.
    
      THERE IS NO WARRANTY FOR THE PROGRAM, TO THE EXTENT PERMITTED BY
    APPLICABLE LAW.  EXCEPT WHEN OTHERWISE STATED IN WRITING THE COPYRIGHT
    HOLDERS AND/OR OTHER PARTIES PROVIDE THE PROGRAM "AS IS" WITHOUT WARRANTY
    OF ANY KIND, EITHER EXPRESSED OR IMPLIED, INCLUDING, BUT NOT LIMITED TO,
    THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR
    PURPOSE.  THE ENTIRE RISK AS TO THE QUALITY AND PERFORMANCE OF THE PROGRAM
    IS WITH YOU.  SHOULD THE PROGRAM PROVE DEFECTIVE, YOU ASSUME THE COST OF
    ALL NECESSARY SERVICING, REPAIR OR CORRECTION.
    
      16. Limitation of Liability.
    
      IN NO EVENT UNLESS REQUIRED BY APPLICABLE LAW OR AGREED TO IN WRITING
    WILL ANY COPYRIGHT HOLDER, OR ANY OTHER PARTY WHO MODIFIES AND/OR CONVEYS
    THE PROGRAM AS PERMITTED ABOVE, BE LIABLE TO YOU FOR DAMAGES, INCLUDING ANY
    GENERAL, SPECIAL, INCIDENTAL OR CONSEQUENTIAL DAMAGES ARISING OUT OF THE
    USE OR INABILITY TO USE THE PROGRAM (INCLUDING BUT NOT LIMITED TO LOSS OF
    DATA OR DATA BEING RENDERED INACCURATE OR LOSSES SUSTAINED BY YOU OR THIRD
    PARTIES OR A FAILURE OF THE PROGRAM TO OPERATE WITH ANY OTHER PROGRAMS),
    EVEN IF SUCH HOLDER OR OTHER PARTY HAS BEEN ADVISED OF THE POSSIBILITY OF
    SUCH DAMAGES.
    
      17. Interpretation of Sections 15 and 16.
    
      If the disclaimer of warranty and limitation of liability provided
    above cannot be given local legal effect according to their terms,
    reviewing courts shall apply local law that most closely approximates
    an absolute waiver of all civil liability in connection with the
    Program, unless a warranty or assumption of liability accompanies a
    copy of the Program in return for a fee.
    
                         END OF TERMS AND CONDITIONS
    
                How to Apply These Terms to Your New Programs
    
      If you develop a new program, and you want it to be of the greatest
    possible use to the public, the best way to achieve this is to make it
    free software which everyone can redistribute and change under these terms.
    
      To do so, attach the following notices to the program.  It is safest
    to attach them to the start of each source file to most effectively
    state the exclusion of warranty; and each file should have at least
    the "copyright" line and a pointer to where the full notice is found.
    
        <one line to give the program's name and a brief idea of what it does.>
        Copyright (C) <year>  <name of author>
    
        This program is free software: you can redistribute it and/or modify
        it under the terms of the GNU General Public License as published by
        the Free Software Foundation, either version 3 of the License, or
        (at your option) any later version.
    
        This program is distributed in the hope that it will be useful,
        but WITHOUT ANY WARRANTY; without even the implied warranty of
        MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
        GNU General Public License for more details.
    
        You should have received a copy of the GNU General Public License
        along with this program.  If not, see <http://www.gnu.org/licenses/>.
    
    Also add information on how to contact you by electronic and paper mail.
    
      If the program does terminal interaction, make it output a short
    notice like this when it starts in an interactive mode:
    
        <program>  Copyright (C) <year>  <name of author>
        This program comes with ABSOLUTELY NO WARRANTY; for details type `show w'.
        This is free software, and you are welcome to redistribute it
        under certain conditions; type `show c' for details.
    
    The hypothetical commands `show w' and `show c' should show the appropriate
    parts of the General Public License.  Of course, your program's commands
    might be different; for a GUI interface, you would use an "about box".
    
      You should also get your employer (if you work as a programmer) or school,
    if any, to sign a "copyright disclaimer" for the program, if necessary.
    For more information on this, and how to apply and follow the GNU GPL, see
    <http://www.gnu.org/licenses/>.

      The GNU General Public License does not permit incorporating your program
    into proprietary programs.  If your program is a subroutine library, you
    may consider it more useful to permit linking proprietary applications with
    the library.  If this is what you want to do, use the GNU Lesser General
    Public License instead of this License.  But first, please read
    <http://www.gnu.org/philosophy/why-not-lgpl.html>.
```

## ChangeLog

2024-01-26 Gary Polhill <gary.polhill@hutton.ac.uk>

  * `setup`: Put `carefully` around model-specific code so that errors are caught and handled in headless mode
  * `go`: Put `carefully` around model-specific code so that errors are caught and handled in headless mode


2023-11-01 Gary Polhill <gary.polhill@hutton.ac.uk>

  * `setup`: Added seed control

2019-06-16 Gary Polhill <gary.polhill@hutton.ac.uk>

  * Updated to NetLogo version 6.1

2014-10-26 Gary Polhill <gary.polhill@hutton.ac.uk>

  * Created template
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
NetLogo 6.4.0
@#$#@#$#@
@#$#@#$#@
@#$#@#$#@
<experiments>
  <experiment name="Template Experiment" repetitions="1" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go</go>
    <exitCondition>error?</exitCondition>
    <metric>rng-seed</metric>
    <enumeratedValueSet variable="random-seed?">
      <value value="true"/>
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
