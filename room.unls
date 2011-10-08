; -*- Lisp -*-
;; rooms
(defmacro room-sdesc car)
(defmacro room-ldesc cadr)
(defmacro room-travel caddr)

(defmacro def-room1
  (list (string "AT END OF ROAD AGAIN")
        (string "YOU ARE STANDING AT THE END OF A ROAD BEFORE A SMALL BRICK BUILDING.\nAROUND YOU IS A FOREST.  A SMALL STREAM FLOWS OUT OF THE BUILDING AND\nDOWN A GULLY.\n")))

(defmacro def-room2
  (list (string "AT HILL IN ROAD")
        (string "YOU HAVE WALKED UP A HILL, STILL IN THE FOREST.  THE ROAD SLOPES BACK\nDOWN THE OTHER SIDE OF THE HILL.  THERE IS A BUILDING IN THE DISTANCE.\n")))

(defmacro def-room3
  (list (string "INSIDE BUILDING")
        (string "YOU ARE INSIDE A BUILDING, A WELL HOUSE FOR A LARGE SPRING.\n")))

(defmacro def-room4
  (list (string "IN VALLEY")
        (string "YOU ARE IN A VALLEY IN THE FOREST BESIDE A STREAM TUMBLING ALONG A\nROCKY BED.\n")))

(defmacro def-room5
  (list (string "IN FOREST")
        (string "YOU ARE IN OPEN FOREST, WITH A DEEP VALLEY TO ONE SIDE.\n")))

(defmacro def-room6
  (list (string "IN FOREST")
        (string "YOU ARE IN OPEN FOREST NEAR BOTH A VALLEY AND A ROAD.\n")))

(defmacro def-room7
  (list (string "AT SLIT IN STREAMBED")
        (string "AT YOUR FEET ALL THE WATER OF THE STREAM SPLASHES INTO A 2-INCH SLIT\nIN THE ROCK.  DOWNSTREAM THE STREAMBED IS BARE ROCK.\n")))

(defmacro def-room8
  (list (string "OUTSIDE GRATE")
        (string "YOU ARE IN A 20-FOOT DEPRESSION FLOORED WITH BARE DIRT.  SET INTO THE\nDIRT IS A STRONG STEEL GRATE MOUNTED IN CONCRETE.  A DRY STREAMBED\nLEADS INTO THE DEPRESSION.\n")))

(defmacro def-room-table
  (list V
	def-room1
        def-room2
        def-room3
        def-room4
        def-room5
        def-room6
        def-room7
        def-room8
	))
(defmacro (lookup-room tbl roomid)
  (car (roomid cdr tbl)))
(defmacro room1 c1)
(defmacro room2 c2)
