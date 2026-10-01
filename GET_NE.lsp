; ============================================================
; GET_NE.LSP
; Command: GET_NE
;
; Workflow:
;   1. Pick multiple coordinate points in the required order.
;   2. Each point creates NE_POINT_MARKER + visible No/N/E label.
;   3. Press Enter when finished.
;   4. Pick a table insertion point.
;   5. A No | N | E AutoCAD table is created for this GET_NE run.
;
; Contract preserved for TABLE_NE.LSP:
;   Block Name : NE_POINT_MARKER
;   XData App  : SURVEY_NE_DATA
;   XData      : Point No. / Northing / Easting
;
; Coordinate convention:
;   X = Easting (E)
;   Y = Northing (N)
; ============================================================

(vl-load-com)

(setq NE:BLOCK-NAME "NE_POINT_MARKER")
(setq NE:APP-NAME   "SURVEY_NE_DATA")

(defun NE:trim (s)
  (if s (vl-string-trim " \t\r\n\"" s) "")
)

(defun NE:layer (name color)
  (if (not (tblsearch "LAYER" name))
    (entmakex
      (list
        '(0 . "LAYER")
        '(100 . "AcDbSymbolTableRecord")
        '(100 . "AcDbLayerTableRecord")
        (cons 2 name)
        '(70 . 0)
        (cons 62 color)
        '(6 . "Continuous")
      )
    )
  )
  name
)

(defun NE:ensure-regapp ()
  (if (not (tblsearch "APPID" NE:APP-NAME))
    (regapp NE:APP-NAME)
  )
)

(defun NE:ensure-block ()
  (if (not (tblsearch "BLOCK" NE:BLOCK-NAME))
    (progn
      (entmakex
        (list
          '(0 . "BLOCK")
          '(100 . "AcDbEntity")
          '(8 . "0")
          '(100 . "AcDbBlockBegin")
          (cons 2 NE:BLOCK-NAME)
          '(70 . 0)
          '(10 0.0 0.0 0.0)
        )
      )
      (entmakex
        (list
          '(0 . "CIRCLE")
          '(100 . "AcDbEntity")
          '(8 . "0")
          '(100 . "AcDbCircle")
          '(10 0.0 0.0 0.0)
          '(40 . 1.0)
        )
      )
      (entmakex
        (list
          '(0 . "LINE")
          '(100 . "AcDbEntity")
          '(8 . "0")
          '(100 . "AcDbLine")
          '(10 -1.35 0.0 0.0)
          '(11 1.35 0.0 0.0)
        )
      )
      (entmakex
        (list
          '(0 . "LINE")
          '(100 . "AcDbEntity")
          '(8 . "0")
          '(100 . "AcDbLine")
          '(10 0.0 -1.35 0.0)
          '(11 0.0 1.35 0.0)
        )
      )
      (entmakex
        (list
          '(0 . "ENDBLK")
          '(100 . "AcDbEntity")
          '(8 . "0")
          '(100 . "AcDbBlockEnd")
        )
      )
    )
  )
)

(defun NE:put-xdata (en no n e / ed)
  (NE:ensure-regapp)
  (setq ed (entget en '("*")))
  (entmod
    (append
      ed
      (list
        (list
          -3
          (list
            NE:APP-NAME
            (cons 1000 no)
            (cons 1040 n)
            (cons 1040 e)
          )
        )
      )
    )
  )
  (entupd en)
)

(defun NE:insert-marker (pt no n e scale layer / en)
  (NE:ensure-block)
  (setq en
    (entmakex
      (list
        '(0 . "INSERT")
        '(100 . "AcDbEntity")
        (cons 8 layer)
        '(100 . "AcDbBlockReference")
        (cons 2 NE:BLOCK-NAME)
        (cons 10 pt)
        (cons 41 scale)
        (cons 42 scale)
        (cons 43 scale)
      )
    )
  )
  (if en (NE:put-xdata en no n e))
  en
)

(defun NE:mtext (pt txt h layer)
  (entmakex
    (list
      '(0 . "MTEXT")
      '(100 . "AcDbEntity")
      (cons 8 layer)
      '(100 . "AcDbMText")
      (cons 10 pt)
      (cons 40 h)
      (cons 41 (* h 18.0))
      '(71 . 1)
      '(72 . 1)
      (cons 1 txt)
      (cons 7 (getvar "TEXTSTYLE"))
    )
  )
)

(defun NE:line (p1 p2 layer)
  (entmakex
    (list
      '(0 . "LINE")
      '(100 . "AcDbEntity")
      (cons 8 layer)
      '(100 . "AcDbLine")
      (cons 10 p1)
      (cons 11 p2)
    )
  )
)

; Create the same No | N | E table structure used by TABLE_NE.
; rows = ((No N-string E-string) ...)
(defun NE:create-table (rows ip / doc space tbl th rh cw-no cw-ne r)
  (setq
    ip  (trans ip 1 0)
    doc (vla-get-ActiveDocument (vlax-get-acad-object))
    space
      (if (= 1 (getvar "CVPORT"))
        (vla-get-PaperSpace doc)
        (vla-get-ModelSpace doc)
      )
    th (getvar "TEXTSIZE")
  )

  (if (or (not th) (<= th 0.0))
    (setq th 2.5)
  )

  (setq
    rh    (* th 1.8)
    cw-no (* th 6.0)
    cw-ne (* th 10.0)
  )

  (setq tbl
    (vla-AddTable
      space
      (vlax-3d-point ip)
      (1+ (length rows))
      3
      rh
      cw-ne
    )
  )

  (vla-SetColumnWidth tbl 0 cw-no)
  (vla-SetColumnWidth tbl 1 cw-ne)
  (vla-SetColumnWidth tbl 2 cw-ne)

  (vla-SetText tbl 0 0 "No.")
  (vla-SetText tbl 0 1 "N")
  (vla-SetText tbl 0 2 "E")

  (setq r 1)
  (foreach row rows
    (vla-SetText tbl r 0 (nth 0 row))
    (vla-SetText tbl r 1 (nth 1 row))
    (vla-SetText tbl r 2 (nth 2 row))
    (setq r (1+ r))
  )

  (vla-Update tbl)
  tbl
)


; Return point number from SURVEY_NE_DATA XData, or nil.
(defun NE:get-point-no (en / xd appdata item)
  (setq xd (assoc -3 (entget en (list NE:APP-NAME))))
  (if xd
    (progn
      (setq appdata (cadr xd))
      (if (= (car appdata) NE:APP-NAME)
        (progn
          (setq item (cadr appdata))
          (if (= (car item) 1000) (cdr item))
        )
      )
    )
  )
)

; Parse P<number> only. Returns integer or nil.
(defun NE:p-number (s / u tail)
  (if s
    (progn
      (setq u (strcase (NE:trim s)))
      (if (and (> (strlen u) 1) (= (substr u 1 1) "P"))
        (progn
          (setq tail (substr u 2))
          (if (and (/= tail "") (not (vl-string-search "." tail)))
            (if (= (itoa (atoi tail)) tail)
              (atoi tail)
            )
          )
        )
      )
    )
  )
)

; Scan the drawing for existing NE_POINT_MARKER blocks and return max P number.
(defun NE:max-existing-p (/ ss i en no n maxp)
  (setq maxp 0)
  (if
    (setq ss
      (ssget "_X"
        (list
          '(0 . "INSERT")
          (cons 2 NE:BLOCK-NAME)
        )
      )
    )
    (progn
      (setq i 0)
      (repeat (sslength ss)
        (setq en (ssname ss i)
              no (NE:get-point-no en)
              n  (NE:p-number no))
        (if (and n (> n maxp)) (setq maxp n))
        (setq i (1+ i))
      )
    )
  )
  maxp
)

(defun c:GET_NE
  (/ *error* oldc pt no def ms th off ml tl tp le txt nextp count)

  (defun *error* (msg)
    (if oldc (setvar "CMDECHO" oldc))
    (if
      (and msg
           (/= msg "Function cancelled")
           (/= msg "quit / exit abort"))
      (princ (strcat "\nError: " msg))
    )
    (princ)
  )

  (setq oldc (getvar "CMDECHO"))
  (setvar "CMDECHO" 0)

  ; Settings once per run.
  (initget 6)
  (setq ms (getreal "\nMarker size <0.25>: "))
  (if (not ms) (setq ms 0.25))

  (initget 6)
  (setq th (getreal "\nText height <0.25>: "))
  (if (not th) (setq th 0.25))

  (initget 6)
  (setq off (getreal "\nText offset <1.00>: "))
  (if (not off) (setq off 1.00))

  (setq
    ml (NE:layer "CHECK_NE_MARKER" 3)
    tl (NE:layer "CHECK_NE_TEXT" 2)
    count 0
    nextp (1+ (NE:max-existing-p))
  )

  (princ
    (strcat
      "\nExisting NE markers scanned. Next default point is P"
      (itoa nextp)
      "."
    )
  )
  (princ "\nPick NE points one by one. Press Enter anytime to pause/exit GET_NE.")

  (while
    (setq pt
      (getpoint
        (strcat
          "\nPick coordinate point <Enter to pause>"
          (if (> count 0)
            (strcat " [" (itoa count) " created this run]")
            "")
          ": "
        )
      )
    )

    ; UCS -> WCS
    (setq pt (trans pt 1 0))

    ; Default point number is derived from markers already in the drawing.
    (setq
      def (strcat "P" (itoa nextp))
      no  (getstring T (strcat "\nPoint No <" def ">: "))
    )
    (if (= (NE:trim no) "")
      (setq no def)
      (setq no (NE:trim no))
    )

    ; Create marker + persistent XData.
    (NE:insert-marker pt no (cadr pt) (car pt) ms ml)

    ; Visible leader + label.
    (setq
      tp (list (+ (car pt) off) (+ (cadr pt) off) 0.0)
      le (list (- (car tp) (* th 0.30))
               (- (cadr tp) (* th 0.30))
               0.0)
      txt (strcat
            "No. " no
            "\\PN= " (rtos (cadr pt) 2 3)
            "\\PE= " (rtos (car pt) 2 3))
    )
    (NE:line pt le tl)
    (NE:mtext tp txt th tl)

    (setq count (1+ count))

    ; Re-scan after every creation. This also behaves correctly if the user
    ; manually enters P20, or if another existing P number is already present.
    (setq nextp (1+ (NE:max-existing-p)))

    (princ
      (strcat
        "\nNo. " no
        "   N = " (rtos (cadr pt) 2 3)
        "   E = " (rtos (car pt) 2 3)
      )
    )
  )

  (princ
    (strcat
      "\nGET_NE paused. "
      (itoa count)
      " point(s) created this run. "
      "Run GET_NE again to continue; use TABLE_NE when ready to make a table."
    )
  )

  (*error* nil)
)

(princ "\nGET_NE.LSP loaded. Type GET_NE to run.")
(princ)
