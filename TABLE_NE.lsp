; ============================================================
; TABLE_NE.LSP
; Command: TABLE_NE
;
; Create a No. | N | E AutoCAD table from persistent NE_POINT_MARKER data.
;
; Modes:
;   All    - use all valid NE_POINT_MARKER blocks in the drawing
;   Select - user selects specific markers
;
; Contract:
;   Block Name : NE_POINT_MARKER
;   XData App  : SURVEY_NE_DATA
;   XData      : Point No. / Northing / Easting
;
; Coordinate convention:
;   X = Easting (E)
;   Y = Northing (N)
; ============================================================

(vl-load-com)

(setq TNE:BLOCK-NAME "NE_POINT_MARKER")
(setq TNE:APP-NAME   "SURVEY_NE_DATA")

(defun TNE:trim (s)
  (if s (vl-string-trim " \t\r\n\"" s) "")
)

(defun TNE:get-xdata (en / xd appdata vals)
  (setq xd (assoc -3 (entget en (list TNE:APP-NAME))))
  (if xd
    (progn
      (setq appdata (cadr xd))
      (if (= (car appdata) TNE:APP-NAME)
        (progn
          (setq vals (cdr appdata))
          (if
            (and
              (>= (length vals) 3)
              (= (car (nth 0 vals)) 1000)
              (= (car (nth 1 vals)) 1040)
              (= (car (nth 2 vals)) 1040)
            )
            (list
              (cdr (nth 0 vals))
              (cdr (nth 1 vals))
              (cdr (nth 2 vals))
            )
          )
        )
      )
    )
  )
)

; Fallback for legacy marker blocks that may contain NO/N/E attributes.
(defun TNE:get-attributes (en / obj atts no n e tag val)
  (setq obj (vlax-ename->vla-object en))
  (if (= :vlax-true (vla-get-HasAttributes obj))
    (progn
      (setq atts (vlax-invoke obj 'GetAttributes))
      (foreach a atts
        (setq tag (strcase (vla-get-TagString a))
              val (TNE:trim (vla-get-TextString a)))
        (cond
          ((= tag "NO") (setq no val))
          ((= tag "N")  (setq n val))
          ((= tag "E")  (setq e val))
        )
      )
      (if (and no n e)
        (list no (atof n) (atof e))
      )
    )
  )
)

(defun TNE:get-data (en / d)
  (setq d (TNE:get-xdata en))
  (if d d (TNE:get-attributes en))
)

(defun TNE:key (n e)
  (strcat (rtos n 2 3) "|" (rtos e 2 3))
)

(defun TNE:p-number (s / u tail)
  (if s
    (progn
      (setq u (strcase (TNE:trim s)))
      (if (and (> (strlen u) 1) (= (substr u 1 1) "P"))
        (progn
          (setq tail (substr u 2))
          (if (and (/= tail "") (= (itoa (atoi tail)) tail))
            (atoi tail)
          )
        )
      )
    )
  )
)

; Sort P1, P2, ... numerically. Non-P names follow alphabetically.
(defun TNE:row-less (a b / na nb)
  (setq na (TNE:p-number (car a))
        nb (TNE:p-number (car b)))
  (cond
    ((and na nb) (< na nb))
    (na T)
    (nb nil)
    (T (< (strcase (car a)) (strcase (car b))))
  )
)

(defun TNE:collect (ss / i en d key seen rows)
  (setq i 0 seen '() rows '())
  (if ss
    (repeat (sslength ss)
      (setq en (ssname ss i)
            d  (TNE:get-data en))
      (if d
        (progn
          (setq key (TNE:key (nth 1 d) (nth 2 d)))
          ; Keep the first occurrence of a coordinate at 3 decimals.
          (if (not (member key seen))
            (progn
              (setq seen (cons key seen))
              (setq rows
                (cons
                  (list
                    (nth 0 d)
                    (rtos (nth 1 d) 2 3)
                    (rtos (nth 2 d) 2 3)
                  )
                  rows
                )
              )
            )
          )
        )
      )
      (setq i (1+ i))
    )
  )
  (vl-sort rows 'TNE:row-less)
)

(defun TNE:create-table
  (rows ip / doc space tbl th
             title-h header-h data-h
             cw-no cw-ne
             r c lastrow)

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

  ; Compact survey-table proportions.
  (setq
    title-h  (* th 2.0)
    header-h (* th 1.6)
    data-h   (* th 1.5)
    cw-no    (* th 5.0)
    cw-ne    (* th 9.0)
    lastrow  (1+ (length rows))
  )

  ; AutoCAD creates a title row + header row + requested data rows.
  ; Passing (1+ rows) gives: title + header + all data rows.
  (setq tbl
    (vla-AddTable
      space
      (vlax-3d-point ip)
      (+ 2 (length rows))
      3
      data-h
      cw-ne
    )
  )

  ; ----------------------------------------------------------
  ; Column widths
  ; ----------------------------------------------------------
  (vla-SetColumnWidth tbl 0 cw-no)
  (vla-SetColumnWidth tbl 1 cw-ne)
  (vla-SetColumnWidth tbl 2 cw-ne)

  ; ----------------------------------------------------------
  ; Row heights
  ; row 0 = title, row 1 = header, row 2... = data
  ; ----------------------------------------------------------
  (vla-SetRowHeight tbl 0 title-h)
  (vla-SetRowHeight tbl 1 header-h)

  (setq r 2)
  (while (< r (+ 2 (length rows)))
    (vla-SetRowHeight tbl r data-h)
    (setq r (1+ r))
  )

  ; ----------------------------------------------------------
  ; Title + headers
  ; ----------------------------------------------------------
  (vla-SetText tbl 0 0 "COORDINATE TABLE")
  (vla-SetText tbl 1 0 "No.")
  (vla-SetText tbl 1 1 "N")
  (vla-SetText tbl 1 2 "E")

  ; Explicit text heights by row type:
  ; 1 = Title, 2 = Header, 4 = Data
  (vl-catch-all-apply 'vla-SetTextHeight (list tbl 1 (* th 1.10)))
  (vl-catch-all-apply 'vla-SetTextHeight (list tbl 2 th))
  (vl-catch-all-apply 'vla-SetTextHeight (list tbl 4 th))

  ; Center title/header.
  ; acMiddleCenter = 5
  (setq c 0)
  (repeat 3
    (vl-catch-all-apply 'vla-SetCellAlignment (list tbl 0 c 5))
    (vl-catch-all-apply 'vla-SetCellAlignment (list tbl 1 c 5))
    (setq c (1+ c))
  )

  ; ----------------------------------------------------------
  ; Data
  ; ----------------------------------------------------------
  (setq r 2)
  (foreach row rows
    (vla-SetText tbl r 0 (nth 0 row))
    (vla-SetText tbl r 1 (nth 1 row))
    (vla-SetText tbl r 2 (nth 2 row))

    ; Point No. centered.
    (vl-catch-all-apply 'vla-SetCellAlignment (list tbl r 0 5))

    ; Coordinates right aligned where supported.
    ; acMiddleRight = 6
    (vl-catch-all-apply 'vla-SetCellAlignment (list tbl r 1 6))
    (vl-catch-all-apply 'vla-SetCellAlignment (list tbl r 2 6))

    (setq r (1+ r))
  )

  (vla-Update tbl)
  tbl
)

(defun c:TABLE_NE (/ *error* oldc mode ss rows ip filter)
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

  (setq filter
    (list
      '(0 . "INSERT")
      (cons 2 TNE:BLOCK-NAME)
    )
  )

  (initget "All Select")
  (setq mode (getkword "\nNE points [All/Select] <All>: "))
  (if (not mode) (setq mode "All"))

  (cond
    ((= mode "All")
      (setq ss (ssget "_X" filter))
    )
    (T
      (princ "\nSelect NE_POINT_MARKER blocks: ")
      (setq ss (ssget filter))
    )
  )

  (if (not ss)
    (princ "\nNo NE_POINT_MARKER blocks found/selected.")
    (progn
      (setq rows (TNE:collect ss))
      (if (not rows)
        (princ "\nNo valid NE point data found in the selected markers.")
        (progn
          (princ
            (strcat
              "\nFound "
              (itoa (length rows))
              " unique NE point(s)."
            )
          )
          (if (setq ip (getpoint "\nPick table insertion point <Enter to cancel>: "))
            (progn
              (TNE:create-table rows ip)
              (princ
                (strcat
                  "\nTABLE_NE completed: "
                  (itoa (length rows))
                  " row(s)."
                )
              )
            )
            (princ "\nTABLE_NE cancelled; no table created.")
          )
        )
      )
    )
  )

  (*error* nil)
)

(princ "\nTABLE_NE.LSP loaded. Type TABLE_NE to run.")
(princ)
