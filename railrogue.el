;; railrogue -- play railrogue in Emacs
;; came from here: https://www.youtube.com/watch?v=gk39mp8Vy4M
;; YouTube video called 'Writing Games with Emacs'
;; do M-x eval-buffer to load this code into your current emacs session

(defun railrogue ()
  "Start playing railrogue."
  (interactive)
  (switch-to-buffer "railrogue") ;will create it if it doesn't already exist
  (railrogue-mode)
  (railrogue-init))

(define-derived-mode railrogue-mode special-mode "railrogue")

(defun railrogue-init ()
  "Start a new game of railrogue. Makes a row x col board of period chars."
  (setq *railrogue-board* (make-vector (* *railrogue-width*
					  *railrogue-height*)
				       ?\.))
  (railrogue-print-board))

(defvar *railrogue-board* nil
  "The board itself.")

(defconst *railrogue-width* 20
  "The width of the board.")

(defconst *railrogue-height* 10
  "The height of the board.")

(defun railrogue-print-board ()
  (let ((inhibit-read-only t))
    (erase-buffer)
    (dotimes (row *railrogue-height*)
      (dotimes (column *railrogue-width*)
	(insert (railrogue-get-square row column)))
      (insert "\n"))))

(defun railrogue-get-square (row column)
  "Get the value in the (row, column) square."
  (elt *railrogue-board*
       (+ column
	  (* row
	     *railrogue-width*))))

(defun railrogue-set-square (row column value)
  "Set the value in the (row, column) square to value."
  (aset *railrogue-board*
	(+ column
	   (* row
	      *railrogue-width*))
	value))

(defun railrogue-mark ()
  "Mark the current square"
  (interactive)
  (let ((row (1- (line-number-at-pos))) ;line-number-at-pos is not 0-indexed
	(column (current-column)))      ;current-column is
    (railrogue-set-square row column ?\X)
    (railrogue-print-board)))
