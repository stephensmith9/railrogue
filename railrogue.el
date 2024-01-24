;; railrogue.el -- play railrogue in Emacs
;; came from here: https://www.youtube.com/watch?v=gk39mp8Vy4M
;; YouTube video called 'Writing Games with Emacs'
;; do M-x eval-buffer to load this code into your current emacs session

; this function will be what the user calls using 'M-x railrogue'
(defun railrogue ()
  "Start playing railrogue."
  (interactive)
  (switch-to-buffer "railrogue") ;will create it if it doesn't already exist
  (railrogue-mode) ; go into our new mode
  (railrogue-init)) ; then init it

; this is where we define our new mode, and we derive it using this special function
; and the special param called special-mode, which is made for doing exactly this type
; of thing (creating a new mode) and it creates a buffer that you can't type in, among
; a few other things, which remains for us to lookup and discover. The final param
; is what you want to see in the modeline, in our case the string "railrogue"
(define-derived-mode railrogue-mode special-mode "railrogue"
  (define-key railrogue-mode-map (kbd "SPC") 'railrogue-mark)
  (define-key railrogue-mode-map (kbd "r") 'railrogue-reset)
  (define-key railrogue-mode-map (kbd "C-c s") 'railrogue-set-current-item))

(defvar *railrogue-board* nil
  "The board itself.")

(defconst *railrogue-width* 20
  "The width of the board.")

(defconst *railrogue-height* 10
  "The height of the board.")

(defconst *railrogue-track* ?\=
  "The char for a section of track.")

(defconst *railrogue-engine-eastbound* ?\>
  "The char for an east-facing engine.")

(defconst *railrogue-engine-westbound* ?\<
  "The char for a west-facing engine.")

(defun railrogue-init ()
  "Start a new game of railrogue."
  (railrogue-fill-board ?\.)
  (railrogue-print-board)
  (setq *railrogue-current-item* ?\=))

(defun railrogue-print-board ()
  "Write the board to the buffer, based on all the current values for each square."
  (let ((inhibit-read-only t)) ; remember that the new mode statement put us in read-only mode by default
    (erase-buffer) ; clear out anything already there, like a previous board
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

; this function gets created at the 20min mark in the youtube video and
; we stopped at 23:00 basically the first time we wrote this script
(defun railrogue-mark ()
  "Mark the current square"
  (interactive) ; add this because the user will call this function
  (let ((row (1- (line-number-at-pos))) ; line-number-at-pos is not 0-indexed
	(column (current-column)))      ; current-column is
    (railrogue-set-square row column *railrogue-current-item*)
    (setq my-posn (point)) ; save our posn so we can return here after the board is redrawn
    (railrogue-print-board)
    (goto-char my-posn)))

(defun railrogue-fill-board (value)
  "Fill the board with a single value."
  (setq *railrogue-board* (make-vector (* *railrogue-width*
					  *railrogue-height*)
				       value)))

(defun railrogue-reset ()
  "Reset all squares on the board."
  (interactive) ; user will call this
  (railrogue-fill-board ?\.)
  (railrogue-print-board))
  
; instead of current player in tic-tac-toe, we're going to set current item
; which will be things like track, switch, engine, railcar, etc.
(defun railrogue-set-current-item (value)
  "Set value to the current character."
  (interactive "sEnter a char to use as current item: ")
  (setq *railrogue-current-item* value)
  (message "Item now set to: %s" value))
