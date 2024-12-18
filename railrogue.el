;; railrogue.el -- play railrogue in Emacs
;;
;; UPDATE Dec17/24
;; changed to using gamegrid library, ideas from gamegrid articles
;; at vannilla.org/write (yes, that's 2 letter n's in vannilla)
;;
;; original concept from here: https://www.youtube.com/watch?v=gk39mp8Vy4M
;; YouTube video called 'Writing Games with Emacs'
;;
;; do M-x eval-buffer to load this code into your current emacs session

(require 'gamegrid)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; define our constants (and the occasional variable)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; this variable will hold the buffer that gamegrid needs to run the game
; we also need to check that it's the current buffer as most gamegrid
; functions use buffer-local variables
(defconst railrogue-buffer-name "Railrogue")
(defconst railrogue-tick 0.5
  "Time interval between updates.")
(defconst railrogue-buffer-width 16)
(defconst railrogue-buffer-height 16)
(defconst railrogue-empty 0) ; game display entity 1 of 4
(defconst railrogue-ground 1) ; was floor in article example - game display entity 2 of 4
(defconst railrogue-track 2) ; was wall in article example - game display entity 3 of 4
(defconst railrogue-score-file-name "railrogue-game-scores") ; score not used right now but included
(defvar railrogue-score 0) ; defvar because this changes
(defvar railrogue-paused nil)
(defconst railrogue-player 3) ; game display entity 4 of 4 - 3 is the player index in the 256 posn vector
(defvar railrogue-player-x 4)
(defvar railrogue-player-y 5)
(defconst railrogue-empty-options ; what to display in grid if no display entity found
  '(((t 32)) ; 32 is space character, we need a 3 element list
    nil
    nil))
(defconst railrogue-ground-options
  '(((glyph colorize)
     (t 32))
    ((color-x color-x)
     (mono-x mono-x)
     (color-tty color-tty))
    (((glyph color-x) [0 0 0])
     (color-tty "black"))))
(defconst railrogue-track-options
  '(((glyph colorize)
     (t ?+))
    ((color-x color-x)
     (mono-x mono-x)
     (color-tty color-tty))
    (((glyph color-x) [0.5 0.5 0.5])
     (color-tty "gray"))))
(defconst railrogue-player-options
  '(((glyph colorize)
     (t ?+))
    ((color-x color-x)
     (mono-x mono-x)
     (color-tty color-tty))
    (((glyph color-x) [0.5 0.5 0.5])
     (color-tty "yellow"))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; main functions to start the game, and update the game
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; we need an interactive function to start our game
; this function will be what the user calls using 'M-x railrogue'
(defun railrogue ()
  "Start playing railrogue. Play instructions should go here."
  (interactive)
  (switch-to-buffer railrogue-buffer-name) ; create it if it doesn't already exist
  (gamegrid-kill-timer) ; stop the game loop, resets the game (starts new game)
  (railrogue-mode) ; set up keybindings and other features
  (railrogue-start-game)) ; start the game

(defun railrogue-update-game (buffer)
  "Update the game.
BUFFER is the buffer in which this function has been called.
It should be `railrogue-buffer-name'."
  (unless (or railrogue-paused
              (not (string= (buffer-name buffer)
                            railrogue-buffer-name))
              (null railrogue-update-list)) ; only pop an element if these are all not true
    (let ((action (pop railrogue-update-list)))
      (let ((nx (+ railrogue-player-x (car action)))
            (ny (+ railrogue-player-y (cdr action))))
        (unless (= (gamegrid-get-cell nx ny) railrogue-track)
          (gamegrid-set-cell railrogue-player-x
                             railrogue-player-y
                             railrogue-ground)
          (gamegrid-set-cell nx ny railrogue-player)
          (setq railrogue-player-x nx
                railrogue-player-y ny
                railrogue-player-moved nil))))))

; this is where we define our new mode, and we derive it using this special function
; and the special param called special-mode, which is made for doing exactly this type
; of thing (creating a new mode) and it creates a buffer that you can't edit in, among
; a few other things, which remains for us to lookup and discover. The final param
; is what you want to see in the modeline, in our case the string "railrogue"
(define-derived-mode railrogue-mode special-mode "railrogue"
  (add-hook 'kill-buffer-hook #'gamegrid-kill-timer nil t) ; essential to kill game if buffer killed
  (use-local-map railrogue-null-map) ; special keymap used before starting game, and when game ends
  (gamegrid-init (railrogue-display-options))) ; initialize the display, rather than the library itself

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; define our keymaps
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar railrogue-null-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "q") 'bury-buffer) ; moves buffer to bottom of buffer list
    (define-key map (kbd "n") 'railrogue-start-game)
    map)
  "Railrogue's null menu keymap.")

(defvar railrogue-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "q") 'railrogue-end-game) ; note, not bury-buffer any longer
    (define-key map (kbd "n") 'railrogue-start-game)
    (define-key map (kbd "p") 'railrogue-pause-game)
    (define-key map (kbd "a") 'railrogue-move-left)
    (define-key map (kbd "s") 'railrogue-move-down)
    (define-key map (kbd "d") 'railrogue-move-right)
    (define-key map (kbd "w") 'railrogue-move-up)
    map)
  "Railrogue's in-game menu keymap.")

  ;; (define-key railrogue-mode-map (kbd "SPC") 'railrogue-mark)
  ;; (define-key railrogue-mode-map (kbd "r") 'railrogue-reset)
  ;; (define-key railrogue-mode-map (kbd "C-c s") 'railrogue-set-current-item))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; utility functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; reset the game to its initial state, stop the game loop, set up the proper keymap,
; then start the game loop again
(defun railrogue-start-game ()
  "Start a new game."
  (interactive)
  (unless (string= (buffer-name (current-buffer)) railrogue-buffer-name)
    (error "To start a new game, switch to the `railrogue-buffer-name' buffer."))
  (railrogue-reset-game) ; useful if we need long or complex steps to reset game state
  (use-local-map railrogue-mode-map) ; change to the keymap with all the bindings
  (gamegrid-start-timer railrogue-tick #'railrogue-update-game))

(defun railrogue-reset-game ()
  "Reset the game (bare minimum for now)."
  (gamegrid-kill-timer)
  (railrogue-init-buffer))

(defun railrogue-end-game ()
  "End the current game."
  (interactive)
  (gamegrid-kill-timer)
  (use-local-map railrogue-null-map)
  (gamegrid-add-score railrogue-score-file-name railrogue-score))

(defun railrogue-init-buffer ()
  "Initialize the buffer. buffer-read-only is bound to nil because the game's major mode
is derived from special-mode, which by default make the buffer read-only. (0,0) is the top
left corner of the grid."
  (gamegrid-init-buffer railrogue-buffer-width
			railrogue-buffer-height
			railrogue-empty)
  (let ((buffer-read-only nil))
    (dotimes (y railrogue-buffer-height)
      (dotimes (x railrogue-buffer-width)
	(gamegrid-set-cell x y railrogue-track))) ; place the track
    (let ((y 1) ; start at first row
	  (wmax (1- railrogue-buffer-width)) ; set max col width
	  (hmax (1- railrogue-buffer-height))) ; set max row height
      (while (< y hmax)
	(let ((x 1))
	  (while (< x wmax)
	    (gamegrid-set-cell x y railrogue-ground) ; place the ground
	    (setq x (1+ x))))
	(setq y (1+ y)))))
  (gamegrid-set-cell railrogue-player-x
		     railrogue-player-y
		     railrogue-player))

(defun railrogue-pause-game ()
  "Pause the game."
  (interactive)
  (if railrogue-paused
      (setq railrogue-paused nil)
    (setq railrogue-paused t)))

(defun railrogue-display-options ()
  "Return a vector with display information."
  (let ((vec (make-vector 256 nil)))
    (dotimes (c 256)
      (aset vec c
            (cond ((= c railrogue-empty)
                   railrogue-empty-options) ; -options will support multiple configs
                  ((= c railrogue-ground)
                   railrogue-ground-options)
                  ((= c railrogue-track)
                   railrogue-track-options)
                  ((= c railrogue-player)
                   railrogue-player-options)
                  (t
                   '(nil nil nil))))) ; gives a hint as to the contents of -options vars (3 elements)
    vec))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; player input functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar railrogue-update-list ())
(defvar railrogue-player-moved nil) ; flag needed to stop infinite conses being pushed
; onto railrogue-update-list if user presses and holds down keys

(defun railrogue-move-left ()
  "Move the player left."
  (interactive)
  (unless railrogue-player-moved
    (push (cons -1 0) railrogue-update-list))
    (setq railrogue-player-moved t))

(defun railrogue-move-down ()
  "Move the player down."
  (interactive)
  (unless railrogue-player-moved
    (push (cons 0 1) railrogue-update-list))
    (setq railrogue-player-moved t))

(defun railrogue-move-right ()
  "Move the player right."
  (interactive)
  (unless railrogue-player-moved
    (push (cons 1 0) railrogue-update-list))
    (setq railrogue-player-moved t))

(defun railrogue-move-up ()
  "Move the player up."
  (interactive)
  (unless railrogue-player-moved
    (push (cons 0 -1) railrogue-update-list))
  (setq railrogue-player-moved t))

;; everything below here is old, but saved in case we need to reuse some of the code
;; or ideas in the next iteration (where we change the gamegrid logic to be more
;; railroad-like)

(defvar *railrogue-board* nil
  "The board itself.")

(defconst *railrogue-width* 20
  "The width of the board.")

(defconst *railrogue-height* 10
  "The height of the board.")

(defconst *railrogue-fill-char* ?\.
  "The default char used to fill the board on creation.")

(defconst *railrogue-track* ?\=
  "The char for a section of track.")

(defconst *railrogue-engine-eastbound* ?\>
  "The char for an east-facing engine.")

(defconst *railrogue-engine-westbound* ?\<
  "The char for a west-facing engine.")

; instead of current player in tic-tac-toe, we're going to set current item
; which will be things like track, switch, engine, railcar, etc.
(defun railrogue-set-current-item (value)
  "Set value to the current character."
  (interactive "sEnter a char to use as current item: ")
  (setq *railrogue-current-item* value)
  (message "Item now set to: %s" value))
