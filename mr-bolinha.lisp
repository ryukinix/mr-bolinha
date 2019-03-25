;;;; mr-bolinha.lisp
;;;; trivial-gamekit guide https://borodust.org/projects/trivial-gamekit/manual/

(in-package #:mr-bolinha)

(defconstant +screen-width+ 800)
(defconstant +screen-height+ 600)
(defparameter *score-table* nil)

;; These get updated when the game loops
(defparameter *mouse-x* 0)
(defparameter *mouse-y* 0)
(defparameter *radius* 32)
(defconstant TAU (* 2.0 PI))
;; We'll assume a very simple game, tear these out if you're storing score
;; on a per instance basis (remove em from the draw loop too)
(defparameter *score* 0)
(defparameter *lives* 3)
(defparameter *numero-de-bolinhas* 200)
(defparameter *bolinhas* nil)

(defun center-x ()
  (/ +screen-width+
     2.0))

(defun center-y ()
  (/ +screen-height+
     2.0))

(defun clear-scores (&optional(table-size 10))
  (setf *score-table* (loop
			            :for i :from 0 :below table-size
			            :collecting (cons "---" 0))))

(defun add-score (name score)
  "Adds a score if it's high enough, does nothing if not"
  (setf *score-table* (append *score-table* (list (cons name score))))
  (setf *score-table* (stable-sort *score-table* #'> :key #'cdr))
  (setf *score-table* (butlast *score-table*)))

(gamekit:defgame example () ()
                 (:viewport-width +screen-width+)
                 (:viewport-height +screen-height+))

;; We'll store user button presses in a key bag each cycle
(defvar *key-bag* nil)

(defun bind-movement-button (button)
  "This function updates the key-bag each game cycle"
  (gamekit:bind-button button :pressed ;Add each cycle
		               (lambda ()
			             (push button *key-bag*)))
  (gamekit:bind-button button :released ;Clean up each cycle
		               (lambda ()
			             (setf *key-bag* (delete button *key-bag*))))

  (gamekit:bind-cursor (lambda (x y)
			             (setf *mouse-x* x)
			             (setf *mouse-y* y))))

(defun key-down (key)
  "High level key checking function"
  (member key *key-bag*))

(defun reset-game ()
  "Save scores, reset game, reposition instances"
  (setf *score* 0)
  (setf *lives* 3))

(defmethod gamekit:post-initialize ((app example))
  "Redefines post-initialize to set up our *key-bag*"
  ;; load scores would go here ...
  (clear-scores)
  (loop :for key :in '(:space :left :right :up :down :w :a :s :d :mouse-left)
        :do (bind-movement-button key))
  (loop :for key :in '(:escape :q)
        :do (gamekit:bind-button key :released #'gamekit:stop))
  (reset-game))

(defmethod gamekit:act ((this example))
  "We redefine the act method here, all game events get called from here"
  (when (key-down :mouse-left) (incf *lives* 1)))

(defun ty (y)
  "This transforms the y coordinate into a traditional top = 0 bottom = height"
  (- +screen-height+ y))

(defun gerar-bolinhas ()
  "GERAR-BOLINHAS gera um conjunto aleatório de bolinhas"
  (loop repeat *numero-de-bolinhas*
        collect (list (gamekit:vec2 (random +screen-width+)
                                    (random +screen-height+))
                      (+ 2 (random 3))
                      :fill-paint (gamekit:vec4 0 0 0 1))))


(defun desenhar-bolinhas (bolinhas)
  "RENDERIZA bolinhas"
  (mapcar (lambda (bolinha)
            (apply #'draw-circle bolinha))
          bolinhas))

(defun square (x)
  "SQUARE calcula o quadrado de X"
  (* x x))

(defun distancia-euclidiana (p1 p2)
  "DISTANCIA-EUCLIDANA P1 e P2 são gamekit:vec2"
  (let ((x1 (gamekit:x p1))
        (x2 (gamekit:x p2))
        (y1 (gamekit:y p1))
        (y2 (gamekit:y p2)))
    (sqrt (+ (square (- x1 x2))
             (square (- y1 y2))))))

(defun interseccao-de-bolinhas (bolinha1 bolinha2)
  "INTERSECCAO-DE-BOLINHAS verifica se há intersecção entre BOLINHA1 e BOLINHA2"
  (let* ((p1 (car bolinha1))
         (p2 (car bolinha2))
         (r1 (second bolinha1))
         (r2 (second bolinha2))
         (d (distancia-euclidiana p1 p2)))
    (<= d (+ r1 r2))))

(defun comedor-de-bolinhas (player bolinhas)
  "COMEDOR-DE-BOLINHAS deleta as BOLINHAS que possuem intersecção com PLAYER"
  (if (key-down :mouse-left)
      (remove-if
       (lambda (bolinha)
         (when (interseccao-de-bolinhas player bolinha)
           (prog1 t
             (setq *radius* (+ *radius*
                               (* 0.1 (second bolinha)))))))
       bolinhas)
      bolinhas))

(defmethod gamekit:draw ((this example))
  "We redefine the draw method here. YOU ONLY CALL DRAWING FROM HERE"
  (defparameter *circle-color* '(0 0 1 1))

  (when (key-down :d)
    (setq *circle-color* '(0 0 1 1))
    (decf *radius*))

  (when (key-down :mouse-left)
    (setq *circle-color* '(1 0 0 1)))

  (defparameter *player*
    (list (gamekit:vec2 *mouse-x* *mouse-y*)
	      *radius*
	      :fill-paint (apply #'gamekit:vec4 *circle-color*)
	      :stroke-paint (gamekit:vec4 0 .3 1 1)))

  ;; *bolinhas*
  ;; *player*
  (desenhar-bolinhas *bolinhas*)
  (setq *bolinhas*
        (comedor-de-bolinhas *player* *bolinhas*))
  ;; comer bolinhas
  ;; as bolinhas só podem ser comidas quando apertar o botão
  ;; as bolinhas pretas tem que estar da azul (do jogador)

  (gamekit:draw-text "MR BOLINHA" (gamekit:vec2 0 (ty 24)))
  (apply #'gamekit:draw-circle *player*))

(defun main ()
  ""
  (setq *bolinhas* (gerar-bolinhas))
  (gamekit:start 'example))
