;;									0		1		2		3		4 		5
(defparameter *board* '((0 0 0) ((1 1 1) (1 1 1) (1 1 1) (1 1 1) (1 1 1) (1 1 1)
								(1 1 1) (1 1 1) (1 1 1) (1 1 1) (1 1 1) (1 1 1)) (0 0 0)))
;;									6		7		8		9		10		11
(defparameter *ai-score* 0) ;; partidas ganadas por la IA
(defparameter *player-score* 0) ;; partidas ganadas por el jugador
(defparameter *limit* 1) ;; limite en la busqueda a la profundo
(defparameter *current-turn* 1) ;;1 player o 2 AI
(defparameter *finish* nil)
(defparameter *winner* 0)

(defun isHoleEmpty? (board num)
	"Dice si el hoyo esta vacio"
	(loop for h in (nth num (second board)) always (= h 0)))

(defun getMarbles (board num) 
	"Obtiene el total de canicas en el hoyo"
	(loop for h in (nth num (second board)) sum h))

(defun insert-after (lst index newelt) 
	(push newelt (cdr (nthcdr index lst))) 
	lst)

(defun generateMoves (board hole turn)
	"Genera una lista con posibles movimientos"
	(let ((moves '()) (marbles (getMarbles board hole)) )
			(loop for i from (+ hole 1) to (+ hole marbles )
					collect (mod i 12) into c
					finally (setf moves c)) 
			(when (= turn 1)
					(when (>= (+ hole marbles) 6)
							(insert-after moves (abs (- (+ hole marbles) 6 1)) -1 )
							(setf moves (reverse (rest (reverse moves)))) ))
			(when (= turn 2)
					(when (>= (+ hole marbles) 12)
							(insert-after moves (abs (- (+ hole marbles) 12 1)) -2 )
							(setf moves (reverse (rest (reverse moves)))) )) 
			moves ))

;(defun move (board from type to)
;	"Mueve canicas [type] 0,1 y 2" 
;	(setq result nil)
;	(loop for s in board
;		collect s into copy
;		finally (setf result copy))
;	(let ((hole1 (nth from result)) (hole2 (nth to result)))
;		(setf (nth type hole1) (- (nth type hole1) 1))
;		(setf (nth type hole2) (+ (nth type hole2) 1)))
;	result )

(defun move (board movements hole)
	"Mueve las canicas"
	(when (not (null movements)) 
		(format t "Selecciona el color de canica que deseas mover a la casilla ~A:~&Opciones:~&(0) Azul = ~A~&(1) Verde = ~A~&(2) Rojo = ~A~&" 
			(if (< (first movements) 0) "GOAL" (first movements) ) 
			(nth 0 (nth hole (second board))) 
			(nth 1 (nth hole (second board))) 
			(nth 2 (nth hole (second board))))
		(setf type (read))
		(cond ((or (< type 0) (> type 2))
				(format t "~&Opcion invalida~&")
				(move board movements hole))
				((<= (nth type (nth hole (second board))) 0) 
					(format t "~&Insuficientes canicas~&")
					(move board movements hole) )
				((= (first movements) -1)
						(setf (nth type (first board)) (+ 1 (nth type (first board))) )
						(setf (nth type (nth hole (second board))) (- (nth type (nth hole (second board))) 1) )
						(move board (rest movements) hole))
				((= (first movements) -2) 
						(setf (nth type (nth 2 board)) (+ 1 (nth type (nth 2 board))) )
						(setf (nth type (nth hole (second board))) (- (nth type (nth hole (second board))) 1) )
						(move board (rest movements) hole))
				(T 	(setf (nth type (nth (first movements) (second board))) 
							(+ 1 (nth type (nth (first movements) (second board)))) )
					(setf (nth type (nth hole (second board))) 
							(- (nth type (nth hole (second board))) 1) )
					(move board (rest movements) hole)) ) ))

(defun isGameOver? (board)
	"revisa si ya se acabo el juego"
	(loop for h in (nth 1 board)
		with over = nil
		do (loop for c in h 
				do (print c)
				if (= c 1)
					do (return (setf over nil))
				do (setf over T))
		when (null over)
			do (return over)
		finally (return over) ))



(defun ask-box ()
	"Pide que selecciones una casilla"
	(format t "Selecciona una casilla (0 al 5):~&")
	(setq selected-box (read))
	(when (or (< selected-box 0) (> selected-box 5) (isHoleEmpty? *board* selected-box))
		(format t "Error no contiene canicas:~&")
		(ask-box))
	selected-box)

(defun human-turn ()
	"Movimiento del jugador"
	(let* ((selected-hole (ask-box)) 
			;(total-marbles (getMarbles *board* selected-hole)) 
			(movements (generateMoves *board* selected-hole 1)))
			(move *board* movements selected-hole)
			(when (not (= (first (last movements)) -1))
					(setf *current-turn* 2) ))
	;(setq selected-hole (ask-box))
	;(setq total-marbles (getMarbles *board* selected-hole))
	())

(defun Agent-turn ()
	"Movimiento de la Agente"
	(setf *finish* T))

(defun gameloop () 
	"El ciclo de juego"
	(display-board)
	(if (= *current-turn* 1)
		(human-turn)
		(Agent-turn))
	;(setf *finish* (isGameOver? *board*))
	(when (not *finish*)
			(gameloop) ))

(defun getHole (x)
	"Devuelve la lista de canicas del hoyo"
	(nth x (nth 1 *board*)) )

(defun display-board () 
	"Despliega el estado actual del tablero"
	(format t "~&====================================================================~&")
	(format t "Azul = A 	Verde = V 		Rojo = R")
	(format t "~&====================================================================~&")
	(format t "~&===========(5)======(4)======(3)======(2)======(1)======(0)=========~&")
	(format t "~&= A ~A == A V R == A V R == A V R == A V R == A V R == A V R == ~A A =~&" 
		(first (first *board*)) (first (nth 2 *board*)) )
	(format t "~&=     == ~{~A ~}== ~{~A ~}== ~{~A ~}== ~{~A ~}== ~{~A ~}== ~{~A ~}==     =~&" 
		(getHole 5) (getHole 4) (getHole 3) (getHole 2) (getHole 1) (getHole 0))
	(format t "~&= V ~A ======================================================== ~A V =~&" 
		(second (first *board*)) (second (nth 2 *board*)))
	(format t "~&=     == A V R == A V R == A V R == A V R == A V R == A V R ==     =~&")
	(format t "~&= R ~A == ~{~A ~}== ~{~A ~}== ~{~A ~}== ~{~A ~}== ~{~A ~}== ~{~A ~}== ~A R =~&" 
		(nth 2 (first *board*)) (getHole 6) (getHole 7) (getHole 8) (getHole 9) (getHole 10) (getHole 11) (nth 2 (nth 2 *board*)))
	(format t "~&==========(6)======(7)======(8)======(9)======(10)=====(11)=========~&")
	(format t "~&====================================================================~&")
	(format t "Puntaje:~%")
	(format t "        Azul = 1 punto ~%        Verde = 5 puntos ~%        Rojo = 10 puntos")
	(format t "~&====================================================================~&"))


(defun start-game ()
	"Empieza el juego"
	(gameloop))

(defun select-level ()
	"Selecciona un nivel"
	(request-level)
	(start-game))

(defun request-level ()
	"Pide el nivel maximo de profundidad de la IA"
	(format t "~&Ingresa el nivel de dificultad (Limite de profundidad > 0):~&")
	(setf *limit* (read))
	(when (<= *limit* 0)
			(request-level)))

(defun display-rules () 
	"Despliega las reglas"
	(format t "~%============================= Reglas =============================~%")
	(format t "1) En su turno, cada jugador elige alguna de sus casillas,~%")
	(format t "   remueve TODAS las fichas en esa casilla y las reparte,~%")
	(format t "   una en cada una de las siguientes casillas, exceptuando~%")
	(format t "   la base del oponente.~%")
	(format t "2) Si la última ficha movida cae en la base del jugador en~%")
	(format t "   turno, entonces ese mismo jugador vuelve a tirar, de lo~%")
	(format t "   contrario será turno del siguiente jugador...~%")
	(format t "3) El primer jugador que vacie todas sus casillas, captura~%")
	(format t "   todas las fichas restantes de su oponente y las agrega a~%")
	(format t "   su base. El jugador con mas puntos gana la partida...~%")
	(format t "=================================================================="))

(defun display-menu () 
	"Despliega el menu"
	(format t "~%============================= Menu =============================~%")
	(format t "1) Jugar~%")
	(format t "2) Reglas~%")
	(format t "3) Salir~%")
	(format t "=================================================================="))

(defun get-menu-selection (attemps) 
	"Espera la opción del menu"
	(format t "~[~&~A ~;Opción incorrecta~&~A~;Vamos 1, 2 o 3, no es tan dificil~&~A~;\[Face palm\] Ingresa 0 para desplegar el menu nuevamente~&~A~;~A~:;Este juego es muy dificil para ti, ingresa 3 para salir~&~A~]~%" attemps "Selecciona una opción")
	(let ((sel-option (read)))
		(cond ((and (> attemps 0) (equal sel-option 0)) (display-menu) (get-menu-selection 4))
			  ((equal sel-option 1) (select-level))
			  ((equal sel-option 2) (display-rules)
			  							(display-menu)
			  							(get-menu-selection 0))
			  ((equal sel-option 3) (displayFarewell))
			  (T (get-menu-selection (+ 1 attemps))) )) )

(defun display-title ()
	"Despliega el titulo"
	(format t "~%=============================================================================~%")
	(format t ".___  ___.      ___      .__   __.   ______     ___       __          ___      ~%")
    (format t "|   \\/   |     /   \\     |  \\ |  |  /      |   /   \\     |  |        /   \\     ~%")
    (format t "|  \\  /  |    /  ^  \\    |   \\|  | |  ,----'  /  ^  \\    |  |       /  ^  \\    ~%")
    (format t "|  |\\/|  |   /  /_\\  \\   |  . `  | |  |      /  /_\\  \\   |  |      /  /_\\  \\   ~%")
    (format t "|  |  |  |  /  _____  \\  |  |\\   | |  `----./  _____  \\  |  `----./  _____  \\  ~%")
    (format t "|__|  |__| /__/     \\__\\ |__| \\__|  \______/__/     \\__\\ |_______/__/     \\__\\ ~%")
    (format t "============================================================================="))

(defun displayChicken () 
	"Despliega una gallina"
	nil )
(defun displayCongrats () 
	"Despliega un mensaje de felicitaciones"
	nil )
(defun displayNotBad ()
	"Despliega un mensaje de empate"
	nil )
(defun displayFatality ()
	"Despliega un mensaje de derrota"
	nil )

(defun displayFarewell ()
	"Despliega despedida"
	(cond ((= *player-score* *ai-score* 0) (displayChicken))
			((= *player-score* *ai-score*) (displayNotBad))
			((< *player-score* *ai-score*) (displayFatality))
			((> *player-score* *ai-score*) (displayCongrats))
			(T (format t "~%Adios.")) ))

(defun startScreen ()
	"Despliega el titulo, el menu y lee una entrada"
	(display-title)
	(display-menu)
	(get-menu-selection 0))

(startScreen)
