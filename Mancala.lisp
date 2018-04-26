;;									0		1		2		3		4 		5
(defparameter *board* '((0 0 0) ((1 1 1) (1 1 1) (1 1 1) (1 1 1) (1 1 1) (1 1 1)
								(1 1 1) (1 1 1) (1 1 1) (1 1 1) (1 1 1) (1 1 1)) (0 0 0)))
;;									6		7		8		9		10		11
(defparameter *ai-score* 0) ;; partidas ganadas por la IA
(defparameter *player-score* 0) ;; partidas ganadas por el jugador
(defparameter *limit* 1) ;; limite en la busqueda a la profundo
(defparameter *current-turn* 1) ;;1 player o 2 AI
(defparameter *finish* nil)
(defparameter *finisher* 0)
(defparameter *ai-move* nil)

;; (evalstate '((0 0 0) ((1 1 1) (1 1 1) (1 1 1) (1 1 1) (1 1 1) (1 1 1) (1 1 1) (1 1 1) (1 1 1) (1 1 1) (1 1 1) (0 0 0)) (1 0 0)))

(defun evalState (board)
	"Evalua el estado"
	(let ((score 0) (evl-mov 0) (evl-neg-mov 0) (evl 0))
		(setf score (loop for i in (third board)
						for j from 0 to 2
						sum (* (+ j 1) i  5) into score
						finally (return (* 13 score))))
		(setf evl-mov (loop for h in (second board)
						for i from 6 to 11
						sum (loop for j from 0 to 2
								sum (+ (* (nth j h) 5) 1) into x
								finally (return (* x (mod i 6) )) )))
		(setf evl-neg-mov (loop for h in (second board)
						for i from 1 to 6
						sum (loop for j from 0 to 2
								sum (+ (* (nth j h) j 5) 1) into x
								finally (return (* x (- i) ) ) )))
		(setf evl (+ score (* evl-mov 0.5) (* evl-neg-mov 0.5) ))
		evl ))



(defun getMarblesList (board hole) 
	(loop for i from 0 to 2
		with marbles = nil
		do (loop for j from 0 below (nth i (nth hole (second board)))
				do (setf marbles (cons i marbles) ) ) 
		finally (return marbles) ))

(defun cloneBoard (board)
	"Copia un nuevo tablero"
	(let ((result nil) )
		(setf result 
			(append result (list (loop for g in (first board)
								collect g)) ))
		(setf result (append result 
							(list (loop for h in (second board)
								collect (loop for p in h
											collect p))) ))
		(setf result 
			(append result (list (loop for g in (third board)
								collect g)) ))
		result))

(defun apply-AI-move (board hole movements)
	(loop for mov in movements
		;do (print (first mov))
		if (=  (first mov) -2)
			do (setf (nth (second mov) (third board)) (+ (nth (second mov) (third board)) 1) )
		else 
			do (setf (nth (second mov) (nth (first mov) (second board))) (+ (nth (second mov) (nth (first mov) (second board))) 1) )
		do (setf (nth (second mov) (nth hole (second board))) 0) ) 
	
	board)

(defun groupMovements (movements num)
	"Divide una lista en [num] partes"
	(loop for i from 0 below num
		collect (subseq movements (* num i) (+ (* num i) num) ) ))

(defun filterOperators (operators start)
	(loop for op in operators 
		with table = nil
		do (setf table (make-hash-table))
		when (and (= (first (first op)) start) (loop for mov in op
					if (gethash (first mov) table)
						do (return Nil)
						do (setf (gethash (first mov) table) 1)
					finally (return T) ))
			collect op into filtered
		finally (return filtered)))



(defun isHoleEmpty? (board num)
	"Dice si el hoyo esta vacio"
	(loop for h in (nth num (second board)) always (= h 0)))

(defun getMarbles (board num) 
	"Obtiene el total de canicas en el hoyo"
	(loop for h in (nth num (second board)) sum h))

(defun addGoalMov (lst search goal)
	(let ((new-lst nil) (sIndex (loop for x in lst
						for i from 0 to (length lst)
						when (= x search)
							do (return i))))
		(cond ((= sIndex 0) 
						(setf new-lst (append (list goal) lst))
						(setf new-lst (subseq new-lst 0 (- (length new-lst) 1) )) )
			((= sIndex (- (length new-lst) 1)) 
				(setf new-lst (subseq new-lst 0 (- (length lst) 1) ))
				(setf new-lst (append new-lst (list goal) )) )
			(T (setf new-lst (subseq lst 0 sIndex ))
				(setf new-lst (append new-lst (list goal) ))
				(setf new-lst (append new-lst (subseq lst sIndex (- (length lst) 1)) ) )) )
		new-lst ))

(defun generateMoves (board hole turn)
	"Genera una lista con posibles movimientos"
	(let ((moves '()) (marbles (getMarbles board hole)) )
			(loop for i from (+ hole 1) to (+ hole marbles )
					collect (mod i 12) into c
					finally (setf moves c)) 
			(when (and (= turn 1) (>= (+ hole marbles) 6))
					(setf moves (addGoalMov moves 6 -1)) )
			(when (and (= turn 2) (>= (+ hole marbles) 12) )
					(setf moves (addGoalMov moves 0 -2)) ) 
			moves ))


(defun f (board hole operator) 
	3)

(defun quicksort (lst)
	(if (null lst) 
    	nil
    	(let* ((x (first lst))
	     		(r (rest lst))
	     		(fn (lambda (a) (< (f a) (f x) ))))
			(append (quicksort (remove-if-not fn r))
					(list x)
(quicksort (remove-if fn r))))))

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
		(cond ((not (numberp type)) 
				(format t "Error ingrese un numero entero:~&")
				(move board movements hole))
			((or  (< type 0) (> type 2))
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
	(let ((over nil))
		(when (loop for i from 0 to 5
						always (isHoleEmpty? board i))
				(setf over T)
				(setf *finisher* 1))
		(when (loop for i from 6 to 11
						always (isHoleEmpty? board i))
				(setf over T)
				(setf *finisher* 2))
		over ))

(defun gencombinations (movements marbles)
	(cond ((= (length movements) 1)
		(loop for i downfrom 2 to 0
						collect (list (first movements) (nth i marbles))) )
		(T (loop for res in (gencombinations (rest movements) marbles)
			with nextres = '()
			do (loop for marble in marbles
						if (listp (first res))
							collect (append res (list (list (first movements) marble))) into temp
						else
							collect (list res (list (first movements) marble)) into temp
						finally (setf nextres (append nextres temp)))
			finally (return nextres) )) ))

(defun getDiffMarbles (board hole)
	"Obtiene una lista con los diferentes tipos de canicas de [hole]"
	(loop for i from 0 to 2
		when (nth i (nth hole (second board)) )
		collect i))

(defun isValidOperator? (board hole operator)
	"Valida si un operador es valido con el [board] y el [hole] seleccionado"
	(loop for op in operator 
		with red = 0
		with blue = 0
		with green = 0
		with marbles = (nth hole (second board))
		if (= (second op) 0)
			do (setf blue (+ blue 1))
		else
			if (= (second op) 1)
				do (setf green (+ green 1))
			else
				do (setf red (+ red 1))
			end
		finally (return (not (or (< (- (first marbles) blue) 0) (< (- (second marbles) green) 0) (< (- (third marbles) red) 0)) )) ))

(defun negamax-alfabeta (board depth alfa beta color)
	(cond ((or (isGameOver? board) (> depth *limit*) )
		 (* color (evalState board)) )
		(T (let ((bestMov nil) 
				(bestHole nil)
				(bestValue most-negative-fixnum))
			(loop for i downfrom 11 to 6
				with movements = nil
				with operators = nil
				with value = nil
				with num-mov = nil
				with start-mov = nil
				when (not (isHoleEmpty? board i))
				do (setf movements (generateMoves board i 2)) and
				do (setf num-mov (length movements)) and
				do (setf start-mov (first movements)) and
				do (setf operators (gencombinations movements (getDiffMarbles board i))) and
				do (setf movements nil) and
				do (print (length operators)) and
				do (loop for op1 in operators
						with new-state = nil
						with op = nil
						if (not (listp (first op1))) 
							do (setf op (list op1) )
						else 
							do (setf op op1)
						when (isValidOperator? board i op)
							do (setf new-state (cloneBoard board)) and
							do (apply-AI-move new-state i op) and
							do (setf value (negamax-alfabeta new-state (+ depth 1) (- beta) (- (max alfa bestValue)) (- color) ) ) and
							do (setf new-state nil) and
							do (setf value (- value)) and
							when (> value bestValue) 
								do (setf bestValue value) and
								do (setf bestMov op) and
								do (setf bestHole i) and
								when (>= bestValue beta )
									do (return T)
								end )
				do (setf operators nil)
				finally (setf *ai-move* (list bestHole bestMov bestValue))(return value) ) ))))


(defun ask-box ()
	"Pide que selecciones una casilla"
	(format t "Selecciona una casilla (0 al 5):~&")
	(setq selected-box (read))
	(cond ((not (numberp selected-box)) 
				(format t "Error ingrese un numero entero:~&")
				(ask-box))
		((or (< selected-box 0) (> selected-box 5) (isHoleEmpty? *board* selected-box))
				(format t "Error no contiene canicas:~&")
				(ask-box))
		(T T))
	selected-box)

(defun human-turn ()
	"Movimiento del jugador"
	(let* ((selected-hole (ask-box)) 
			(movements (generateMoves *board* selected-hole 1)))
			(move *board* movements selected-hole)
			(when (not (= (first (last movements)) -1))
					(setf *current-turn* 2) )))


(defun display-movements (operators)
	(format t "Movimientos de Skynet~&")
	(loop for op in operators
		with hole = 0
		with type = 0
		do (setf type (second op))
		if (= (first op) -2 )
			do (setf hole "GOAL")
		else
			do (setf hole (first op))
		when (= 0 type)
			do (format t "Movio una canica azul a ~A~&" hole)
		when (= 1 type)
			do (format t "Movio una canica verde a ~A~&" hole)
		when (= 2 type)
			do (format t "Movio una canica rojo a ~A~&" hole) ) )

(defun Agent-turn ()
	"Movimiento de la Agente"
	(display-board)
	(setf *ai-move* nil)
	(negamax-alfabeta *board* 0 most-negative-fixnum most-positive-fixnum 1)
	(let ((operators *ai-move*))
		(display-movements (reverse (second operators)))
		(apply-AI-move *board* (first operators) (reverse (second operators)))
		(when (not (= (first (first (second operators))) -2)) 
				(setf *current-turn* 1)) ))

(defun display-score ()
	(let ((scoreHuman 0) (scoreIA 0))
		(cond ((= *finisher* 1)
				(setf scoreHuman (loop for i from 6 to 11
					with hole = nil
					do (setf hole (nth i (second *board*)))
					sum (+ (* (first hole) 1)
							 (* (second hole) 5)
							  (* (third hole) 10) ) into total
					finally (return (+ total (* (first (first *board*)) 1) 
								(* (second (first *board*)) 5) 
								(* (third (first *board*)) 10) )) ) )
				(setf scoreIA (+ (* (first (third *board*)) 1) 
								(* (second (third *board*)) 5) 
								(* (third (third *board*)) 10) ) ) )
			(T (setf scoreIA (loop for i from 0 to 5
									with hole = nil
									do (setf hole (nth i (second *board*)))
									sum (+ (* (first hole) 1)
											 (* (second hole) 5)
											  (* (third hole) 10) ) into total
									finally (return (+ total (* (first (third *board*)) 1) 
												(* (second (third *board*)) 5)
												 (* (third (third *board*)) 10))) ))
				(setf scoreHuman (+ (* (first (first *board*)) 1) 
									(* (second (first *board*)) 5) 
									(* (third (first *board*)) 10) )) ) )
		(cond ((> scoreHuman scoreIA)
				(setf *player-score* (+ *player-score* 1))
				(format t "Los humanos vencieron a las maquinas~&"))
				(T (setf *ai-score* (+ *ai-score* 1))
					(format t "Skynet ha ganado~&")) )
		(format t "Puntaje ~&Humano = ~A ~&AI = ~A ~&" scoreHuman scoreIA ) ))


(defun ask-playagain ()
	"Pregunta al jugador si quiere jugar de nuevo"
	(format t "Desea jugar de nuevo Y/N:~&")
	(setq selected-option (read))
	(cond ((not (symbolp selected-option)) 
				(format t "Error ingrese Y o N:~&")
				(ask-playagain))
		(T T))
	selected-option)

(defun reset-game ()
	(setf *board* '((0 0 0) ((1 1 1) (1 1 1) (1 1 1) (1 1 1) (1 1 1) (1 1 1)
								(1 1 1) (1 1 1) (1 1 1) (1 1 1) (1 1 1) (1 1 1)) (0 0 0)))
	(setf *current-turn* 1)
	(setf *finish* nil)
	(setf *finisher* 0)
	(setf *ai-move* nil)
	(setf *limit* 1))

(defun display-gameover ()
	"Muestra el puntaje total"
	(format t "~&====================================================================~&")
	(format t "Fin del juego~&")
	(format t "~&====================================================================~&")
	(display-score)
	(cond ((equal (ask-playagain) 'Y)
			(reset-game)
			(select-level))
		(T (displayFarewell)) ))

(defun gameloop () 
	"El ciclo de juego"
	(display-board)
	(if (= *current-turn* 1)
		(human-turn)
		(Agent-turn))
	(setf *finish* (isGameOver? *board*))
	(if (not *finish*)
		(gameloop)
		(display-gameover)	))

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
