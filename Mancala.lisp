(defparameter *board* '((0 0 0) ((1 1 1) (1 1 1) (1 1 1) (1 1 1) (1 1 1) (1 1 1))
								 ((1 1 1) (1 1 1) (1 1 1) (1 1 1) (1 1 1) (1 1 1)) (0 0 0)))
(defparameter *ai-score* 0) ;; partidas ganadas por la IA
(defparameter *player-score* 0) ;; partidas ganadas por el jugador
(defparameter *limit* 1) ;; limite en la busqueda a la profundo
(defparameter *current-turn* 1) ;;1 o 2 
(defparameter *finish* nil)

(defun move (from &rest))

(defun is-GameOver? (state turn)
	"revisa si ya se acabo el juego"
	(loop for h in (nth turn state)
		with over
		do (loop for c in h 
				if (= c 1)
					do (return (setf over nil))
				 	do (setf over T))
		finally (return over) ))

(defun hole (x y)
	"Devuelve la lista de canicas del hoyo"
	(nth y (nth x *board*) ))

(defun display-board () 
	"Despliega el estado actual del tablero"
	(format t "~&====================================================================~&")
	(format t "Azul = A 	Verde = V 	Rojo = R")
	(format t "~&====================================================================~&")
	(format t "~&====================================================================~&")
	(format t "~&= A ~A == A V R == A V R == A V R == A V R == A V R == A V R == ~A A =~&" 
		(first (first *board*)) (first (nth 3 *board*)) )
	(format t "~&=     == ~{~A ~}== ~{~A ~}== ~{~A ~}== ~{~A ~}== ~{~A ~}== ~{~A ~}==     =~&" 
		(hole 1 0) (hole 1 1) (hole 1 2) (hole 1 3) (hole 1 4) (hole 1 5))
	(format t "~&= V ~A ======================================================== ~A V =~&" 
		(second (first *board*)) (second (nth 3 *board*)))
	(format t "~&=     == A V R == A V R == A V R == A V R == A V R == A V R ==     =~&")
	(format t "~&= R ~A == ~{~A ~}== ~{~A ~}== ~{~A ~}== ~{~A ~}== ~{~A ~}== ~{~A ~}== ~A R =~&" 
		(nth 2 (first *board*)) (hole 2 0) (hole 2 1) (hole 2 2) (hole 2 3) (hole 2 4) (hole 2 5) (nth 2 (nth 3 *board*)))
	(format t "~&====================================================================~&")
	(format t "~&====================================================================~&")
	(format t "Puntaje:~%")
	(format t "        Azul = 1 punto ~%        Verde = 5 puntos ~%        Rojo = 10 puntos")
	(format t "~&====================================================================~&"))



(defun start-game ()
	"Empieza el juego"
	(request-level))

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
			  ((equal sel-option 1) (start-game))
			  ((equal sel-option 2) (display-rules)
			  							(display-menu)
			  							(get-menu-selection 0))
			  ((equal sel-option 3) (display-farewell))
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

(defun display-chicken () 
	"Despliega una gallina"
	nil )
(defun display-congrats () 
	"Despliega un mensaje de felicitaciones"
	nil )
(defun display-not-bad ()
	"Despliega un mensaje de empate"
	nil )
(defun display-fatality ()
	"Despliega un mensaje de derrota"
	nil )

(defun display-farewell ()
	"Despliega despedida"
	(cond ((= *player-score* *ai-score* 0) (display-chicken))
			((= *player-score* *ai-score*) (display-not-bad))
			((< *player-score* *ai-score*) (display-fatality))
			((> *player-score* *ai-score*) (display-congrats))
			(T (format t "~%Adios.")) ))

(defun start-screen ()
	"Despliega el titulo, el menu y lee una entrada"
	(display-title)
	(display-menu)
	(get-menu-selection 0))

(start-screen)