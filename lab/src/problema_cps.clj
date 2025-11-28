;; Definim els arbres binaris d'una manera senzilla...

(def Buit nil)
(def Node (fn [val esq dre] {:val val, :esq esq, :dre dre}))

;; Una fulla és un node de la forma {:val val, :esq nil, :dre nil}
;; L'arbre buit NO és una fulla
(def buit? nil?)
(def fulla? #(and (not (nil? %)) (buit? (:esq %)) (buit? (:dre %))))


;; Imaginem ara que volem calcular una funció F, d'un arbre binari d'enters i que retorna un enter: F: Arbre(Z) -> Z
;; Aquesta funció F depén de dues funcions G: ZxZxZ -> Z (donats tres enters, retorna un enter), i H: Z -> Z.
;; La definició és:
;; (F Buit) = 0
;; (F n)    = (G (:val n) (H (F (:esq n))) (H (F (:dre n)))) si n no és una fulla
;; (F n)    = (:val n)                                       si n és una fulla
;;

(defn F
  " a és l'arbre, i G i H són les funcions que cal per calcular F"
  [G H a]
  (letfn [(f [node]
            (cond
              (buit? node)  0
              (fulla? node) (:val node)
              :else         (let [fesq  (f (:esq node))
                                  fdre  (f (:dre node))
                                  r     (G (:val node) (H fesq) (H fdre))]
                              r)))]
    (f a)))

;; Per exemple:
;;
;; Si fem servir una funció auxiliar...

(def quadrat #(*' % %))

;; ...definim una funció EX sobre arbres binaris tal que:
;; (EX Buit) = 0
;; (EX node) = (+' (:val node) (quadrat (EX (:esq node))) (quadrat (EX (:dre node)))) si node no és fulla
;; (EX node) = (:val node) si node és una fulla

;; Podem fer:
(def EX (partial F +' quadrat))

;; Arbres d'exemple:

(def arbre1 (Node -25 (Node -70 (Node -12 (Node  3 nil nil)
                                          (Node  2 nil nil))
                                (Node -9  (Node  1 nil nil)
                                          (Node  4 nil nil)))
                      (Node -50 (Node 1   (Node -2 nil nil)
                                          (Node  1 nil nil))
                                (Node -27 (Node -3 nil nil)
                                          (Node -4 nil nil)))))
;; (EX arbre1) => 100

(def arbre2 (Node -2  (Node 10  (Node 3   (Node  4 nil nil)
                                          (Node  2 nil nil))
                                (Node 20  (Node  5 nil nil)
                                          (Node  4 nil nil)))
                      (Node -15 (Node 1   (Node -2 nil nil)
                                          (Node  1 nil nil))
                                (Node 4   (Node -8 nil nil)
                                          (Node -4 nil nil)))))

;; (EX arbre2) => 68231527

;; Ara, volem fer un petit canvi en aquest problema. Volem que, si en calcular F apareix
;; el nombre 42 (com a resultat de calcular F en *qualsevol* dels nodes de l'arbre), el
;; valor retornat sigui -1. A més, volem que aquest -1 es retorni *immediatament*, sense
;; fer cap de les accions que queden pendents, per exemple, retornar de crides recursives.

;; Si mirem de resoldre-ho directament... (anomenem F'' a aquesta errònia solució):

(defn F''
  " a és l'arbre, i G i H són les funcions que cal per calcular F"
  [G H a]
  (letfn [(f [node]
            (cond
              (buit? node)  0
              (fulla? node) (let [r (:val node)]
                              (println node r)
                              (if (= r 42) -1 r))
              :else         (let [fesq  (f (:esq node))
                                  fdre  (if (= fesq -1) -1 (f (:dre node)))
                                  r     (if (or (= fesq -1) (= fdre -1)) -1 (G (:val node) (H fesq) (H fdre)))]
                              (println node r)
                              (if (or (= r 42) (= r -1)) -1 r))))]
    (f a)))

;; Hi hem posat uns 'println' per poder veure què passa...
;;
;; Per seguir amb l'exemple anterior:

(def EX'' (partial F'' +' quadrat))

;; Definim alguns arbres més (ja que (EX arbre1) = (EX'' arbre1) i (EX arbre2) = (EX'' arbre2) ):
  
(def arbre3 (Node -25 (Node -70 (Node -12 (Node  3 nil nil)
                                          (Node  2 nil nil))
                                (Node -9  (Node  1 nil nil)
                                          (Node 42 nil nil)))
                      (Node -50 (Node 1   (Node -2 nil nil)
                                          (Node  1 nil nil))
                                (Node -27 (Node -3 nil nil)
                                          (Node -4 nil nil)))))
;; (EX   arbre3) => 9507768740164
;; (EX'' arbre3) => -1

(def arbre4 (Node -2  (Node 10  (Node 3   (Node  4 nil nil)
                                          (Node  2 nil nil))
                                (Node 1   (Node  5 nil nil)
                                          (Node  4 nil nil)))
                      (Node -15 (Node 1   (Node -1 nil nil)
                                          (Node  1 nil nil))
                                (Node 4   (Node -8 nil nil)
                                          (Node -4 nil nil)))))

;; (EX   arbre4) => 55006307
;; (EX'' arbre4) => -1

(def arbre5 (Node 5 arbre1 arbre3))

;; (EX   arbre5) => 90397666416439735746756901N
;; (EX'' arbre5) => -1

;; Anem a veure el detall de les execucions d'EX'' amb aquests arbres:

(comment
(EX'' arbre3) =>
{:val 3, :esq nil, :dre nil} 3
{:val 2, :esq nil, :dre nil} 2
{:val -12, :esq {:val 3, :esq nil, :dre nil}, :dre {:val 2, :esq nil, :dre nil}} 1
{:val 1, :esq nil, :dre nil} 1
{:val 42, :esq nil, :dre nil} 42
{:val -9, :esq {:val 1, :esq nil, :dre nil}, :dre {:val 42, :esq nil, :dre nil}} -1
{:val -70, :esq {:val -12, :esq {:val 3, :esq nil, :dre nil}, :dre {:val 2, :esq nil, :dre nil}}, :dre {:val -9, :esq {:val 1, :esq nil, :dre nil}, :dre {:val 42, :esq nil, :dre nil}}} -1
{:val -25, :esq {:val -70, :esq {:val -12, :esq {:val 3, :esq nil, :dre nil}, :dre {:val 2, :esq nil, :dre nil}}, :dre {:val -9, :esq {:val 1, :esq nil, :dre nil}, :dre {:val 42, :esq nil, :dre nil}}}, :dre {:val -50, :esq {:val 1, :esq {:val -2, :esq nil, :dre nil}, :dre {:val 1, :esq nil, :dre nil}}, :dre {:val -27, :esq {:val -3, :esq nil, :dre nil}, :dre {:val -4, :esq nil, :dre nil}}}} -1
-1
)

(comment
(EX'' arbre4) =>
{:val 4, :esq nil, :dre nil} 4
{:val 2, :esq nil, :dre nil} 2
{:val 3, :esq {:val 4, :esq nil, :dre nil}, :dre {:val 2, :esq nil, :dre nil}} 23
{:val 5, :esq nil, :dre nil} 5
{:val 4, :esq nil, :dre nil} 4
{:val 1, :esq {:val 5, :esq nil, :dre nil}, :dre {:val 4, :esq nil, :dre nil}} 42
{:val 10, :esq {:val 3, :esq {:val 4, :esq nil, :dre nil}, :dre {:val 2, :esq nil, :dre nil}}, :dre {:val 1, :esq {:val 5, :esq nil, :dre nil}, :dre {:val 4, :esq nil, :dre nil}}} -1
{:val -2, :esq {:val 10, :esq {:val 3, :esq {:val 4, :esq nil, :dre nil}, :dre {:val 2, :esq nil, :dre nil}}, :dre {:val 1, :esq {:val 5, :esq nil, :dre nil}, :dre {:val 4, :esq nil, :dre nil}}}, :dre {:val -15, :esq {:val 1, :esq {:val -1, :esq nil, :dre nil}, :dre {:val 1, :esq nil, :dre nil}}, :dre {:val 4, :esq {:val -8, :esq nil, :dre nil}, :dre {:val -4, :esq nil, :dre nil}}}} -1
-1
)

(comment
(EX'' arbre5) =>
{:val 3, :esq nil, :dre nil} 3
{:val 2, :esq nil, :dre nil} 2
{:val -12, :esq {:val 3, :esq nil, :dre nil}, :dre {:val 2, :esq nil, :dre nil}} 1
{:val 1, :esq nil, :dre nil} 1
{:val 4, :esq nil, :dre nil} 4
{:val -9, :esq {:val 1, :esq nil, :dre nil}, :dre {:val 4, :esq nil, :dre nil}} 8
{:val -70, :esq {:val -12, :esq {:val 3, :esq nil, :dre nil}, :dre {:val 2, :esq nil, :dre nil}}, :dre {:val -9, :esq {:val 1, :esq nil, :dre nil}, :dre {:val 4, :esq nil, :dre nil}}} -5
{:val -2, :esq nil, :dre nil} -2
{:val 1, :esq nil, :dre nil} 1
{:val 1, :esq {:val -2, :esq nil, :dre nil}, :dre {:val 1, :esq nil, :dre nil}} 6
{:val -3, :esq nil, :dre nil} -3
{:val -4, :esq nil, :dre nil} -4
{:val -27, :esq {:val -3, :esq nil, :dre nil}, :dre {:val -4, :esq nil, :dre nil}} -2
{:val -50, :esq {:val 1, :esq {:val -2, :esq nil, :dre nil}, :dre {:val 1, :esq nil, :dre nil}}, :dre {:val -27, :esq {:val -3, :esq nil, :dre nil}, :dre {:val -4, :esq nil, :dre nil}}} -10
{:val -25, :esq {:val -70, :esq {:val -12, :esq {:val 3, :esq nil, :dre nil}, :dre {:val 2, :esq nil, :dre nil}}, :dre {:val -9, :esq {:val 1, :esq nil, :dre nil}, :dre {:val 4, :esq nil, :dre nil}}}, :dre {:val -50, :esq {:val 1, :esq {:val -2, :esq nil, :dre nil}, :dre {:val 1, :esq nil, :dre nil}}, :dre {:val -27, :esq {:val -3, :esq nil, :dre nil}, :dre {:val -4, :esq nil, :dre nil}}}} 100
{:val 3, :esq nil, :dre nil} 3
{:val 2, :esq nil, :dre nil} 2
{:val -12, :esq {:val 3, :esq nil, :dre nil}, :dre {:val 2, :esq nil, :dre nil}} 1
{:val 1, :esq nil, :dre nil} 1
{:val 42, :esq nil, :dre nil} 42
{:val -9, :esq {:val 1, :esq nil, :dre nil}, :dre {:val 42, :esq nil, :dre nil}} -1
{:val -70, :esq {:val -12, :esq {:val 3, :esq nil, :dre nil}, :dre {:val 2, :esq nil, :dre nil}}, :dre {:val -9, :esq {:val 1, :esq nil, :dre nil}, :dre {:val 42, :esq nil, :dre nil}}} -1
{:val -25, :esq {:val -70, :esq {:val -12, :esq {:val 3, :esq nil, :dre nil}, :dre {:val 2, :esq nil, :dre nil}}, :dre {:val -9, :esq {:val 1, :esq nil, :dre nil}, :dre {:val 42, :esq nil, :dre nil}}}, :dre {:val -50, :esq {:val 1, :esq {:val -2, :esq nil, :dre nil}, :dre {:val 1, :esq nil, :dre nil}}, :dre {:val -27, :esq {:val -3, :esq nil, :dre nil}, :dre {:val -4, :esq nil, :dre nil}}}} -1
{:val 5, :esq {:val -25, :esq {:val -70, :esq {:val -12, :esq {:val 3, :esq nil, :dre nil}, :dre {:val 2, :esq nil, :dre nil}}, :dre {:val -9, :esq {:val 1, :esq nil, :dre nil}, :dre {:val 4, :esq nil, :dre nil}}}, :dre {:val -50, :esq {:val 1, :esq {:val -2, :esq nil, :dre nil}, :dre {:val 1, :esq nil, :dre nil}}, :dre {:val -27, :esq {:val -3, :esq nil, :dre nil}, :dre {:val -4, :esq nil, :dre nil}}}}, :dre {:val -25, :esq {:val -70, :esq {:val -12, :esq {:val 3, :esq nil, :dre nil}, :dre {:val 2, :esq nil, :dre nil}}, :dre {:val -9, :esq {:val 1, :esq nil, :dre nil}, :dre {:val 42, :esq nil, :dre nil}}}, :dre {:val -50, :esq {:val 1, :esq {:val -2, :esq nil, :dre nil}, :dre {:val 1, :esq nil, :dre nil}}, :dre {:val -27, :esq {:val -3, :esq nil, :dre nil}, :dre {:val -4, :esq nil, :dre nil}}}}} -1
-1
)

;; Fixem-nos que, a la que detecta el 42 no es fan més crides noves, però cal propagar el -1
;; cap amunt, retornant-lo de les crides recursives que hi havia pendents.

;; ====> NO ÉS AIXÒ EL QUE DEMANEM <====

;; Cal retornar *immediatament* en detectar el nombre especial 42.

;; Suposem que anomenem F' a la solució que busquem (i hi posem uns 'println' per veure què passa), aleshores
;; si fem (def EX' (partial F' +' quadrat)), hauríem d'observar:

(comment
(EX' arbre3) =>
{:val 3, :esq nil, :dre nil} 3
{:val 2, :esq nil, :dre nil} 2
{:val -12, :esq {:val 3, :esq nil, :dre nil}, :dre {:val 2, :esq nil, :dre nil}} 1
{:val 1, :esq nil, :dre nil} 1
{:val 42, :esq nil, :dre nil} 42
-1
)

(comment
(EX' arbre4) =>
{:val 4, :esq nil, :dre nil} 4
{:val 2, :esq nil, :dre nil} 2
{:val 3, :esq {:val 4, :esq nil, :dre nil}, :dre {:val 2, :esq nil, :dre nil}} 23
{:val 5, :esq nil, :dre nil} 5
{:val 4, :esq nil, :dre nil} 4
{:val 1, :esq {:val 5, :esq nil, :dre nil}, :dre {:val 4, :esq nil, :dre nil}} 42
-1
)

(comment
(EX' arbre5) =>
{:val 3, :esq nil, :dre nil} 3
{:val 2, :esq nil, :dre nil} 2
{:val -12, :esq {:val 3, :esq nil, :dre nil}, :dre {:val 2, :esq nil, :dre nil}} 1
{:val 1, :esq nil, :dre nil} 1
{:val 4, :esq nil, :dre nil} 4
{:val -9, :esq {:val 1, :esq nil, :dre nil}, :dre {:val 4, :esq nil, :dre nil}} 8
{:val -70, :esq {:val -12, :esq {:val 3, :esq nil, :dre nil}, :dre {:val 2, :esq nil, :dre nil}}, :dre {:val -9, :esq {:val 1, :esq nil, :dre nil}, :dre {:val 4, :esq nil, :dre nil}}} -5
{:val -2, :esq nil, :dre nil} -2
{:val 1, :esq nil, :dre nil} 1
{:val 1, :esq {:val -2, :esq nil, :dre nil}, :dre {:val 1, :esq nil, :dre nil}} 6
{:val -3, :esq nil, :dre nil} -3
{:val -4, :esq nil, :dre nil} -4
{:val -27, :esq {:val -3, :esq nil, :dre nil}, :dre {:val -4, :esq nil, :dre nil}} -2
{:val -50, :esq {:val 1, :esq {:val -2, :esq nil, :dre nil}, :dre {:val 1, :esq nil, :dre nil}}, :dre {:val -27, :esq {:val -3, :esq nil, :dre nil}, :dre {:val -4, :esq nil, :dre nil}}} -10
{:val -25, :esq {:val -70, :esq {:val -12, :esq {:val 3, :esq nil, :dre nil}, :dre {:val 2, :esq nil, :dre nil}}, :dre {:val -9, :esq {:val 1, :esq nil, :dre nil}, :dre {:val 4, :esq nil, :dre nil}}}, :dre {:val -50, :esq {:val 1, :esq {:val -2, :esq nil, :dre nil}, :dre {:val 1, :esq nil, :dre nil}}, :dre {:val -27, :esq {:val -3, :esq nil, :dre nil}, :dre {:val -4, :esq nil, :dre nil}}}} 100
{:val 3, :esq nil, :dre nil} 3
{:val 2, :esq nil, :dre nil} 2
{:val -12, :esq {:val 3, :esq nil, :dre nil}, :dre {:val 2, :esq nil, :dre nil}} 1
{:val 1, :esq nil, :dre nil} 1
{:val 42, :esq nil, :dre nil} 42
-1
)

;; Compareu el resultat d'executar F'' (que no és el que volem, malgrat calcular correctament el
;; que volem calcular) i F' (la solució que vosaltres heu de fer).

;; Us recordo que ja vam veure un exemple més senzill d'això mateix que volem fer aquí,
;; quan vam explicar CPS (tema 4, planes 13-16)

;; SOLUCIÓ... ?
;;
