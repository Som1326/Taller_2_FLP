#lang eopl
;Sebastián Orrego Marín - 1941144
;Franklin Aguirre ...


;Implementación en Listas
;Interfaz: Definición de funciones para definir grafos no dirigidos con representación utilizando listas

;graph ::= ('vertices List(<caracter>)
;      ::= ('edges List(vertices))

;Constructores
(define edges
  (lambda (l)
    (list 'edges l)))

(define vertices
  (lambda (l)
    (list 'vertices l)))

;Predicados
(define vertices?
  (lambda (l)
    (if (eqv? (car l) 'vertices)
        (letrec
            (
             (listVertices
              (lambda (list)
                (if (not (null? list))
                    (if (symbol? (car list))
                        (listVertices (cdr list)) 
                        #f
                        )
                    #t
                    )
                )
              )
             )
          (listVertices (cadr l))
          )
        #f
        )
    )
  )

(define edges?
  (lambda (listEdges listVertices)
    (if (null? listEdges)
        #t
        (if (eqv? (car listEdges) 'edges)
            (letrec
                (
                 (comparision
                  (lambda (l1 l2)
                    (if (not (null? l1))
                        (if (not (null? l2))
                            (if (eqv? (car l1) (car l2))
                                (comparision (cdr l1) listVertices)
                                (comparision l1 (cdr l2))
                                )
                            ;(comparision (cdr l1) (cadr listVertices))
                            l1
                            )
                        (edges? (cons 'edges (cadr listEdges)) listVertices)
                        )
                    )
                  )
                 )
              (comparision (caadr listEdges) (cadr listVertices))
              )
            #f
            )
        )
    )
  )