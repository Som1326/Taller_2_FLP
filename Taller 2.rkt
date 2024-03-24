#lang eopl
;Sebastián Orrego Marín - 1941144
;Franklin Aguirre ...


;Implementación en Listas
;Interfaz: Definición de funciones para definir grafos no dirigidos con representación utilizando listas

;graph ::= ('graph ('vertices List(<caracter>) ('edges List(<caracter>)))

;gragh ::= (<graph> vertices edges)
;vertices ::= (<vertices> <list>)
;edges ::= (<edges> <list>)

;Constructores
(define graph
  (lambda (vertices edges)
    (list 'graph vertices edges)
    )
  )

(define vertices
  (lambda (l)
    (list 'vertices l)
    )
  )

(define edges
  (lambda (l)
    (list 'edges l)
    )
  )

;Predicados
(define graph?
  (lambda (l)
    (if (eqv? (car l) 'graph)
        #t
        #f
        )
    )
  )

(define vertices?
  (lambda (l)
    (if (eqv? (car l) 'vertices)
        #t
        #f
        )
    )
  )

(define edges?
  (lambda (l)
    (if (eqv? (car l) 'edges)
        #t
        #f
        )
    )
  )

;Extractores
(define graph->vertices
  (lambda (l)
    (if (graph? l)
        (cadr l)
        (eopl:error "Graph has no valid vertices")
        )
    )
  )

(define graph->edges
  (lambda (l)
    (if (graph? l)
        (cadr (caddr l))
        (eopl:error "Graph has no valid edges")
        )
    )
  )

(define vertices->list
  (lambda (l)
    (if (vertices? l)
        (cadr l)
        (eopl:error "Vertices has no valid nodelist")
        )
    )
  )

(define edges->list
  (lambda (l)
    (if (edges? l)
        (cadr l)
        (eopl:error "Edges has no valid nodelist")
        )
    )
  )

;Implementación en datatypes
;Interfaz: Definición de funciones para definir grafos no dirigidos con representación utilizando datatypes

;gragh ::= (<graph> vertices edges)
;vertices ::= (<vertices> <list>)
;edges ::= (<edges> edge)
;edge ::= (<element> <element>)

(define-datatype Data-graph  Data-graph?
  (graph-exp (vertices-exp Data-vertices?)
             (edges-exp Data-edges?))
  )

(define-datatype Data-vertices Data-vertices?
  (vertices-exp (vertices-list (list-of symbol?)))
  )

(define-datatype Data-edges Data-edges?
  (edges-exp (edges-list (list-of Data-edge?)))
  )

(define-datatype Data-edge Data-edge?
  (edge-exp (a symbol?)
            (b symbol?))
  )

;Función PARSEBNF

(define parseMap
  (lambda (l1)
    (if (null? l1)
        empty
        (cons (edge-exp (caar l1) (cadar l1)) (parseMap (cdr l1)))
        )
    )
  )

(define PARSEBNF
  (lambda (dato)
    (cond
      [(eqv? (car dato) 'graph) (graph-exp (PARSEBNF (cadr dato)) (PARSEBNF (caddr dato)))]
      [(eqv? (car dato) 'vertices) (vertices-exp (cadr dato))]
      [(eqv? (car dato) 'edges) (edges-exp (PARSEBNF (cons 'edge (cadr dato))))]
      [(eqv? (car dato) 'edge) (parseMap (cdr dato))]
      )
    )
  )
      
;Función UNPARSEBNF

(define UNPARSEBNF
  (lambda (exp)
    (cases Data-graph exp
      (graph-exp (vertices-exp edges-exp)
                 (list 'graph (UNPARSEBNF-vertices vertices-exp) (UNPARSEBNF-edges edges-exp)))
      )
    )
  )

(define UNPARSEBNF-vertices
  (lambda (exp)
    (cases Data-vertices exp
      (vertices-exp (vertices-list)
                    (list 'vertices vertices-list))
      )
    )
  )

(define UNPARSEBNF-edges
  (lambda (exp)
    (cases Data-edges exp
      (edges-exp (edges-list)
                 (cons 'edges (list (unparseMap edges-list))))
      )
    )
  )

(define unparseMap
  (lambda (l1)
    (if (null? l1)
        empty
        (cons (UNPARSEBNF-edge (car l1)) (unparseMap (cdr l1)))
        )
    )
  )

(define UNPARSEBNF-edge
  (lambda (exp)
    (if (null? exp)
        empty
        (cases Data-edge exp
          (edge-exp (a b)
                    (list a b)))
        )
    )
  )

; parte 3 del taller y posibles soluciones a los enunciados 

(define add-edge
  (lambda (graph edge)
    (cond
      ((graph? graph)
       (let ((vertices (graph->vertices graph))
             (edges (graph->edges graph)))
         (if (or (member edge edges)
                 (member (reverse edge) edges))
             graph
             (graph (vertices) (cons edge edges)))))
      (else
       (eopl:error "Invalid graph")))))


(define vecinos
  (lambda (graph node)
    (cases Data-graph graph
      (graph-exp (vertices edges)
                 (let ((neighbor-nodes '()))
                   (for-each (lambda (edge)
                               (cases Data-edge edge
                                 (edge-exp (a b)
                                           (cond
                                             ((eq? a node) (set! neighbor-nodes (cons b neighbor-nodes)))
                                             ((eq? b node) (set! neighbor-nodes (cons a neighbor-nodes)))))))
                             edges)
                   neighbor-nodes)))))



;(define add-edge
;  (lambda (graph edge)
;    (let* ((edges (graph->edges graph))
;           (vertices (graph->vertices graph))
;           (new-edge (list (car edge) (cadr edge)))
;           (reverse-edge (list (cadr edge) (car edge))))
;      (if (or (member new-edge edges) (member reverse-edge edges))
;          graph ; La arista ya existe, devuelve el grafo sin cambios.
;          (graph vertices (cons new-edge edges)))))) ; Añade la nueva arista.





;(define vecinos
;  (lambda (grafo nodo)
;    (let ((bordes (edges-exp (Data-graph-edges-exp grafo))))
;      (let loop ((bordes bordes) (vecinos '()))
;        (cond
;          ((null? bordes) vecinos)
;          ((eq? (Data-edge-a (car bordes)) nodo)
;           (loop (cdr bordes) (cons (Data-edge-b (car bordes)) vecinos)))
;          ((eq? (Data-edge-b (car bordes)) nodo)
;           (loop (cdr bordes) (cons (Data-edge-a (car bordes)) vecinos)))
;          (else (loop (cdr bordes) vecinos))
;          )
;        )
;      )
;    )
;  )











