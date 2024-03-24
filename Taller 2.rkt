#lang eopl
;Sebastián Orrego Marín - 1941144
;Franklin Aguirre - 1841743


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

;Función add-edge

(define add-edge
  (lambda (graph edge)
    (cases Data-graph graph
      (graph-exp (vertices edges)
                 (let ((edges-list (edges-list edges)))
                   (if (or (member-aux (edge-exp (car edge) (cadr edge)) edges-list)
                           (member-aux (reverse-edge (edge-exp (car edge) (cadr edge))) edges-list))
                       (eopl:error "The given edge already exists in the graph")
                       (graph-exp vertices (edges-exp (cons (edge-exp (car edge) (cadr edge)) edges-list)))))))))

(define reverse-edge
  (lambda (edge)
    (cases Data-edge edge
      (edge-exp (a b)
                (edge-exp b a)))))

(define edges-list
  (lambda (edges)
    (cases Data-edges edges
      (edges-exp (edges-list)
                 edges-list))))

(define (member-aux edge edges-list)
  (let loop ((lst edges-list))
    (cond ((null? lst) #f)
          ((equal? edge (car lst)) #t)
          (else (loop (cdr lst))))))

;Función vecinos

(define vecinos
  (lambda (graph node)
    (define edges-list
      (lambda (edges)
        (cases Data-edges edges
          (edges-exp (edges-list)
                     edges-list))))
    
    (cases Data-graph graph
      (graph-exp (vertices edges)
                 (let ((neighbor-nodes '()))
                   (my-for-each (lambda (edge)
                               (cases Data-edge edge
                                 (edge-exp (a b)
                                           (cond
                                             ((eq? a node) (set! neighbor-nodes (cons b neighbor-nodes)))
                                             ((eq? b node) (set! neighbor-nodes (cons a neighbor-nodes)))))))
                             (edges-list edges))
                   neighbor-nodes)))))

(define (my-for-each proc lst)
  (cond
    ((null? lst) 'done)
    (else
     (proc (car lst))
     (my-for-each proc (cdr lst)))))

;Apartado de pruebas implementación en Listas

(define test-graph-list1
  (graph
   (vertices '(a b c d))
   (edges '((a b) (c d) (c b) (a c))))
  )

(define test-graph-list2
  (graph
   (vertices '(x y z w v))
   (edges '((x w) (x y) (y v) (w z) (z y) (v w))))
  )

;Apartado de pruebas implementación en Datatypes

(define test-graph1
  (graph-exp
    (vertices-exp '(a b c d))
    (edges-exp
      (list
        (edge-exp 'a 'b)
        (edge-exp 'c 'd)
        (edge-exp 'c 'b)
        (edge-exp 'a 'c)))
    )
  )

(define test-graph2
  (graph-exp
    (vertices-exp '(x y z w v))
    (edges-exp
      (list
        (edge-exp 'x 'w)
        (edge-exp 'x 'y)
        (edge-exp 'y 'v)
        (edge-exp 'w 'z)
        (edge-exp 'z 'y)
        (edge-exp 'v 'w)))
    )
  )

;Apartado de pruebas de add-edge 

(display "--------------------------------------------")
(newline)
(display "Pruebas de add-edge ")
(newline)
(display "Grafo original 1: ")
(newline)
(display test-graph1)
(newline)
(newline)

(define modified-graph1 (add-edge test-graph1 '(a d)))

(display "Grafo 1 modificado (añadiendo (a, d)): ")
(newline)

(display modified-graph1)
(newline)
(newline)

(display "Grafo original 2: ")
(newline)
(display test-graph2)
(newline)
(newline)

(define modified-graph2 (add-edge test-graph2 '(w y)))

(display "Grafo 2 modificado (añadiendo (w, y)): ")
(newline)

(display modified-graph2)
(newline)
(newline)
(display "--------------------------------------------")
(newline)


;Apartado de pruebas de vecinos

(display "Pruebas de vecinos ")
(newline)
(display "Grafo original: ")
(display test-graph2)
(newline)
(newline)

(display "Vecinos de 'a': ")
(display (vecinos test-graph1 'a))
(newline)
(newline)

(display "Vecinos de 'x': ")
(display (vecinos test-graph2 'x))
(newline)