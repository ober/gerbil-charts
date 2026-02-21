;;; -*- Gerbil -*-
;;; Graphviz DOT DSL and utilities
;;; Generate DOT files from S-expressions and JSON, render via dot/neato/fdp/etc.
(export
  ;; Core generation
  dot->string
  render-dot
  render-dot-to-file
  ;; DSL constructors
  digraph
  graph
  subgraph
  node
  edge
  attr
  ;; JSON conversion
  json->dot
  ;; Rendering engines
  graphviz-engines)

(import (only-in :gerbil-graphviz/graphviz
                 read-dot-string graph->string graph->file)
        :std/misc/string
        :std/format
        :std/text/json
        :std/sugar
        :std/srfi/1)

;; Available graphviz layout engines
(def graphviz-engines
  '(dot neato fdp sfdp circo twopi osage patchwork))

;; Escape string for DOT format
(def (escape-dot-string s)
  (if (not s) ""
    (let ((s (string-subst s "\"" "\\\"")))
      (string-subst s "\n" "\\n"))))

;; Format DOT attribute list
(def (format-attrs attrs)
  (if (null? attrs) ""
    (string-append
      "["
      (string-join
        (map (lambda (kv)
               (format "~a=\"~a\""
                       (car kv)
                       (escape-dot-string (format "~a" (cdr kv)))))
             attrs)
        ", ")
      "]")))

;; Convert plist to alist, converting keywords to symbols
(def (plist->alist plist)
  (let loop ((rest plist) (acc '()))
    (if (null? rest) (reverse acc)
      (if (null? (cdr rest))
        (error "Odd-length plist" plist)
        (let ((key (car rest)))
          (loop (cddr rest)
                (cons (cons (if (keyword? key) (keyword->symbol key) key)
                           (cadr rest))
                      acc)))))))

;; DOT node statement
(def (node id . attrs)
  (let* ((alist (if (and (pair? attrs) (keyword? (car attrs)))
                  (plist->alist attrs)
                  attrs))
         (label-pair (assoc 'label alist))
         (label (if label-pair label-pair (cons 'label (symbol->string id)))))
    (cons 'node (cons id (cons label (filter (lambda (x) (not (eq? (car x) 'label))) alist))))))

;; DOT edge statement
(def (edge from to . attrs)
  (let ((alist (if (and (pair? attrs) (keyword? (car attrs)))
                 (plist->alist attrs)
                 attrs)))
    (cons 'edge (cons from (cons to alist)))))

;; DOT attribute statement
(def (attr type . attrs)
  (let ((alist (if (and (pair? attrs) (keyword? (car attrs)))
                 (plist->alist attrs)
                 attrs)))
    (cons 'attr (cons type alist))))

;; DOT subgraph
(def (subgraph name . body)
  (cons 'subgraph (cons name body)))

;; DOT graph (undirected)
(def (graph name . body)
  (cons 'graph (cons name body)))

;; DOT digraph (directed)
(def (digraph name . body)
  (cons 'digraph (cons name body)))

;; Convert DOT S-expression to string
(def (dot->string graph-expr (indent 0))
  (def (ind) (make-string (* indent 2) #\space))

  (cond
    ((and (pair? graph-expr) (eq? (car graph-expr) 'digraph))
     (let ((name (cadr graph-expr))
           (body (cddr graph-expr)))
       (string-append
         "digraph " (symbol->string name) " {\n"
         (string-join (map (lambda (x) (dot->string x (+ indent 1))) body) "\n")
         "\n}\n")))

    ((and (pair? graph-expr) (eq? (car graph-expr) 'graph))
     (let ((name (cadr graph-expr))
           (body (cddr graph-expr)))
       (string-append
         "graph " (symbol->string name) " {\n"
         (string-join (map (lambda (x) (dot->string x (+ indent 1))) body) "\n")
         "\n}\n")))

    ((and (pair? graph-expr) (eq? (car graph-expr) 'subgraph))
     (let ((name (cadr graph-expr))
           (body (cddr graph-expr)))
       (string-append
         (ind) "subgraph " (symbol->string name) " {\n"
         (string-join (map (lambda (x) (dot->string x (+ indent 1))) body) "\n")
         "\n" (ind) "}")))

    ((and (pair? graph-expr) (eq? (car graph-expr) 'node))
     (let ((id (cadr graph-expr))
           (label (caddr graph-expr))
           (attrs (cdddr graph-expr)))
       (string-append
         (ind) (symbol->string id) " "
         (format-attrs (cons label attrs)) ";")))

    ((and (pair? graph-expr) (eq? (car graph-expr) 'edge))
     (let ((from (cadr graph-expr))
           (to (caddr graph-expr))
           (attrs (cdddr graph-expr)))
       (string-append
         (ind) (symbol->string from) " -> " (symbol->string to) " "
         (format-attrs attrs) ";")))

    ((and (pair? graph-expr) (eq? (car graph-expr) 'attr))
     (let ((type (cadr graph-expr))
           (attrs (cddr graph-expr)))
       (string-append
         (ind) (symbol->string type) " "
         (format-attrs attrs) ";")))

    (else
     (error "Unknown DOT expression" graph-expr))))

;; Convert JSON graph description to DOT S-expression
(def (json->dot json)
  (let* ((graph-type (string->symbol (or (hash-get json "graph_type") "digraph")))
         (name (string->symbol (or (hash-get json "name") "G")))
         (nodes (or (hash-get json "nodes") []))
         (edges (or (hash-get json "edges") []))
         (clusters (or (hash-get json "clusters") []))
         (graph-attrs (or (hash-get json "graph_attrs") []))
         (node-attrs (or (hash-get json "node_attrs") []))
         (edge-attrs (or (hash-get json "edge_attrs") [])))

    ;; Build body
    (let ((body
           (append
             ;; Global graph attributes
             (if (null? graph-attrs) '()
               (list (apply attr (cons 'graph
                                  (map (lambda (kv)
                                         (cons (string->symbol (car kv)) (cdr kv)))
                                       (hash->list graph-attrs))))))

             ;; Default node attributes
             (if (null? node-attrs) '()
               (list (apply attr (cons 'node
                                  (map (lambda (kv)
                                         (cons (string->symbol (car kv)) (cdr kv)))
                                       (hash->list node-attrs))))))

             ;; Default edge attributes
             (if (null? edge-attrs) '()
               (list (apply attr (cons 'edge
                                  (map (lambda (kv)
                                         (cons (string->symbol (car kv)) (cdr kv)))
                                       (hash->list edge-attrs))))))

             ;; Clusters (subgraphs)
             (map (lambda (cluster)
                    (let* ((cluster-name (string->symbol (hash-ref cluster "name")))
                           (cluster-label (hash-get cluster "label"))
                           (cluster-nodes-raw (or (hash-get cluster "nodes") []))
                           ;; Nodes can be either strings (IDs) or hash-tables (full definitions)
                           (cluster-nodes
                            (map (lambda (n)
                                   (cond
                                     ;; If it's a string, just use it as ID
                                     ((string? n) (node (string->symbol n)))
                                     ;; If it's a hash-table, it's a full node definition
                                     ((hash-table? n)
                                      (let* ((id (string->symbol (hash-ref n "id")))
                                             (label (hash-get n "label"))
                                             (attrs (filter (lambda (kv)
                                                            (not (member (car kv) '("id" "label"))))
                                                           (hash->list n))))
                                        (apply node
                                               (cons id
                                                     (append
                                                       (if label (list (cons 'label label)) '())
                                                       (map (lambda (kv)
                                                              (cons (string->symbol (car kv)) (cdr kv)))
                                                            attrs))))))
                                     (else (error "Invalid node in cluster" n))))
                                 cluster-nodes-raw))
                           ;; Collect all attributes (direct properties + attrs hash)
                           (reserved-keys '("name" "label" "nodes" "attrs"))
                           (direct-attrs (filter (lambda (kv)
                                                  (not (member (car kv) reserved-keys)))
                                                (hash->list cluster)))
                           (attrs-hash (or (hash-get cluster "attrs") #f))
                           (all-attrs (if attrs-hash
                                        (append direct-attrs (hash->list attrs-hash))
                                        direct-attrs)))
                      (apply subgraph
                             (cons cluster-name
                                   (append
                                     ;; Cluster attributes
                                     (if (or cluster-label (not (null? all-attrs)))
                                       (list (apply attr
                                              (cons 'graph
                                                    (append
                                                      (if cluster-label
                                                        (list (cons 'label cluster-label))
                                                        '())
                                                      (map (lambda (kv)
                                                             (cons (string->symbol (car kv)) (cdr kv)))
                                                           all-attrs)))))
                                       '())
                                     ;; Nodes in cluster
                                     cluster-nodes)))))
                  clusters)

             ;; Nodes
             (map (lambda (n)
                    (let* ((id (string->symbol (hash-ref n "id")))
                           (label (hash-get n "label"))
                           (attrs (filter (lambda (kv)
                                           (not (member (car kv) '("id" "label"))))
                                         (hash->list n))))
                      (apply node
                             (cons id
                                   (append
                                     (if label (list (cons 'label label)) '())
                                     (map (lambda (kv)
                                            (cons (string->symbol (car kv)) (cdr kv)))
                                          attrs))))))
                  nodes)

             ;; Edges
             (map (lambda (e)
                    (let* ((from (string->symbol (hash-ref e "from")))
                           (to (string->symbol (hash-ref e "to")))
                           (attrs (filter (lambda (kv)
                                           (not (member (car kv) '("from" "to"))))
                                         (hash->list e))))
                      (apply edge
                             (cons from
                                   (cons to
                                         (map (lambda (kv)
                                                (cons (string->symbol (car kv)) (cdr kv)))
                                              attrs))))))
                  edges))))

      ;; Build graph
      (apply (if (eq? graph-type 'digraph) digraph graph)
             (cons name body)))))

;; Render DOT to output string using graphviz FFI
(def (render-dot dot-string
                 engine: (engine 'dot)
                 format: (format 'svg))
  (unless (member engine graphviz-engines)
    (error "Unknown graphviz engine" engine))
  (let ((g (read-dot-string dot-string)))
    (graph->string g
      format: (symbol->string format)
      engine: (symbol->string engine))))

;; Render DOT directly to file using graphviz FFI
(def (render-dot-to-file dot-string output-file
                         engine: (engine 'dot)
                         format: (format 'svg))
  (unless (member engine graphviz-engines)
    (error "Unknown graphviz engine" engine))
  (let ((g (read-dot-string dot-string)))
    (graph->file g output-file
      format: (symbol->string format)
      engine: (symbol->string engine)))
  output-file)
