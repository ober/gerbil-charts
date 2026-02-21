;;; -*- Gerbil -*-
;;; YAML support for gerbil-charts
;;; Uses gerbil-libyaml for native YAML parsing
(export
  yaml-string->json
  yaml-file->json
  yaml-available?)

(import :clan/text/yaml)

;; YAML is always available via gerbil-libyaml
(def (yaml-available?) #t)

;; Convert YAML string to hash-table
(def (yaml-string->json yaml-str)
  (car (yaml-load-string yaml-str)))

;; Convert YAML file to hash-table
(def (yaml-file->json filename)
  (car (yaml-load filename)))
