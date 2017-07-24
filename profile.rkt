#lang typed/racket/base

(provide (all-defined-out))

(require "base.rkt")
(require "layer.rkt")

#;(define psd-profile : (->* () (Output-Port) Void)
  (lambda [[out (current-output-port)]]
    (fprintf out (foldr string-append ""
                        (add-between (list "~a Object:" "Size: [~a * ~a]" "Channels: ~a" "Depth: ~a" "Color Mode: ~a"
                                           "Compression Method: ~a" "Resources Count: ~a~a" "Global Mask: ~a" "Tagged Blocks: ~a~a~n")
                                     "~n    "))
             (case ~version [{1} 'PSD] [{2} 'PSB]) ~width ~height ~channels ~depth
             (list-ref '{Bitmap Grayscale Indexed RGB CMYK Multichannel Duotone Lab} ~color-mode)
             (list-ref '{Raw RLE ZIP-no-prediction ZIP-with-prediction} ~compression)
             (hash-count image-resources) (hash-keys image-resources)
             (cond [(zero? (hash-count global-mask)) "None"]
                   [(= 128 (hash-ref global-mask 'kind)) "Use value stored per layer"]
                   [else global-mask])
             (hash-count tagged-blocks) (hash-keys tagged-blocks))
    
    (fprintf out "~n    Layer Count: ~a~n" (vector-length layers))
    (for ([index (in-range (sub1 (vector-length layers)) -1 -1)])
      (send (vector-ref layers index) folder?)
      (send (vector-ref layers index) desc "        " out)
      (newline))))

#;(define psd-layer-profile : (->* (String) (Output-Port) Void)
  (lambda [[prefix ""] [out (current-output-port)]]
    (define frmt (foldr string-append ""
                        (add-between (list (string-append prefix "Layer Object: ~a") "Rectangle: (~a, ~a, ~a, ~a)" "Channels: ~a"
                                           "Layer Type: ~a" "Blend: ~a" "Opacity: ~a" "Clipping: ~a" "Flags: #b~b" "Mask/Adjustment: ~a"
                                           "Additional Info: ~a~a~n")
                                     (string-append "~n" prefix "    "))))
    (fprintf out frmt (get-name) top left bottom right (length channels)
             (list-ref '{"Normal" "Open Folder" "Closed Folder" "Folder Boundary"} (get-type))
             blend opacity clipping flags mask
             (hash-count additional-info) (hash-keys additional-info))))
