;; -*- geiser-scheme-implementation: racket -*-
#lang racket

(require racket/vector)

(require ffi/unsafe
         ffi/unsafe/define)

(require "common.rkt")

(provide
 polygon
 polygon-rings
 find-polygons
 find-polygons-cached)

(define-ffi-definer define-shp (ffi-lib "libshp.so.1"))

;;;
;;; FFI data structures
;;;
(define _SAFile _intptr)

(define-cstruct _SAHooks
  ([FOpen _pointer] [FRead _pointer]  [FWrite _pointer] [FSeek _pointer]
   [FTell _pointer] [FFlush _pointer] [FClose _pointer] [Remove _pointer]
   [Error _pointer] [Atof _pointer]))

(define-cstruct _SHPInfo
  ([sHooks _SAHooks]
   [fpSHP _SAFile] [fpSHX _SAFile]
   [nShapeType _int]

   [nFileSize _uint]
   [nRecords _int]
   [nMaxRecords _int]
   [panRecOffset _uintptr]
   [panRecSize _uintptr]

   [adBoundsMin (_array _double 4)]
   [adBoundsMax (_array _double 4)]

   [bUpdated _int]
   [pabyRec _bytes]
   [nBufSize _int]))

(define _SHPHandle _SHPInfo-pointer)


(define-cstruct _SHPObject
  ([nSHPType _int]
   [nShapeId _int] ; -1 is unknown/unassigned
   [nParts _int]
   [panPartStart _pointer]
   [panPartType _pointer]

   [nVertices _int]
   [padfX _pointer] [padfY _pointer] [padfZ _pointer] [padfM _pointer]                        
   [dfXMin _double] [dfYMin _double] [dfZMin _double] [dfMMin _double]
   [dfXMax _double] [dfYMax _double] [dfZMax _double] [dfMMax _double]

   [bMeasureIsUsed _int]))


(define-cstruct _SHPTreeNode
  ([adfBoundsMin (_array _double 4)]
   [adfBoundsMax (_array _double 4)]

   [nShapeCount _int]
   [panShapeIds _intptr]
   [papsShapeObj _pointer] ; _SHPObject-pointer-pointer

   [nSubNodes _int]
   [apsSubNode (_array _pointer 4)]))

(define-cstruct _SHPTree
  ([hSHP _SHPHandle]
   [nMaxDepth _int]
   [nDimension _int]
   [nTotalCount _int]
   [psRoot _SHPTreeNode-pointer]))

(define-shp SHPOpen (_fun _string _string -> _SHPInfo-pointer))
(define-shp SHPClose (_fun _SHPInfo-pointer -> _void))
(define-shp SHPReadObject (_fun _SHPInfo-pointer _int -> _SHPObject-pointer))
(define-shp SHPDestroyObject (_fun _SHPObject-pointer -> _void))

(define-shp SHPSearchDiskTree
  (_fun _pointer (_array/list _double 4) (_array/list _double 4) _intptr -> _pointer))

;; C standard library for FILE* handling
(define stdlib (ffi-lib #f))
(define fopen (get-ffi-obj "fopen" stdlib (_fun _string _string -> _pointer)))
(define fclose (get-ffi-obj "fclose" stdlib (_fun _pointer -> _int)))

;; Structs containing types
(define (resolve-part-type number-identifier)
  (match number-identifier
    [0 'TRISTRIP]
    [1 'TRIFAN]
    [2 'OUTERRING]
    [3 'INNERRING]
    [4 'FIRSTRING]
    [5 'RING]))

(define (resolve-shape-type number-identifier)
  (match number-identifier
    [0 'NULL]
    [1 'POINT]
    [3 'ARC]
    [5 'POLYGON]
    [8 'MULTIPOINT]
    [11 'POINTZ]
    [13 'ARCZ]
    [15 'POLYGONZ]
    [18 'MULTIPOINTZ]
    [21 'POINTM]
    [23 'ARCM]
    [25 'POLYGONM]
    [28 'MULTIPOINTM]
    [31 'MULTIPATCH]))

;;; Actual functionality
(define-struct polygon (min-coordinate max-coordinate rings))

(define (extract-part-vertices shape start end)
  (stream-map
   (lambda (index)
     (extract-vertex-coordinates shape index))
   (in-range start (+ 1 end))))

(define (extract-vertex-coordinates shape vertex-index)
  (let ([x (ptr-ref (SHPObject-padfX shape) _double vertex-index)]
        [y (ptr-ref (SHPObject-padfY shape) _double vertex-index)])
    (lonlat x y)))

(define (index-lookup index-file-path min-coordinate max-coordinate)
  (let* ([min-list (list (lonlat-longitude min-coordinate) (lonlat-latitude min-coordinate) 0.0 0.0)]
         [max-list (list (lonlat-longitude max-coordinate) (lonlat-latitude max-coordinate) 0.0 0.0)]
         [shape-count-ptr (malloc (ctype-sizeof _int) 'atomic)] ; [shape-count-ptr (malloc (ctype-sizeof _int) 'raw)]
         [index-file (fopen index-file-path "rb")]
         [result (SHPSearchDiskTree index-file min-list max-list (cast shape-count-ptr _pointer _intptr))]
	 [shape-count (ptr-ref shape-count-ptr _int)])
    ;; (free shape-count-ptr)
    (fclose index-file)
    (stream->list (stream-map (lambda (index) (ptr-ref result _int index)) (in-range shape-count)))))

(define (extract-polygon-ring shape-pointer part-index)
  (let ([shape-type (resolve-shape-type (SHPObject-nSHPType shape-pointer))])
    (unless (eq? shape-type 'POLYGON)
      (error (format "Shape type ~a is not supported" shape-type))))
  
  (let* ([part-start (ptr-ref (SHPObject-panPartStart shape-pointer) _int part-index)]
         [part-count (SHPObject-nParts shape-pointer)]
         [part-vertices (SHPObject-nVertices shape-pointer)]
         [next-part-start (ptr-ref (SHPObject-panPartStart shape-pointer) _int (+ 1 part-index))]
         [part-end (if (= part-index (- part-count 1))
                       (- part-vertices 1)
                       (- next-part-start 1))]
         [type (resolve-part-type (ptr-ref (SHPObject-panPartType shape-pointer) _int))])

    (stream->list
     (extract-part-vertices shape-pointer part-start part-end))))

(define (extract-polygon shape-file-handle shape-index)
  (let* ([shape-pointer (SHPReadObject shape-file-handle shape-index)]
	 [part-count (SHPObject-nParts shape-pointer)]
	 [shape-min-lat (SHPObject-dfYMin shape-pointer)]
	 [shape-min-lon (SHPObject-dfXMin shape-pointer)]
	 [shape-max-lat (SHPObject-dfYMax shape-pointer)]
	 [shape-max-lon (SHPObject-dfXMax shape-pointer)]
	 [rings (stream->list
		 (stream-map
		  (lambda (part-index)
		    (extract-polygon-ring shape-pointer part-index))
		  (in-range part-count)))])
    (SHPDestroyObject shape-pointer)
    (polygon (lonlat shape-min-lon shape-min-lat)
	     (lonlat shape-max-lon shape-max-lat)
	     rings)))

;; Exported functions
(define (find-polygons shape-file index-file-path min-coordinate max-coordinate)
  (let* ([shape-file-handle (SHPOpen shape-file "rb")]
	 [indices (index-lookup index-file-path min-coordinate max-coordinate)])
    (let ([result (map (lambda (index) (extract-polygon shape-file-handle index)) indices)])
      (SHPClose shape-file-handle)
      result)))

(define polygon-cache (make-weak-hash))

(define (find-polygons-cached shape-file index-file-path min-coordinate max-coordinate)
  (let* ([shape-file-handle (SHPOpen shape-file "rb")]
         [indices (index-lookup index-file-path min-coordinate max-coordinate)])

    (let ([result (map
                   (lambda (index)
                     (hash-ref! polygon-cache index
                                (thunk
                                 (extract-polygon shape-file-handle index))))
                   indices)])
      (SHPClose shape-file-handle)
      result)))
