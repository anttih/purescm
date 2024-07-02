#!chezscheme
(library (Chez foreign)
  (export argv
          glob
          (rename (exit_ exit))
          writeUTF8TextFile
          readUTF8TextFile
          copyFile
          (rename (system_ system)))
  (import (chezscheme)
          (purs runtime pstring))

  (define libc-init
    (case (machine-type)
      ; Not tested at all with Windows
      ; [(i3nt ti3nt a6nt ta6nt) (load-shared-object "libpcre2-16.dll")]
      [(i3osx ti3osx a6osx ta6osx arm64osx tarm64osx) (load-shared-object "libc.dylib")]
      [(i3le ti3le a6le ta6le arm64le tarm64le) (load-shared-object "libc.so.6")]
      [else (error "purescm"
                   (format "Failed to load libc machine-type ~s not supported." (machine-type)))]))

  (define libc_strlen
    (foreign-procedure "strlen" ((* char)) size_t))

  (define-ftype glob_t
    (struct [gl_pathc size_t]
            [gl_pathv (* (* char))]
            [gl_offs size_t]))

  (define libc_glob (foreign-procedure "glob" (string int uptr (* glob_t)) int))

  (define (glob pattern)
    (let* ([globbuf (make-ftype-pointer glob_t (foreign-alloc (ftype-sizeof glob_t)))]
           [res (libc_glob (pstring->string pattern) 0 0 globbuf)]
           [count (ftype-ref glob_t (gl_pathc) globbuf)]
           [paths (let recur ([i 0] [ps '()])
                    (if (fx< i count)
                      (let* ([s (ftype-&ref glob_t (gl_pathv i *) globbuf)]
                             [s-len (libc_strlen s)]
                             [str (char*->pstring s s-len)])
                        (recur (fx1+ i) (cons str ps)))
                      ps))])
      paths))

  (define (char*->pstring fptr len)
    (define bb (make-bytevector len))
    (let f ([i 0])
      (if (fx<? i len)
        (let ([c (ftype-ref char () fptr i)])
          (bytevector-u8-set! bb i (char->integer c))
          (f (fx+ i 1)))))
    (string->pstring (utf8->string bb)))

  (define argv
    (lambda () (map (lambda (arg) (string->pstring arg)) (command-line))))

  (define exit_ exit)

  (define (readUTF8TextFile path)
    (let* ([ip (open-file-input-port (pstring->string path))]
           [buflen (expt 2 16)]
           [buf (make-bytevector buflen)])
      (let-values ([(op extract) (open-bytevector-output-port)])
        (let loop ()
          (let ([n (get-bytevector-n! ip buf 0 buflen)])
            (unless (eof-object? n)
              (put-bytevector op buf 0 n)
              (loop))))
        (close-input-port ip)
        ; TODO avoid double allocation
        (string->pstring (utf8->string (extract))))))

  (define (writeUTF8TextFile path content)
    (let ([op (open-file-output-port
                (pstring->string path)
                (file-options no-fail)
                'block
                (make-transcoder (utf-8-codec)))]
          ; TODO fix double allocation by writing pstring directly to a utf-8 bytevector
          [output (pstring->string content)])
      (block-write op output)
      (close-output-port op)))

(define (copyFile from to)
  (let* ([ip (open-file-input-port (pstring->string from))]
         [op (open-file-output-port (pstring->string to) (file-options no-fail))]
         [buflen (expt 2 16)]
         [buf (make-bytevector buflen)])
    (let loop ()
      (let ([n (get-bytevector-n! ip buf 0 buflen)])
        (unless (eof-object? n)
          (put-bytevector op buf 0 n)
          (loop))))
    (close-output-port op)
    (close-input-port ip)
    '()))

  (define (system_ cmd)
    (system (pstring->string cmd)))

  )
