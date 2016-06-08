; -*- coding: iso-8859-1; mode: Lisp; indent-tabs-mode: nil; -*-

; ===================================================================

; Halbruegge, M., Quade, M. & Engelbrecht, K.-P. (2016). Cognitive 
; Strategies in HCI and Their Implications on User Error. In Proc. 
; CogSci 2016

; -------------------------------------------------------------------

; Abstract: Human error while performing well-learned tasks on a computer
; is an infrequent, but pervasive problem. Such errors are
; often attributed to memory deficits, such as loss of activation or
; interference with other tasks (Altmann & Trafton, 2002). We
; are arguing that this view neglects the role of the environment.
; As embodied beings, humans make extensive use of external
; cues during the planning and execution of tasks. In this paper,
; we study how the visual interaction with a computer interface
; is linked to user errors. Gaze recordings confirm our hypothesis
; that the use of the environment increases when memory
; becomes weak. An existing cognitive model of sequential action
; and procedural error (Halbrügge, Quade, & Engelbrecht,
; 2015) is extended to account for the observed gaze behavior.

; ===================================================================

; HOW TO RUN

; The model depends on the "lean embedded lisp" sub-project of ACT-CV
; http://act-cv.sourceforge.net/

; If you want to use it without ACT-CV, you need to fill the visicon 
; and audicon and handle clicks and shouts by yourself.

; ===================================================================

; Copyright (c) 2016 Marc Halbruegge (marc.halbruegge@tu-berlin.de)

#|
This library is free software; you can redistribute it and/or
modify it under the terms of the GNU Lesser General Public
License version 2.1 as published by the Free Software Foundation.

This library is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
Lesser General Public License for more details.

You should have received a copy of the GNU Lesser General Public
License along with this library; if not, write to the Free Software
Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
|#

(defun set-buffer-only (event)
  (eq (evt-action event) 'set-buffer-chunk))
; :trace-filter production-firing-only

(clear-all)

(defvar *give-up-ticks*)
(setf *give-up-ticks* 15)

(define-model pipe-dev-test

; :egs utility noise
; :iu initial utility (user defined productions)
; :nu initial utility (learned productions)
; :epl production compilation

; :esc decl mem activation
; :er random chunk if several match
; :bll [.5] base level learning -> time dependent activation decay, higher value means faster decay
; :blc [0.0] base level activation -> constant to be added to the activation computation for every chunk
; :mas [2] spreading activation, bigger means more added activation is possible
; :mp [1] partial matching, bigger means similarity is more important
; :ans activation noise
; :rt [0] retrieval threshold -> min activation
; :nsji allow/warn negative values in the fan calculation?
; :act activation trace

; :cursor-noise [nil] mouse cursor noise
(sgp :v nil :trace-filter production-firing-only :trace-detail medium :show-focus t
     :iu 0 :egs .5 :ul t
     :epl nil
     :imaginal-activation 1.0
     :aural-activation 1.0
     :mp nil
     :mas 6
     :esc t
     :bll .1 :blc .75
     :er t :ans nil :nsji nil :act nil
     :cursor-noise nil)

; allow parametrization before declarative mem is being filled
(eval (read-from-string (EvalToString "(PARENT-SGP)")))

(chunk-type recipe main prev info action order state worldmark)
(chunk-type meal main s1 s2 s3 s4) ; put some info about the task into the imaginal buffer
(chunk-type mealslot name)
(chunk-type number)
(chunk-type countorder n1 n2)

(add-dm
 (init isa chunk)
 (start isa chunk)
 (try-retrieve isa chunk)
 (retrieve-loc isa chunk)
 (find isa chunk)
 (attending isa chunk)
 (encoding isa chunk)
 (find-next isa chunk)
 (prepare-mouse isa chunk)
 (wait-click isa chunk)
 (retrieve-next isa chunk)
 (tstimag1 isa chunk)
 (tstimag2 isa chunk)
 (tstimag3 isa chunk)
 (tstimag4 isa chunk)
 (RE-RETRIEVE isa chunk)
 (check-world-start isa chunk)
 (CHECK-WORLD-SEARCH isa chunk)
 (STOPPING isa chunk)
 (RETRIEVE-ORDER isa chunk)
 (CHECK-WORLD-TRY isa chunk)

 (s1 isa mealslot name s1)(s2 isa mealslot name s2)(s3 isa mealslot name s3)(s4 isa mealslot name s4)

 (one isa number)
 (two isa number)
 (three isa number)
 (four isa number)
 (five isa number)
 (six isa number)
 (seven isa number)
 (eight isa number)
 (nine isa number)
 (ten isa number)
 (eleven isa number)
 (twelve isa number)
 (thirteen isa number)
 (fourteen isa number)
 (fifteen isa number)

 (nilone isa countorder n2 one)
 (onetwo isa countorder n1 one n2 two)
 (twothree isa countorder n1 two n2 three)
 (threefour isa countorder n1 three n2 four)
 (fourfive isa countorder n1 four n2 five)
 (fivesix isa countorder n1 five n2 six)
 (sixseven isa countorder n1 six n2 seven)
 (seveneight isa countorder n1 seven n2 eight)
 (eightnine isa countorder n1 eight n2 nine)
 (nineten isa countorder n1 nine n2 ten)
 (teneleven isa countorder n1 ten n2 eleven)
 (eleventwelve isa countorder n1 eleven n2 twelve)
 (twelvethirteen isa countorder n1 twelve n2 thirteen)
 (thirfourteen isa countorder n1 thirteen n2 fourteen)
 (fourfivteen isa countorder n1 fourteen n2 fifteen)

 (GO isa chunk)
 (goal isa recipe state init)

 (push isa chunk)(on isa chunk)(off isa chunk)
 (end-of-trial isa chunk)(ask-experimenter isa chunk)
 )
(sdp :reference-count 5)

; set-similarities: 0 -> maximum similarity; -1 -> maximum difference
(defun simset100 (l)
  (if (cadr l)
      (progn (set-similarities-fct (list (list (car l)(cadr l) -1)))
             (simset100 (cdr l)))))
(defun simset050 (l)
  (if (cadr l)
      (progn (set-similarities-fct (list (list (car l)(cadr l) -.5)))
             (simset050 (cdr l)))))
(defun simset025 (l)
  (if (cadr l)
      (progn (set-similarities-fct (list (list (car l)(cadr l) -.25)))
             (simset025 (cdr l)))))
(defun simset (l v)
  (if (cadr l)
      (progn (set-similarities-fct (list (list (car l)(cadr l) v)))
             (simset (cdr l) v))))
(defun simset2ndorder (l)
  (if (caddr l)
      (progn (set-similarities-fct (list (list (car l)(caddr l) -.75)))
             (simset2ndorder (cdr l)))))
(let ((nums '(one two three four five six seven eight nine ten eleven twelve thirteen fourteen))
      (orders '(nilone onetwo twothree threefour fourfive fivesix sixseven seveneight eightnine nineten teneleven eleventwelve twelvethirteen thirfourteen fourfivteen))
      (entities '(push on off end-of-trial ask-experimenter s1 s2 s3 s4)))
  (progn
    (sdp-fct (list nums :reference-count 20000))
    (sdp-fct (list orders :reference-count 10000))
    (sdp-fct (list entities :reference-count 20000))
    ;(simset nums)
    ;(simset orders)
))

; allow parametrization of chunks in declarative memory
; COMMENT OUT for running outside of ACT-CV
(eval (read-from-string (EvalToString "(PARENT-SDP)")))


(p start-trial
  =goal>
    isa recipe
    state start
    main =meal
  =imaginal>
    isa meal
==>
  ;+imaginal> =meal
  =imaginal> ;keep it in!

  =goal>
    state find ; find it without using decl mem

  ;!eval! (dm)
  ;!eval! (pprint-chunks-fct (list =imaginal))
)

(p find-actual-object
  =goal>
    isa recipe
    state find
    info =val
==>
  +visual-location>
    isa visual-location
    value =val
  =goal>
    state attending
)

(p attend-object
  =goal>
    isa recipe
    state attending

   ; bind it to be able to move the attention
  =visual-location>
    isa visual-location

  ?visual>
    state free
==>
  +visual>
    isa move-attention
    screen-pos =visual-location

  =goal>
    state encoding
)

#|
; give up - todo: re-retrieve and re-start (or scan the gui)
(p find-object-failure-giveup
  =goal>
    isa recipe
    state attending
    info =val
  ?visual-location>
    state error

  ;!bind! =str (format nil "OOPS_~W" =val)
==>
  +vocal>
    isa speak
    string "OOPS1"

  =goal> init ; overwrite to avoid strengthening
  -goal>
  -imaginal>
)
|#

(p find-object-failure-first
  =goal>
    isa recipe
    state attending
    prev nil
  ?visual-location>
    state error
==>
  =goal>
    state check-world-start
    worldmark nil

  !eval! (EvalToVoid (format nil "(START-WORLD-STRATEGY ~W)" (mp-time)))
#|
  ?vocal>
    state free
==>
  +vocal>
    isa speak
    string "OOPS0"

  =goal> init ; overwrite to avoid strengthening
  -goal>
  =imaginal> init
  -imaginal>
|#
)

; don't use order for now (breaks for "Habe ich")
(p find-object-failure-retry
  =goal>
    isa recipe
    state attending
    info =curstep
    prev =prevstep
    main =trialname
  ?visual-location>
    state error
==>
  +retrieval>
    isa recipe
    info =prevstep
    main =trialname
  =goal>
    state re-retrieve

  ;!eval! (print-visicon)
  ;!output! (re-retrieve =prevstep cur =curstep)
)

; go for a knowledge in the world strategy
(p find-object-failure-try-world
  =goal>
    isa recipe
    state attending
    info =curstep
    prev =prevstep
    main =trialname
  ?visual-location>
    state error
==>
  =goal>
    state check-world-start
    worldmark nil

  !eval! (EvalToVoid (format nil "(START-WORLD-STRATEGY ~W)" (mp-time)))
)


(p next-step-re-retrieve
  =goal>
    isa recipe
    main =rec
    prev =val
    state re-retrieve

  =retrieval>
    isa recipe
    main =rec
    prev =prev
    info =val
    action =a
    order =o

  ; experimenter is not talking to me
  ?aural-location>
    state free
    buffer empty
==>
  ; try =goal instead of +goal to avoid strengthening
  =goal>
    main =rec
    prev =prev
    info =val
    action =a
    order =o
    state start

  ;!output! (retry goal =val)
)

; could not re-retrieve correctly
#|
(p next-step-re-retrieve-err
  =goal>
    isa recipe
    main =rec
    prev =prev
    state re-retrieve

  =retrieval>
    isa recipe
    main =rec
    - info =prev
    info =errprev

  ; experimenter is not talking to me
  ?aural-location>
    state free
    buffer empty
==>
  +vocal>
    isa speak
    string "OOPS5"

  =goal> init ; overwrite to avoid strengthening
  -goal>
  =imaginal> init
  -imaginal>

  ;!output! (re-retrieve error =prev =errprev)
)
|#
(p next-step-re-retrieve-err-try-world
  =goal>
    isa recipe
    main =rec
    info =curstep
    prev =prev
    state re-retrieve

  =retrieval>
    isa recipe
    main =rec
    - info =prev
    info =errprev

  ; experimenter is not talking to me
  ?aural-location>
    state free
    buffer empty
==>
  =goal>
    state check-world-start
    worldmark nil

  !eval! (EvalToVoid (format nil "(START-WORLD-STRATEGY ~W)" (mp-time)))
)


; could not re-retrieve correctly
(p next-step-re-retrieve-err2
  =goal>
    isa recipe
    main =rec
    prev =prev
    state re-retrieve

  =retrieval>
    isa recipe
    - main =rec
    info =prev

  ; experimenter is not talking to me
  ?aural-location>
    state free
    buffer empty
  ?vocal>
    state free
==>
  +vocal>
    isa speak
    string "OOPS6"

  =goal> init ; overwrite to avoid strengthening
  -goal>
  =imaginal> init
  -imaginal>
)

; could not re-retrieve correctly
(p next-step-re-retrieve-err3
  =goal>
    isa recipe
    main =rec
    prev =prev
    state re-retrieve

  =retrieval>
    isa recipe
    - main =rec
    - info =prev
  ?vocal>
    state free
==>
  +vocal>
    isa speak
    string "OOPS7"

  =goal> init ; overwrite to avoid strengthening
  -goal>
  =imaginal> init
  -imaginal>
)

; experimenter *is* talking to me
(p next-step-stop-it
  =goal>
    isa recipe

  =aural-location>
    isa audio-event
  ?aural>
    state free
==>
  +aural>
    isa sound
    event =aural-location

  =goal>
    state stopping

  ;!eval! (print-audicon)
)

; experimenter *is* talking to me
(p next-step-stop-it2
  =goal>
    isa recipe
    state stopping

  =aural>
    isa sound
    kind word
    content "STOP"
  ?vocal>
    state free
==>
  +vocal>
    isa speak
    string "STOPPED"

  =goal> init ; overwrite to avoid strengthening
  -goal>
  =imaginal> init
  -imaginal>

  ;!eval! (print-audicon)
)

(p next-step-re-retrieve2
  =goal>
    isa recipe
    main =rec
    info =prev
    state re-retrieve

  =retrieval>
    isa recipe
    main =rec
    prev nil
    info =val
    action =a
    order =o
==>
  =goal>
    ;isa recipe
    main =rec
    prev nil
    info =val
    action =a
    order =o
    state start

   !output! (retry goal =val)
)


; give up - todo: re-retrieve and re-start (or scan the gui)
(p find-object-final-giveup
  =goal>
    isa recipe
    state re-retrieve

  ?retrieval>
    state error
  ?vocal>
    state free
==>
  +vocal>
    isa speak
    string "OOPS4"

  =goal> init ; overwrite to avoid strengthening
  -goal>
  =imaginal> init
  -imaginal>
)

; give up - todo: re-retrieve and re-start
; not reached currently
(p encode-object-failure
  =goal>
    isa recipe
    state encoding
  ?visual>
    state error
  ?vocal>
    state free
==>
  +vocal>
    isa speak
    string "OOPS2"

  =goal> init ; overwrite to avoid strengthening
  -goal>
  =imaginal> init
  -imaginal>
)

; give up - todo: re-retrieve and re-start
; not reached currently
(p encode-search-failure
  =goal>
    isa recipe
    state encoding
    info =val
  =visual>
    isa text
    - value =val
  ?vocal>
    state free
==>
  +vocal>
    isa speak
    string "OOPS3"

  =goal> init ; overwrite to avoid strengthening
  -goal>
  =imaginal> init
  -imaginal>
)

(p encode-obj-need-on
   =goal>
      isa recipe
      state encoding
      action on
      info =val
   =visual>
      isa text
      value =val
      screen-pos =pos
      color black-white

   ?manual>
      state free
==>
   =goal>
      state prepare-mouse
   +manual>
      isa move-cursor
      loc =pos
)

(p encode-obj-already-on
   =goal>
      isa recipe
      state encoding
      action on
      order =order
   =visual>
      isa text
      value =val
      - color black-white

==>
   +retrieval>
      isa countorder
      n1 =order
      :recently-retrieved nil

   =goal>
      state retrieve-order
)
(p encode-obj-need-off
   =goal>
      isa recipe
      state encoding
      action off
      info =val
   =visual>
      isa text
      value =val
      screen-pos =pos
      - color black-white

   ?manual>
      state free
==>
   =goal>
      state prepare-mouse
   +manual>
      isa move-cursor
      loc =pos
)

(p encode-obj-already-off
   =goal>
      isa recipe
      state encoding
      action off
      order =order
   =visual>
      isa text
      value =val
      color black-white

==>
   +retrieval>
      isa countorder
      n1 =order
      :recently-retrieved nil

   =goal>
      state retrieve-order
)
; Todo: don't use the order
(p encode-obj-push
   =goal>
      isa recipe
      state encoding
      action push
      info =val
   =visual>
      isa text
      value =val
      screen-pos =pos

   ?manual>
      state free
==>
   =goal>
      state prepare-mouse
   +manual>
      isa move-cursor
      loc =pos
)

(p click-text
  =goal>
    isa recipe
    state prepare-mouse
  ?manual>
    state free
==>
  =goal>
    state tstimag1
  +manual>
    isa click-mouse
)

(p test-imaginal-1
  =goal>
    isa recipe
    state tstimag1
  =imaginal>
    isa meal
    s1 nil
==>
  =imaginal>
  =goal>
    state tstimag2
)
(p test-imaginal-2
  =goal>
    isa recipe
    state tstimag2
  =imaginal>
    isa meal
    s2 nil
==>
  =imaginal>
  =goal>
    state tstimag3
)
(p test-imaginal-3
  =goal>
    isa recipe
    state tstimag3
  =imaginal>
    isa meal
    s3 nil
==>
  =imaginal>
  =goal>
    state tstimag4
)
(p test-imaginal-4
  =goal>
    isa recipe
    state tstimag4
  =imaginal>
    isa meal
    s4 nil
==>
  =imaginal>
  =goal>
    state wait-click
)
(p test-imaginal-1-mismatch
  =goal>
    isa recipe
    info =info
    state tstimag1
  =imaginal>
    isa meal
    - s1 =info
==>
  =imaginal>
  =goal>
    state tstimag2
)
(p test-imaginal-2-mismatch
  =goal>
    isa recipe
    info =info
    state tstimag2
  =imaginal>
    isa meal
    - s2 =info
==>
  =imaginal>
  =goal>
    state tstimag3
)
(p test-imaginal-3-mismatch
  =goal>
    isa recipe
    info =info
    state tstimag3
  =imaginal>
    isa meal
    - s3 =info
==>
  =imaginal>
  =goal>
    state tstimag4
)
(p test-imaginal-4-mismatch
  =goal>
    isa recipe
    info =info
    state tstimag4
  =imaginal>
    isa meal
    - s4 =info
==>
  =imaginal>
  =goal>
    state wait-click
)
(p test-imaginal-1-match
  =goal>
    isa recipe
    info =info
    state tstimag1
  =imaginal>
    isa meal
    s1 =info
==>
  =imaginal>
    s1 nil
  =goal>
    state wait-click
)
(p test-imaginal-2-match
  =goal>
    isa recipe
    info =info
    state tstimag2
  =imaginal>
    isa meal
    s2 =info
==>
  =imaginal>
    s2 nil
  =goal>
    state wait-click
)
(p test-imaginal-3-match
  =goal>
    isa recipe
    info =info
    state tstimag3
  =imaginal>
    isa meal
    s3 =info
==>
  =imaginal>
    s3 nil
  =goal>
    state wait-click
)
(p test-imaginal-4-match
  =goal>
    isa recipe
    info =info
    state tstimag4
  =imaginal>
    isa meal
    s4 =info
==>
  =imaginal>
    s4 nil
  =goal>
    state wait-click
)
(p after-click-mouse
  =goal>
    isa recipe
    state wait-click
    order =order

  ?manual>
    state free
==>
  +retrieval>
    isa countorder
    n1 =order
    :recently-retrieved nil

  =goal>
    state retrieve-order
)
#|
; don't use the order -> will create errors
(p after-click-mouse-2
  =goal>
    isa recipe
    state wait-click
    main =rec
	info =step

  ?manual>
    state free
==>
  +retrieval>
    isa recipe
    main =rec
	prev =step
	:recently-retrieved nil

  =goal>
    state retrieve-next
)
|#
(p retrieved-order
  =goal>
    isa recipe
    state retrieve-order
    main =rec
    info =step

  =retrieval>
    isa countorder
    n2 =order
==>
  +retrieval>
    isa recipe
    main =rec
    prev =step
    - info =step
    order =order
    :recently-retrieved nil

  =goal>
    state retrieve-next
)
; don't use the order
(p retrieved-order-err
  =goal>
    isa recipe
    state retrieve-order
    main =rec
    info =step

  ?retrieval>
    state error
==>
  +retrieval>
    isa recipe
    main =rec
    prev =step
    - info =step
    :recently-retrieved nil

  =goal>
    state retrieve-next
)

(p retrieved-order-only
  =goal>
    isa recipe
    state retrieve-order
    main =rec
    info =step

  =retrieval>
    isa countorder
    n2 =order
==>
  +retrieval>
    isa recipe
    main =rec
    order =order
    - info =step
    :recently-retrieved nil

  =goal>
    state retrieve-next
)
(p retrieve-next-retry-no-order
  =goal>
    isa recipe
    state retrieve-next
    main =rec
    info =step

  ?retrieval>
    state error
==>
  +retrieval>
    isa recipe
    main =rec
    prev =step
    - info =step
    ;:recently-retrieved nil

  =goal>
)
(p retrieve-next-retry-with-order
  =goal>
    isa recipe
    state retrieve-next
    order =order

  ?retrieval>
    state error
==>
  +retrieval>
    isa countorder
    n1 =order

  =goal>
    state retrieve-order
)

(p retrieve-first-failure-ask
  =goal>
    isa recipe
    state retrieve-next
    main =rec
    info nil

  ?retrieval>
    state error
  ?vocal>
    state free
==>
  +vocal>
    isa speak
    string "REPEAT"

  =goal> init ; overwrite to avoid strengthening
  -goal>
  =imaginal> init
  -imaginal>

  !output! (could not remember =rec)
)

(p retrieve-first-failure-world
  =goal>
    isa recipe
    state retrieve-next
    main =rec
    info nil

  ?retrieval>
    state error
==>
  =goal>
    state check-world-start
    worldmark nil

  !eval! (EvalToVoid (format nil "(START-WORLD-STRATEGY ~W)" (mp-time)))
)


#|
(p retrieve-first-retry-1
  =goal>
    isa recipe
    state retrieve-next
    main =rec
    info nil

  ?retrieval>
    state error
==>
  +retrieval>
    isa recipe
    main =rec
    prev nil
    ;:recently-retrieved nil

  =goal>
)
(p retrieve-first-retry-2
  =goal>
    isa recipe
    state retrieve-next
    main =rec
    info nil

  ?retrieval>
    state error
==>
  +retrieval>
    isa recipe
    main =rec
    order one
    ;:recently-retrieved nil

  =goal>
)
|#

#|
(p retrieve-retry-any
  =goal>
    isa recipe
    state retrieve-next
    main =rec
    - info nil

  ?retrieval>
    state error
==>
  +retrieval>
    isa recipe
    main =rec

  =goal>
)
|#

(p retrieve-check-world-start
  =goal>
    isa recipe
    state retrieve-next
    info =lastval

  ?retrieval>
    state error

==>
  =goal>
    state check-world-start
    worldmark nil

  !eval! (EvalToVoid (format nil "(START-WORLD-STRATEGY ~W)" (mp-time)))
)
(p retrieve-check-world-start-2
  =goal>
    isa recipe
    state retrieve-next
    main =cur
    info =lastval

  =retrieval>
    isa recipe
    main =cur
    prev nil

==>
  =goal>
    state check-world-start
    worldmark nil

  !eval! (EvalToVoid (format nil "(START-WORLD-STRATEGY ~W)" (mp-time)))
)

#|
(p retrieve-check-world-do-start
  =goal>
    isa recipe
    info =curstep
    state check-world-start
==>
  +visual-location>
    isa visual-location
    :attended nil
    :nearest current
    - value =curstep

  =goal>
    state check-world-search
)
|#
(p retrieve-check-world-do-start
  =goal>
    isa recipe
    state check-world-start
==>
  +retrieval>
    isa mealslot
  =goal>
    state check-world-imag
)

(p* check-world-imag
  =goal>
    isa recipe
    state check-world-imag
  =retrieval>
    isa mealslot
    name =slot
  =imaginal>
    isa meal
    =slot nil
==>
  +retrieval>
    isa mealslot
    :recently-retrieved nil
  =imaginal>
  =goal>
)
(p* search-world-imag
  =goal>
    isa recipe
    state check-world-imag
  =retrieval>
    isa mealslot
    name =slot
  =imaginal>
    isa meal
    =slot =val
==>
  +visual-location>
    isa visual-location
    :attended nil
    :nearest current
    value =val

  =imaginal>
  =retrieval>
  =goal>
    state search-world-imag
)
(p* search-world-imag-found
  =goal>
    isa recipe
    main =rec
    state search-world-imag
  =retrieval>
    isa mealslot
    name =slot
  =imaginal>
    isa meal
    =slot =val
  =visual-location>
    isa visual-location
    value =val
==>
  +retrieval>
    isa recipe
    main =rec
    info =val
  =imaginal>
  =goal>
    state retrieve-next
)
(p* search-world-imag-not-found
  =goal>
    isa recipe
    main =rec
    state search-world-imag
  =retrieval>
    isa mealslot
    name =slot
  =imaginal>
    isa meal
    =slot =val
  ?visual-location>
    state error
==>
  +retrieval>
    isa mealslot
    :recently-retrieved nil
  =imaginal>
  =goal>
    state check-world-imag
)
(p search-world-imag-done
  =goal>
    isa recipe
    state check-world-imag
    info =curstep
  ?retrieval>
    state error
==>
  +visual-location>
    isa visual-location
    :attended nil
    :nearest current
    - value =curstep

  =goal>
    state check-world-search
)
(p search-world-imag-done2
  =goal>
    isa recipe
    state check-world-imag
    info nil
  ?retrieval>
    state error
==>
  +visual-location>
    isa visual-location
    :attended nil
    :nearest current

  =goal>
    state check-world-search
)

(p retrieve-check-world-where-imag1
  =goal>
    isa recipe
    main =rec
    state check-world-search

  =visual-location>
    isa visual-location
    value =val
  ?visual>
    state free
  =imaginal>
    isa meal
    s1 =val
==>
  +retrieval>
    isa recipe
    main =rec
    info =val

  =visual-location> ; keep in for spreading activation
  +visual>
    isa move-attention
    screen-pos =visual-location

  =imaginal> ;keep it in!
  =goal>
    state retrieve-next

  !output! (world try imag =val)
)
(p retrieve-check-world-where-imag2
  =goal>
    isa recipe
    main =rec
    state check-world-search

  =visual-location>
    isa visual-location
    value =val
  ?visual>
    state free
  =imaginal>
    isa meal
    s2 =val
==>
  +retrieval>
    isa recipe
    main =rec
    info =val

  =visual-location> ; keep in for spreading activation
  +visual>
    isa move-attention
    screen-pos =visual-location

  =imaginal> ;keep it in!
  =goal>
    state retrieve-next

  !output! (world try imag =val)
)
(p retrieve-check-world-where-imag3
  =goal>
    isa recipe
    main =rec
    state check-world-search

  =visual-location>
    isa visual-location
    value =val
  ?visual>
    state free
  =imaginal>
    isa meal
    s3 =val
==>
  +retrieval>
    isa recipe
    main =rec
    info =val

  =visual-location> ; keep in for spreading activation
  +visual>
    isa move-attention
    screen-pos =visual-location

  =imaginal> ;keep it in!
  =goal>
    state retrieve-next

  !output! (world try imag =val)
)
(p retrieve-check-world-where-imag4
  =goal>
    isa recipe
    main =rec
    state check-world-search

  =visual-location>
    isa visual-location
    value =val
  ?visual>
    state free
  =imaginal>
    isa meal
    s4 =val
==>
  +retrieval>
    isa recipe
    main =rec
    info =val

  =visual-location> ; keep in for spreading activation
  +visual>
    isa move-attention
    screen-pos =visual-location

  =imaginal> ;keep it in!
  =goal>
    state retrieve-next

  !output! (world try imag =val)
)

(p retrieve-check-world-where-init
  =goal>
    isa recipe
    main =rec
    state check-world-search
    worldmark nil

  =visual-location>
    isa visual-location
    value =val
  ?visual>
    state free
==>
  +retrieval>
    isa recipe
    main =rec
    info =val

  =visual-location> ; keep it in

  +visual>
    isa move-attention
    screen-pos =visual-location

  =goal>
    state check-world-try
    worldmark =val

  !output! (world try =val)
)

(p retrieve-check-world-where
  =goal>
    isa recipe
    state check-world-search
    main =rec
    info =cur
    - worldmark =val

  =visual-location>
    isa visual-location
    value =val
    - value =cur
  ?visual>
    state free
==>
  +temporal>
    isa time

  +retrieval>
    isa recipe
    main =rec
    info =val

  =visual-location> ; keep it in

  +visual>
    isa move-attention
    screen-pos =visual-location

  =goal>
    state check-world-try

  !output! (world try =val)
)
(p retrieve-check-world-where-trialstart
  =goal>
    isa recipe
    state check-world-search
    main =rec
    info nil
    - worldmark =val

  =visual-location>
    isa visual-location
    value =val
  ?visual>
    state free
==>
  +retrieval>
    isa recipe
    main =rec
    info =val

  =visual-location> ; keep it in

  +visual>
    isa move-attention
    screen-pos =visual-location

  =goal>
    state check-world-try

  !output! (world try =val)
)
(p retrieve-check-world-where-mm
  =goal>
    isa recipe
    state check-world-search
    info =cur
    - worldmark =val

  =visual-location>
    isa visual-location
    value =val
    - value =cur
  ?visual>
    state free
==>
  +temporal>
    isa time

  +retrieval>
    isa recipe
    info =val

  =visual-location> ; keep it in

  +visual>
    isa move-attention
    screen-pos =visual-location

  =goal>
    state check-world-try

  !output! (world try =val)
)

(p retrieve-check-world-skip-cur
  =goal>
    isa recipe
    state check-world-search
    info =cur

  =visual-location>
    isa visual-location
    value =cur
==>
  +visual-location>
    isa visual-location
    :attended nil
    :nearest clockwise
    - value =cur
)

(p retrieve-check-world-where-done
  =goal>
    isa recipe
    main =rec
    state check-world-search
    worldmark =val

  =visual-location>
    isa visual-location
    value =val
  ?vocal>
    state free
==>
  +vocal>
    isa speak
    string "DONE??"

  =goal> init ; overwrite to avoid strengthening
  -goal>
  =imaginal> init
  -imaginal>

  !output! (world tried all =val)
)
(p retrieve-check-world-where-err
  =goal>
    isa recipe
    main =rec
    state check-world-search
    worldmark =val

  ?visual-location>
    state error
  ?vocal>
    state free
==>
  +vocal>
    isa speak
    string "DONE???"

  =goal> init ; overwrite to avoid strengthening
  -goal>
  =imaginal> init
  -imaginal>

  !output! (world tried all? err)
)
(p retrieve-check-world-where-err-2
  =goal>
    isa recipe
    main =rec
    state check-world-search
    worldmark nil

  ?visual-location>
    state error
  ?vocal>
    state free
==>
  +vocal>
    isa speak
    string "DONE????"

  =goal> init ; overwrite to avoid strengthening
  -goal>
  =imaginal> init
  -imaginal>

  !output! (world tried all? err)
)


(p retrieve-check-world-next
  =goal>
    isa recipe
    state check-world-try
    info =lastval

  ?retrieval>
    state error

  =visual-location>
    isa visual-location
    value =lastworldval
==>
  +visual-location>
    isa visual-location
    :attended nil
    :nearest clockwise
    - value =lastval
    - value =lastworldval

  =goal>
    state check-world-search
)
(p retrieve-check-world-next-trialstart
  =goal>
    isa recipe
    state check-world-try
    info nil

  ?retrieval>
    state error

  =visual-location>
    isa visual-location
    value =lastworldval
==>
  +visual-location>
    isa visual-location
    :attended nil
    :nearest clockwise
    - value =lastworldval

  =goal>
    state check-world-search
)

#|
(p retrieve-check-world-next-err
  =goal>
    isa recipe
    state check-world-try
    info =lastval

  ?visual>
    state error
==>
  +visual-location>
    isa visual-location
    :attended nil
    :nearest clockwise
    - value =lastval

  =goal>
    state check-world-search
)
|#

(p retrieve-check-world-match
  =goal>
    isa recipe
    state check-world-try
    main =rec
    - info =val

  =retrieval>
    isa recipe
    main =rec
    info =val
==>
  =retrieval> ; keep it in

  =goal>
    state retrieve-next

  !output! (world use =val)
)

(p retrieve-check-world-perseveration
  =goal>
    isa recipe
    state check-world-try
    main =rec
    info =val

  =retrieval>
    isa recipe
    main =rec
    info =val
==>
  +visual-location>
    isa visual-location
    :attended nil
    :nearest clockwise
    - value =val

  =goal>
    state check-world-search
)
(p retrieve-check-world-perseveration-ccw
  =goal>
    isa recipe
    state check-world-try
    main =rec
    info =val

  =retrieval>
    isa recipe
    main =rec
    info =val
==>
  +visual-location>
    isa visual-location
    :attended nil
    :nearest counterclockwise
    - value =val

  =goal>
    state check-world-search
)

(p retrieve-check-world-giveup
  ; not optimal, but should work
  !bind! =tickthres *give-up-ticks*
  =goal>
    isa recipe
    state check-world-try
    main =rec
    info =lastval

  =visual-location>
    isa visual-location
    value =val

  =temporal>
    isa time
    ticks =tickthres
==>
  +temporal>
    isa clear

  +visual-location>
    isa visual-location
    :attended nil
    :nearest clockwise
    - value =lastval
    - value =val

  =goal>
    state check-world-search

  !output! (world giveup =val after =tickthres ticks)
)

(p retrieve-check-world-mismatch
  =goal>
    isa recipe
    state check-world-try
    main =rec
    info =lastval

  =retrieval>
    isa recipe
    - main =rec
    info =val
==>
  +visual-location>
    isa visual-location
    :attended nil
    :nearest clockwise
    - value =lastval
    - value =val

  =goal>
    state check-world-search

  !output! (world mismatch =val)
)

; allows intrusions (but not perseverations)
(p retrieve-check-world-allow-mismatch
  =goal>
    isa recipe
    state check-world-try
    - info =val

  =retrieval>
    isa recipe
    info =val
==>
  =retrieval> ; keep it in

  =goal>
    state retrieve-next

  !output! (world use mismatch =val)
)

(p after-click-mouse-last
  =goal>
    isa recipe
    state retrieve-next
    - info nil

  ?retrieval>
    state error
  ;?vocal>
  ;  state free
==>
  =goal>
    state check-world-start
    worldmark nil

  !eval! (EvalToVoid (format nil "(START-WORLD-STRATEGY ~W)" (mp-time)))
#|
  +vocal>
    isa speak
    string "DONE?"

  =goal> init ; overwrite to avoid strengthening
  -goal>
  =imaginal> init
  -imaginal>
|#
)

(p after-click-mouse-stop
  ?goal>
    buffer empty
  =aural>
    isa sound
    kind word
    content "STOP"
==>
  -aural>
  -aural-location>
  ;!eval! (print-audicon)
)


; rely on buffer stuffing
(p attend-any-aural-start
  ?goal>
    buffer empty

  =aural-location>
    isa audio-event

  ?aural>
    state free
    buffer empty
==>
  =aural-location> ; keep chunk in buffer

  +aural>
    isa sound
    event =aural-location

  ;!eval! (print-audicon)
)
(p attend-any-aural-task
  =goal>
    isa meal

  =aural-location>
    isa audio-event

  ?aural>
    state free
    buffer empty
==>
  =aural-location> ; keep chunk in buffer

  +aural>
    isa sound
    event =aural-location

  ;!eval! (print-audicon)
)

(p eat-aural-self
  =aural-location>
    isa audio-event
    location self

  =aural>
    isa sound
    ;event =aural-location ; doesn't work; don't know, why
==>
  -aural>
  -aural-location>

  ;!eval! (print-audicon)
)


(p hear-new-task-1b
  ?goal>
    buffer empty
    state free

  =aural-location>
    isa audio-event
    - location self

  =aural>
    isa sound
    kind speech
    content =trialname

  =imaginal>
    isa meal
==>
  =imaginal> init
  +imaginal>
    isa recipe
    main =trialname

  +goal>
    isa meal
    main =trialname ; activation spreading from the trial name?!
  -aural>
  -aural-location>

  +retrieval>
    isa countorder
    n1 nil

  !output! (next task =trialname)
)
(p hear-new-task-1
  ?goal>
    buffer empty
    state free

  =aural-location>
    isa audio-event
    - location self

  =aural>
    isa sound
    kind speech
    content =trialname

  ?imaginal>
    buffer empty
    state free
==>
  +imaginal>
    isa recipe
    main =trialname

  +goal>
    isa meal
    main =trialname ; activation spreading from the trial name?!
  -aural>
  -aural-location>

  +retrieval>
    isa countorder
    n1 nil

  !output! (next task =trialname)
)
(p hear-new-task-2
  =goal>
    isa meal

  =aural-location>
    isa audio-event
    - location self

  =aural>
    isa sound
    kind speech
    content =info

  =retrieval>
    isa countorder
    n2 =order

  =imaginal>
    isa recipe
    - main nil
    info nil

==>
  =imaginal>
    info =info
    order =order

  +retrieval>
    isa countorder
    n1 =order

  -aural>
  -aural-location>
)
(p hear-new-task-3-on-1
  =goal>
    isa meal
    s1 nil

  =aural-location>
    isa audio-event
    - location self

  =aural>
    isa sound
    kind speech
    content ON

  =imaginal>
    isa recipe
    info =info
    action nil
==>
  =imaginal>
    action ON

   =goal>
     s1 =info

  -aural>
  -aural-location>
)
(p hear-new-task-3-on-2
  =goal>
    isa meal
    s2 nil

  =aural-location>
    isa audio-event
    - location self

  =aural>
    isa sound
    kind speech
    content ON

  =imaginal>
    isa recipe
    info =info
    action nil
==>
  =imaginal>
    action ON

   =goal>
     s2 =info

  -aural>
  -aural-location>
)
(p hear-new-task-3-on-3
  =goal>
    isa meal
    s3 nil

  =aural-location>
    isa audio-event
    - location self

  =aural>
    isa sound
    kind speech
    content ON

  =imaginal>
    isa recipe
    info =info
    action nil
==>
  =imaginal>
    action ON

   =goal>
     s3 =info

  -aural>
  -aural-location>
)
(p hear-new-task-3-on-4
  =goal>
    isa meal
    s4 nil

  =aural-location>
    isa audio-event
    - location self

  =aural>
    isa sound
    kind speech
    content ON

  =imaginal>
    isa recipe
    info =info
    action nil
==>
  =imaginal>
    action ON

   =goal>
     s4 =info

  -aural>
  -aural-location>
)
(p hear-new-task-3-on-x
  =goal>
    isa meal
    - s1 nil
    - s2 nil
    - s3 nil
    - s4 nil

  =aural-location>
    isa audio-event
    - location self

  =aural>
    isa sound
    kind speech
    content ON

  =imaginal>
    isa recipe
    info =info
    action nil
==>
  =imaginal>
    action ON

  -aural>
  -aural-location>
)
(p hear-new-task-3-other
  =goal>
    isa meal

  =aural-location>
    isa audio-event
    - location self

  =aural>
    isa sound
    kind speech
    - content ON
    content =action

  =imaginal>
    isa recipe
    - info nil
    action nil
==>
  =imaginal>
    action =action

  -aural>
  -aural-location>
)
; next step
(p hear-new-task-4
  =goal>
    isa meal

  =aural-location>
    isa audio-event
    - location self

  =aural>
    isa sound
    kind speech
    content =info

  =imaginal>
    isa recipe
    main =trialname
    info =prev
    - action nil

  =retrieval>
    isa countorder
    n2 =order

==>

  +imaginal>
    isa recipe
    main =trialname
    prev =prev
    info =info
    order =order

  +retrieval>
    isa countorder
    n1 =order

  -aural>
  -aural-location>
)
; stop listening, start acting (add terminating task, first)
(p hear-new-task-4b
  =goal>
    isa meal

  =aural-location>
    isa audio-event
    - location self

  =aural>
    isa sound
    kind word
    content "GO"

  =imaginal>
    isa recipe
    main =trialname
    info =prev
    - action nil

  =retrieval>
    isa countorder
    n2 =order
==>
  +imaginal>
    isa recipe
    main =trialname
    prev =prev
    order =order
    info end-of-trial
    action ask-experimenter

  -aural>
  -aural-location>
  -retrieval>
)
(p hear-new-task-5
  =goal>
    isa meal

  =imaginal>
    isa recipe
    main =trialname
    info end-of-trial
    action ask-experimenter
==>
  +imaginal> =goal

  +retrieval>
    isa recipe
    prev nil
    main =trialname
    order one

  =goal> init
  +goal>
    isa recipe
    main =trialname
    state retrieve-next
)
; "meal" chunk is in imaginal buffer for spreading activation
#|
(p next-step-allow-mismatch
  =goal>
    isa recipe
    state retrieve-next
    ;main =rec ; removed to allow more crazy stuff using partial matching
    ;info =step ; remove to allow more crazy stuff using partial matching

  =retrieval>
    isa recipe
    main =rec
    prev =prev
    info =val
    action =a
    order =o
    - info end-of-trial
==>
  =goal>
    ;isa recipe
    main =rec
    prev =prev
    info =val
    action =a
    order =o
    state start

   !output! (next goal =val)
)
|#

(p next-step-perseveration
  =goal>
    isa recipe
    state retrieve-next
    main =rec ; remove to allow more crazy stuff using partial matching
    info =step ; remove to allow more crazy stuff using partial matching
    order =order
  =retrieval>
    isa recipe
    main =rec
    info =step
    - info end-of-trial
==>
  +retrieval>
    isa countorder
    n1 =order

  =goal>
    state retrieve-order
)
(p next-step-perseveration-no-order
  =goal>
    isa recipe
    state retrieve-next
    main =rec
    info =step
  =retrieval>
    isa recipe
    main =rec
    info =step
    - info end-of-trial
==>
  +retrieval>
    isa recipe
    main =rec
    prev =step

  =goal>
)

(p next-step-prev-match
  =goal>
    isa recipe
    state retrieve-next
    ;main =rec ; removed to allow more crazy stuff using partial matching
    info =step ; remove to allow more crazy stuff using partial matching

  =retrieval>
    isa recipe
    main =rec
    prev =step
    info =val
    action =a
    order =o
    - info end-of-trial
==>
  ; keep old goal in dm
  =goal>
    state nil
    worldmark nil
  +goal>
    isa recipe
    main =rec
    prev =step
    info =val
    action =a
    order =o
    state start

   !output! (next goal =val)
)
(p next-step-main-match-only
  =goal>
    isa recipe
    state retrieve-next
    main =rec ; remove to allow more crazy stuff using partial matching
    - info =val ; no perseveration, here
    - info =prev

  =retrieval>
    isa recipe
    main =rec
    prev =prev
    info =val
    action =a
    order =o
    - info end-of-trial
==>

  ; keep old goal in dm
  =goal>
    state nil
    worldmark nil
  +goal>
    isa recipe
    main =rec
    prev =prev
    info =val
    action =a
    order =o
    state start

   !output! (next goal =val)
#|
  =goal>
    state check-world-start
    worldmark nil
|#
)
(p next-step-all-match
  =goal>
    isa recipe
    state retrieve-next
    main =rec ; remove to allow more crazy stuff using partial matching
    info =step ; remove to allow more crazy stuff using partial matching

  =retrieval>
    isa recipe
    main =rec
    prev =step
    info =val
    action =a
    order =o
    - info end-of-trial
==>
  ; keep old goal in dm
  =goal>
    state nil
    worldmark nil
  +goal>
    isa recipe
    main =rec
    prev =step
    info =val
    action =a
    order =o
    state start

   !output! (next goal =val)
)
(p next-step-main-mismatch
  =goal>
    isa recipe
    state retrieve-next
    main =rec
    info =step

  =retrieval>
    isa recipe
    - main =rec
==>
#|
  +vocal>
    isa speak
    string "OOPS8"

  =goal> init
  -goal>
  =imaginal> init
  -imaginal>
|#
  =goal>
    state check-world-start
    worldmark nil

  !eval! (EvalToVoid (format nil "(START-WORLD-STRATEGY ~W)" (mp-time)))
)
(p next-step-main-mismatch-first
  =goal>
    isa recipe
    state retrieve-next
    main =rec
    info nil

  =retrieval>
    isa recipe
    - main =rec
  ?vocal>
    state free
==>
  +vocal>
    isa speak
    string "REPEAT"

  =goal> init ; overwrite to avoid strengthening
  -goal>
  =imaginal> init
  -imaginal>

  !output! (could not remember correctly =rec)
)

(p next-step-is-first
  =goal>
    isa recipe
    state retrieve-next

  =retrieval>
    isa recipe
    main =rec
    prev nil
    info =val
    action =a
    order =o
==>
  =goal>
    ;isa recipe
    main =rec
    info =val
    action =a
    order =o
    state start

  !output! (next goal =val)
  ;!eval! (dm)
)

(p next-step-is-finish-all
  =goal>
    isa recipe
    state retrieve-next
    main =rec ; remove to allow more crazy stuff using partial matching
    info =step ; remove to allow more crazy stuff using partial matching

  =retrieval>
    isa recipe
    main =rec
    prev =step
    info end-of-trial
    action ask-experimenter
  =imaginal>
    isa meal
    s1 nil
    s2 nil
    s3 nil
    s4 nil
  ?vocal>
    state free
==>
  +vocal>
    isa speak
    string "DONE"

  ; keep these in to allow intrusions
  ;=goal> init ; overwrite to avoid strengthening
  =goal> ; merge with original one
    state nil
    worldmark nil
  -goal>
  =imaginal> init
  -imaginal>

  !output! (done with =rec)
)
(p next-step-is-finish-main
  =goal>
    isa recipe
    state retrieve-next
    main =rec ; remove to allow more crazy stuff using partial matching
    ;info =step ; removed to allow more crazy stuff using partial matching

  =retrieval>
    isa recipe
    main =rec
    ;prev =step
    info end-of-trial
    action ask-experimenter
  =imaginal>
    isa meal
    s1 nil
    s2 nil
    s3 nil
    s4 nil
  ?vocal>
    state free
==>
  +vocal>
    isa speak
    string "DONE"

  ; keep these in to allow intrusions
  ;=goal> init ; overwrite to avoid strengthening
  =goal> ; merge with original one
    state nil
    worldmark nil
  -goal>
  =imaginal> init
  -imaginal>

  !output! (done with =rec)
)

(p next-step-is-finish-imag1
  =goal>
    isa recipe
    state retrieve-next
  =retrieval>
    isa recipe
    info end-of-trial
    action ask-experimenter
  =imaginal>
    isa meal
    - s1 nil
==>
  =imaginal>
  =goal>
    state check-world-start
    worldmark nil

  !eval! (EvalToVoid (format nil "(START-WORLD-STRATEGY ~W)" (mp-time)))
)
(p next-step-is-finish-imag2
  =goal>
    isa recipe
    state retrieve-next
  =retrieval>
    isa recipe
    info end-of-trial
    action ask-experimenter
  =imaginal>
    isa meal
    - s2 nil
==>
  =imaginal>
  =goal>
    state check-world-start
    worldmark nil
  !eval! (EvalToVoid (format nil "(START-WORLD-STRATEGY ~W)" (mp-time)))
)
(p next-step-is-finish-imag3
  =goal>
    isa recipe
    state retrieve-next
  =retrieval>
    isa recipe
    info end-of-trial
    action ask-experimenter
  =imaginal>
    isa meal
    - s3 nil
==>
  =imaginal>
  =goal>
    state check-world-start
    worldmark nil
  !eval! (EvalToVoid (format nil "(START-WORLD-STRATEGY ~W)" (mp-time)))
)
(p next-step-is-finish-imag4
  =goal>
    isa recipe
    state retrieve-next
  =retrieval>
    isa recipe
    info end-of-trial
    action ask-experimenter
  =imaginal>
    isa meal
    - s4 nil
==>
  =imaginal>
  =goal>
    state check-world-start
    worldmark nil
  !eval! (EvalToVoid (format nil "(START-WORLD-STRATEGY ~W)" (mp-time)))
)


(p begin-trial
  =goal>
    isa recipe
    state init
  ?vocal>
    state free
==>
  +vocal>
    isa speak
    string "READY"

  =goal> init ; overwrite to avoid strengthening
  -goal>
)

(goal-focus goal)

(start-hand-at-mouse)

;(setf *actr-enabled-p* t)
)

