; -*- coding: iso-8859-1; mode: Lisp; indent-tabs-mode: nil; -*-

; ===================================================================

; Halbruegge, M., and Engelbrecht, K.-P. (2014). An activation-based
; model of execution delays of specific task steps. Cognitive 
; Processing, 15, pages S107-S110

; -------------------------------------------------------------------

; Abstract: When humans use devices like ticket vending machines,
; their actions can be categorized into task-oriented (e.g. selecting
; a ticket) and device-oriented (e.g. removing the bank card after
; having paid). Device-oriented steps contribute only indirectly to
; the user's goal; they take longer than their task-oriented
; counterparts and are more likely to be forgotten. A promising
; explanation is provided by the activation-based memory for goals
; model (Altmann and Trafton 2002). The objectives of this paper are,
; first, to replicate the step prolongation effect of device-
; orientation in a kitchen assistance context, and secondly, to
; investigate whether the activation construct can explain this
; effect using cognitive modeling. Finally, a necessity and
; sensitivity analysis provides more insights into the relationship
; between goal activation and device-orientation effects.

; ===================================================================

; HOW TO RUN

; The model depends on the "lean embedded lisp" sub-project of ACT-CV
; http://act-cv.sourceforge.net/

; If you want to use it without ACT-CV, you need to fill the visicon 
; and audicon and handle clicks and shouts by yourself.

; ===================================================================

; Copyright (c) 2014 Marc Halbruegge (marc.halbruegge@tu-berlin.de)

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
; :nsji allow/warn negative values in the fan calculation?
; :act activation trace
(sgp :v nil :trace-detail medium :show-focus nil 
	:iu 0 :egs .5 :ul t 
	:epl t
	:imaginal-activation 1.0
	:aural-activation 1.0
	:mp nil
	:mas 2
	:esc t 
	:bll .1 :blc 0.5
	:er t :ans nil :nsji nil :act nil)


; allow parametrization before declarative mem is being filled
; COMMENT OUT for running outside of ACT-CV
(eval (read-from-string (EvalToString "(PARENT-SGP)")))
	
(chunk-type recipe main prev info action state)
(chunk-type meal s1 s2 s3 s4) ; put some info about the task into the imaginal buffer

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
	

    (GO isa chunk)
	(goal isa recipe state init)

	(push isa chunk)(on isa chunk)(off isa chunk)
	) 
(sdp :reference-count 5)
; (sdp (LAMMKOTELETTS DEUTSCH HAUPTGERICHT) :reference-count 10)

	
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

; give up - todo: re-retrieve and re-start
(p find-object-failure
  =goal>
    isa recipe
    state attending
  ?visual-location>
    state error
==>
  +vocal>
    isa speak
    string "OOPS"

  -goal>
  -imaginal>
)

; give up - todo: re-retrieve and re-start
(p encode-object-failure
  =goal>
    isa recipe
    state encoding
  ?visual>
    state error
==>
  +vocal>
    isa speak
    string "OOPS"

  -goal>
  -imaginal>
)

; give up - todo: re-retrieve and re-start
(p encode-search-failure
  =goal>
    isa recipe
    state encoding
	info =val
  =visual>
    isa text
    - value =val
==>
  +vocal>
    isa speak
    string "OOPS"

  -goal>
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
	  main =rec
	  info =val
   =visual>
      isa text
      value =val
	  - color black-white

==>
   +retrieval>
      isa recipe
	  main =rec
	  prev =val
	  :recently-retrieved nil

   =goal>
	  state retrieve-next
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
	  main =rec
	  info =val
   =visual>
      isa text
      value =val
	  color black-white

==>
   +retrieval>
      isa recipe
	  main =rec
	  prev =val
	  :recently-retrieved nil

   =goal>
	  state retrieve-next
)

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

; can be triggered too early -> retrieval errors
(p after-click-mouse-last
   =goal>
      isa recipe
      state retrieve-next

   ?retrieval>
      state error

==>
  +vocal>
    isa speak
    string "DONE"

  -goal>
  -imaginal>
)

; rely on buffer stuffing
(p attend-any-aural
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
	
  -aural>
  -aural-location>
  
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

  =imaginal>
    isa recipe
	- main nil
	info nil
	
==>
  =imaginal>
    info =info

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
); next step
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
==>
  +imaginal>
    isa recipe
	main =trialname
	prev =prev
    info =info

  -aural>
  -aural-location>
)
; stop listening, start acting
(p hear-new-task-5
  =goal>
    isa meal

  =aural-location>
    isa audio-event
    - location self

  =aural>
    isa sound
    kind word

  =imaginal>
    isa recipe
	main =trialname
	- action nil
==>
  +imaginal> =goal

  +retrieval>
    isa recipe
    prev nil
    main =trialname

  +goal>
    isa recipe
    state retrieve-next

  -aural>
  -aural-location>
)
; TODO put a "meal" chunk into the imaginal buffer for spreading activation

(p next-step
    =goal>
      isa recipe
      state retrieve-next
	  ;main =rec ; removed to allow more crazy stuff using partial matching
	  ;info =step ; removed to allow more crazy stuff using partial matching

    =retrieval>
      isa recipe
	  main =rec
	  prev =prev
	  info =val
	  action =a
==>
    +goal>
      isa recipe
	  main =rec
	  prev =prev
	  info =val
	  action =a
	  state start
	  
   !output! (next goal =val)	  
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
==>
    +goal>
      isa recipe
	  main =rec
	  info =val
	  action =a
	  state start
	  
   !output! (next goal =val)	  
)

(p begin-trial
  =goal>
	isa recipe
	state init
==>
  +vocal>
    isa speak
    string "READY"

  -goal>
)

(goal-focus goal)

(start-hand-at-mouse)

;(setf *actr-enabled-p* t)
)

