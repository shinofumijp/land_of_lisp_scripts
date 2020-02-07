(load 'graph-util)

(defparameter *wizard-nodes* '((living-room (you are in the living-room.
                                                 a wizard is snoring loudly on the corch.))
                               (garden (you are in a beautiful garden.
                                            there is a well in front of you.))
                               (attic (you are in the attic.
                                           there is a giant welding torch in the corner.))))

(defparameter *wizard-edges* '((living-room (garden west door)
                                            (attic upstairs ladder))
                               (garden (living-room east door))
                               (attic (living-room downstairs ladder))))
(progn
   (princ "input filename >")
   (let ((fname (concatenate 'string "out/" (read-line))))
     (graph->png fname *wizard-nodes* *wizard-edges*)
     (ugraph->png (concatenate 'string fname "-ugraph") *wizard-nodes* *wizard-edges*)))
