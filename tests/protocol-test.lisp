(in-package #:a2a-protocol/tests)

(deftest classes-exist
  (ok (find-class 'a2a-protocol:agent-card))
  (ok (find-class 'a2a-protocol:a2a-task))
  (ok (find-class 'a2a-protocol:a2a-backend)))
