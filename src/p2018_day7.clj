(ns p2018_day7)

; key를 일(work)로 하고 value를 set of 먼저 선행되어야하는 일(pre-work)이라고 정의
; (parsing) 예시 인풋의 경우 parsing해서 -> 표현한다 {A #{C} B #{A} C #{} D #{A} E #{B D F} F #{C}}
; (processing) val가 비어있는 경우에 대해서 work를 실행한다. work 가 실행되면 모든 key-val 쌍을 순회하면서 val에 있는 pre-work를 갱신한다(삭제한다)
; 이 과정을 계속 반복한다.