(use-modules (jroller parse))
(use-modules (jroller eval))


(dice-eval
 ((parse) "1d6"))
(dice-eval
  ((parse) "1d6 * 4"))
(dice-eval
  ((parse) "1d6 + 2 * 3"))
