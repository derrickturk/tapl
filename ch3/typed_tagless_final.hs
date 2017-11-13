-- the (previously) untyped expression language of TAPL chapter 3,
--   using a typed tagless final representation
--   a la http://okmij.org/ftp/tagless-final/course/lecture.pdf

-- look ma, no GADTs or type families

import Prelude hiding (succ)

t = True
f = False

ifThenElse True t _ = t
ifThenElse False _ f = f

z = 0

succ n = 1 + n

pred 0 = 0
pred n = n - 1

isZero = (== 0)
